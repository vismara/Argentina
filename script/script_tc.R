## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_tc.R
##
## Purpose of script: Modeling temperature of bed of compost barn with glm multilevel Bayesian models 
## To run this script use the following command: Rscript script_tc.R 
##
## Author: Edgar de Souza Vismara
##         Frederico Marcio Vieira
##
## Date Created: 2023-10-04

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}


##------------------------------------------------------------------------------
## Modelling 
##------------------------------------------------------------------------------


#Defining the prior
prior<-c(set_prior("normal(25, 10)", class = "Intercept"),
         set_prior("normal(0, 5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sigma")
)
##------------------------------------------------------------------------------
## Modelling TS
##------------------------------------------------------------------------------


#Fit model
mod_ts <- brm(data = df_tc, family = gaussian(),
                TS ~   MOMENTO*Day*Locality,
                iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
                prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
                refresh = 0, seed = 123)


summary(mod_ts)

# Plot analisys

#-------------------------------------------------------------

#Simulate data for predictive posterior plot analisys

new_data <- expand_grid(Locality = c("Rafaela","Salta"),
                        Day = factor(seq(1:12)),
                        MOMENTO = c("Mañana", "Tarde"))
                         
#Computing posterior prediction of simulation draws (used for inference)

tidy_epred_ts<- mod_ts %>% 
  epred_draws(newdata = new_data, re_formula = NULL)


#==========================================================================
#Comparing moments across days on both localities (actualliy only Rafaela)
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_ts[tidy_epred_ts$Locality=="Rafaela",]%>% 
  group_by(Day, MOMENTO) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_m_dia_local <- 
  ggplot(tidy_epred_ts[tidy_epred_ts$Locality=="Rafaela",], 
         aes(x = .epred, y = Day,
             fill = MOMENTO)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Moment") +
  theme_clean() +
  theme(legend.position = "bottom")

# Difference between moments across days on Rafaela

contraste_m_dia_local <- mod_ts %>% 
  emmeans(~ MOMENTO*Day*Locality,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("Day","Locality")) %>% 
  gather_emmeans_draws()

ctr_rafa = contraste_m_dia_local[contraste_m_dia_local$Locality=="Rafaela",]

#Point prediction of contrast with 0,95 interval
ctr_rafa %>% 
  group_by(Day) %>% median_hdi(.value)

#Proportions
table(ctr_rafa$.value>0, ctr_rafa$Day)/4000

#Plot---------------------------------------------------------

plot_cont_m_dia_local <- ggplot(ctr_rafa, aes(x = .value, y = Day, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Morning - Afternoon)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") 


# Combined plot
g_mdl_ts = (plot_m_dia_local| plot_cont_m_dia_local + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive 'TS' between moments across days on Rafaela",
                  theme = theme_clean()) 
ggsave(g_mdl_ts, file = "plots/TS_btw_mom.pdf", width = 8, height = 6.5)
#g_mdl_ts (for debug)

#==========================================================================
#Comparing localities across days  in the morning
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_ts[tidy_epred_ts$MOMENTO=="Mañana",] %>% 
  group_by(Day, Locality) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_m <- 
  ggplot(tidy_epred_ts[tidy_epred_ts$MOMENTO=="Mañana",], 
         aes(x = .epred, y = Day,
             fill = Locality)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive 'TS'", y = "Days",
       fill = "Locality") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")

#Difference between localities across days in the morning

contraste_local_dia_m <- mod_ts %>% 
  emmeans(~ MOMENTO*Day*Locality,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("Day","MOMENTO")) %>% 
  gather_emmeans_draws()

ctr_man = contraste_local_dia_m[contraste_local_dia_m$MOMENTO=="Mañana",]

#Point prediction of contrast with 0,95 interval
ctr_man %>% 
  group_by(Day) %>% median_hdi(.value)

#Proportions
table(ctr_man$.value>0,ctr_man$Day)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_m <- ggplot(ctr_man, aes(x = .value, y = Day, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Combined plot
g_ldm_ts = (plot_local_dia_m| plot_cont_local_dia_m + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive 'TS' between  localities across days in the morning",
                  theme = theme_clean()) 
ggsave(g_ldm_ts, file = "plots/TS_btw_loc.pdf", width = 8, height = 6.5)
#g_ldm_ts (for debug)

#------------------------------------------------------------------------------
## Modelling TI
##------------------------------------------------------------------------------


#Fit model
mod_ti <- brm(data = df_tc, family = gaussian(),
              TI ~   MOMENTO*Day*Locality,
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_ti)

# Plot analisys

#-------------------------------------------------------------

#Computing posterior prediction of simulation draws (used for inference)

tidy_epred_ti<- mod_ti %>% 
  epred_draws(newdata = new_data, re_formula = NULL)


#==========================================================================
#Comparing moments across days on both localities (actualliy only Rafaela)
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_ti[tidy_epred_ti$Locality=="Rafaela",]%>% 
  group_by(Day, MOMENTO) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_m_dia_local_ti <- 
  ggplot(tidy_epred_ti[tidy_epred_ti$Locality=="Rafaela",], 
         aes(x = .epred, y = Day,
             fill = MOMENTO)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Moment") +
  scale_fill_discrete(labels=c('Morning', 'Afternoon')) +
  theme_clean() +
  theme(legend.position = "bottom")

# Difference between moments across days on Rafaela

contraste_m_dia_local_ti <- mod_ti %>% 
  emmeans(~ MOMENTO*Day*Locality,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("Day","Locality")) %>% 
  gather_emmeans_draws()

ctr_rafa_ti = contraste_m_dia_local_ti[contraste_m_dia_local_ti$Locality=="Rafaela",]

#Point prediction of contrast with 0,95 interval
ctr_rafa_ti %>% 
  group_by(Day) %>% median_hdi(.value)

#Proportions
table(ctr_rafa_ti$.value>0, ctr_rafa_ti$Day)/4000

#Plot---------------------------------------------------------

plot_cont_m_dia_local_ti <- ggplot(ctr_rafa_ti, aes(x = .value, y = Day, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Morning - Afternoon)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") 


# Combined plot
g_mdl_ti = (plot_m_dia_local_ti| plot_cont_m_dia_local_ti + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive 'TI' between moments across days on Rafaela",
                  theme = theme_clean()) 
ggsave(g_mdl_ti, file = "plots/TI_btw_mom.pdf", width = 8, height = 6.5)
#g_mdl_ti (for debug)

#==========================================================================
#Comparing localities across days  in the morning
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_ti[tidy_epred_ti$MOMENTO=="Mañana",] %>% 
  group_by(Day, Locality) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_m_ti <- 
  ggplot(tidy_epred_ti[tidy_epred_ti$MOMENTO=="Mañana",], 
         aes(x = .epred, y = Day,
             fill = Locality)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive 'TI'", y = "Days",
       fill = "Locality") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")

#Difference between localities across days in the morning

contraste_local_dia_m_ti <- mod_ti %>% 
  emmeans(~ MOMENTO*Day*Locality,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("Day","MOMENTO")) %>% 
  gather_emmeans_draws()

ctr_man_ti = contraste_local_dia_m_ti[contraste_local_dia_m_ti$MOMENTO=="Mañana",]

#Point prediction of contrast with 0,95 interval
ctr_man_ti %>% 
  group_by(Day) %>% median_hdi(.value)

#Proportions
table(ctr_man_ti$.value>0,ctr_man_ti$Day)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_m_ti <- ggplot(ctr_man_ti, aes(x = .value, y = Day, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") 


# Combined plot
g_ldm_ti = (plot_local_dia_m_ti| plot_cont_local_dia_m_ti + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive 'TI' between  localities across days in the morning",
                  theme = theme_clean()) 
ggsave(g_ldm_ti, file = "plots/TI_btw_loc.pdf", width = 8, height = 6.5)
#g_ldm_ti (for debug)

#=======================================================================================================
#END
#=======================================================================================================

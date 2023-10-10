## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_pl.R
##
## Purpose of script: Modeling milk poduction with glm Bayesian models 
## To run this script use the following command: Rscript script_pl.R 
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
prior<-c(set_prior("normal(35, 10)", class = "Intercept"),
         set_prior("normal(0, 5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR"),
         set_prior("cauchy(0, 25)", class="sigma")
)
##------------------------------------------------------------------------------
## Modelling 
##------------------------------------------------------------------------------


#Fit model
mod_pl <- brm(data = df_pl, family = gaussian(),
              PL_m ~   CATEGORIA*WEEK*LUGAR + (1|ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


#summary(mod_pl)

# Plot analisys

#-------------------------------------------------------------

#Simulate data for predictive posterior plot analisys

rafa_vaca <- expand_grid(WEEK = factor(seq(1:15)),
                         CATEGORIA = c("Vaca"),
                         ID = unique(df_pl[df_pl$LUGAR == "Rafaela" & 
                                             df_pl$CATEGORIA == "Vaca",]$ID),
                         LUGAR = "Rafaela")

rafa_vaq <- expand_grid(WEEK = factor(seq(1:15)),
                        CATEGORIA = c("Vaq"),
                        ID = unique(df_pl[df_pl$LUGAR == "Rafaela" & 
                                            df_pl$CATEGORIA == "Vaq",]$ID),
                        LUGAR = "Rafaela")

salta_vaca <- expand_grid(WEEK = factor(seq(1:15)),
                          CATEGORIA = c("Vaca"),
                          ID = unique(df_pl[df_pl$LUGAR == "Salta" & 
                                              df_pl$CATEGORIA == "Vaca",]$ID),
                          LUGAR = "Salta")

salta_vaq <- expand_grid(WEEK = factor(seq(1:15)),
                         CATEGORIA = c("Vaq"),
                         ID = unique(df_pl[df_pl$LUGAR == "Salta" & 
                                             df_pl$CATEGORIA == "Vaq",]$ID),
                         LUGAR = "Salta")

new_data = rbind(rafa_vaca,rafa_vaq, salta_vaca, salta_vaq)

#Computing posterior prediction of simulation draws (used for inference)

tidy_epred_pl<- mod_pl %>% 
  epred_draws(newdata = new_data, re_formula = NULL)


#==========================================================================
#Comparing categories across Number of milking on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_pl %>% 
  group_by(LUGAR, WEEK, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_c_week_local <- 
  ggplot(tidy_epred_pl, 
         aes(x = .epred, y = WEEK,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior Milk production", y = "days x 2",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  # xlim(0,30)+
  facet_wrap(vars(LUGAR))

# Difference between moments across days on both places

contraste_c_week_local <- mod_pl %>% 
  emmeans(~ CATEGORIA*WEEK*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("WEEK","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_c_week_local %>% 
  group_by(WEEK, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_c_week_local$.value>0, contraste_c_week_local$WEEK, contraste_c_week_local$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_c_week_local <- ggplot(contraste_c_week_local, aes(x = .value, y = WEEK, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cwl = (plot_c_week_local| plot_cont_c_week_local + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive milk production between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cwl, file = "plots/PL_btw_cat.pdf", width = 8, height = 6.5)
#g_cwl (for debug)

#==========================================================================
#Comparing localities across days  on both categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_pl %>% 
  group_by(WEEK, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_week_c <- 
  ggplot(tidy_epred_pl, 
         aes(x = .epred, y = WEEK,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive milk production", y = "Days x2",
       fill = "Locality") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  #xlim(0,80)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_week_c <- mod_pl %>% 
  emmeans(~ CATEGORIA*WEEK*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("WEEK","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_week_c %>% 
  group_by(WEEK, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_week_c$.value>0,contraste_local_week_c$WEEK, contraste_local_week_c$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_week_c <- ggplot(contraste_local_week_c, aes(x = .value, y = WEEK, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_lwc = (plot_local_week_c| plot_cont_local_week_c + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive milk production between \n localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_lwc, file = "plots/PL_btw_loc.pdf", width = 8, height = 6.5)
#g_lwc (for debug)

#==========================================================================================================================
#END
#==========================================================================================================================
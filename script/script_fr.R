## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_fr.R
##
## Purpose of script: Modeling animal Heart rate with glm multilevel Bayesian models 
## To run this script use the following command: Rscript script_fr.R 
##
## Author: Edgar de Souza Vismara
##         Frederico Marcio Vieira
##
## Date Created: 2023-09-10

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}


##-------------------------------------------------------------------------------------------
## Modelling FR
##-------------------------------------------------------------------------------------------


#Defining the prior
prior<-c(set_prior("normal(3.5, 0.5)", class = "Intercept"),
         set_prior("normal(0, 0.5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR")
)

#Fit model
mod_fr <- brm(data = df_fr, family = poisson(link = "log"),
              FR ~   CATEGORIA*DIA*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_fr)

# Plot analisys

#-------------------------------------------------------------

#Simulate data for predictive posterior plot analisys

rafa_vaca <- expand_grid(DIA = factor(seq(1:12)),
                         CATEGORIA = c("Vaca"),
                         ID = unique(df_fr[df_fr$LUGAR == "Rafaela" & 
                                             df_fr$CATEGORIA == "Vaca",]$ID),
                         LUGAR = "Rafaela")

rafa_vaq <- expand_grid(DIA = factor(seq(1:12)),
                        CATEGORIA = c("Vaq"),
                        ID = unique(df_fr[df_fr$LUGAR == "Rafaela" & 
                                            df_fr$CATEGORIA == "Vaq",]$ID),
                        LUGAR = "Rafaela")

salta_vaca <- expand_grid(DIA = factor(seq(1:12)),
                          CATEGORIA = c("Vaca"),
                          ID = unique(df_fr[df_fr$LUGAR == "Salta" & 
                                              df_fr$CATEGORIA == "Vaca",]$ID),
                          LUGAR = "Salta")

salta_vaq <- expand_grid(DIA = factor(seq(1:12)),
                         CATEGORIA = c("Vaq"),
                         ID = unique(df_fr[df_fr$LUGAR == "Salta" & 
                                             df_fr$CATEGORIA == "Vaq",]$ID),
                         LUGAR = "Salta")

newdata = rbind(rafa_vaca,rafa_vaq, salta_vaca, salta_vaq)


#Computing posterior prediction of simulation draws (used for inference)

tidy_epred_fr<- mod_fr %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days and moments on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_fr %>% 
  group_by(DIA, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local <- 
  ggplot(tidy_epred_fr, 
         aes(x = .epred, y = DIA,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,70)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local <- mod_fr %>% 
  emmeans(~ CATEGORIA*DIA*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local %>% 
  group_by(DIA, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local$.value>0, contraste_cat_dia_local$DIA, contraste_cat_dia_local$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local <- ggplot(contraste_cat_dia_local, aes(x = .value, y = DIA, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_fr = (plot_cat_dia_local| plot_cont_cat_dia_local + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive respiratory frequency between category across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_fr, file = "plots/FR_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_oe (for debug)


#==========================================================================
#Comparing localities across days and moments on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_fr %>% 
  group_by(DIA, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat <- 
  ggplot(tidy_epred_fr, 
         aes(x = .epred, y = DIA,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,80)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat <- mod_fr %>% 
  emmeans(~ CATEGORIA*DIA*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat %>% 
  group_by(DIA, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat$.value>0,contraste_local_dia_cat$DIA, contraste_local_dia_cat$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat <- ggplot(contraste_local_dia_cat, aes(x = .value, y = DIA, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_fr = (plot_local_dia_cat| plot_cont_local_dia_cat + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive respiratory frequency between locality across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_fr, file = "plots/FR_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_fr (for debug)

#=======================================================================================================
#END
#=======================================================================================================

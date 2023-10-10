## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_cp.R
##
## Purpose of script: Modeling animal behavior with glm multilevel Bayesian models 
## To run this script use the following command: Rscript script_cp.R 
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

##-------------------------------------------------------------------------------------------
## Modelling OCIOP
##-------------------------------------------------------------------------------------------


#Defining the prior
prior<-c(set_prior("normal(1, 0.3)", class = "Intercept"),
         set_prior("normal(0, 0.5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR")
)

#Fit model
mod_OP <- brm(data = df_cp, family = poisson(link = "log"),
               OCIOP_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
               iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
               prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
               refresh = 0, seed = 123)


summary(mod_OP)

# Plot analisys

#-------------------------------------------------------------

#Simulate data for predictive posterior plot analisys

rafa_vaca <- expand_grid(DIA.ENSAYO = factor(seq(1:12)),
                       CATEGORIA = c("Vaca"),
                       ID = unique(df_cp[df_cp$LUGAR == "Rafaela" & 
                                           df_cp$CATEGORIA == "Vaca",]$ID),
                       LUGAR = "Rafaela")

rafa_vaq <- expand_grid(DIA.ENSAYO = factor(seq(1:12)),
                         CATEGORIA = c("Vaq"),
                         ID = unique(df_cp[df_cp$LUGAR == "Rafaela" & 
                                               df_cp$CATEGORIA == "Vaq",]$ID),
                         LUGAR = "Rafaela")
salta_vaca <- expand_grid(DIA.ENSAYO = factor(seq(1:12)),
                         CATEGORIA = c("Vaca"),
                         ID = unique(df_cp[df_cp$LUGAR == "Salta" & 
                                               df_cp$CATEGORIA == "Vaca",]$ID),
                         LUGAR = "Salta")

salta_vaq <- expand_grid(DIA.ENSAYO = factor(seq(1:12)),
                        CATEGORIA = c("Vaq"),
                        ID = unique(df_cp[df_cp$LUGAR == "Salta" & 
                                              df_cp$CATEGORIA == "Vaq",]$ID),
                        LUGAR = "Salta")

newdata = rbind(rafa_vaca,rafa_vaq, salta_vaca, salta_vaq)


#Computing posterior prediction of simulation draws (used for inference)

tidy_epred_OP <- mod_OP %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#=================================================================================
#Marginal prediction by "category"
#=================================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(CATEGORIA) %>% 
  median_hdi(.epred)

#Plot per group---------------------------------------------------------

plot_cat_OP <- ggplot(tidy_epred_OP, aes(x = .epred, fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(title = NULL, y = "Density", fill = "Category", x = "Posterior predictive frequency") +
  theme_clean() +
  theme(legend.position = "top") +
  xlim(0,7)

# Computing contrasts (Vaq-Vaca)

contraste_cat_OP <- mod_OP %>% 
  emmeans(~ CATEGORIA,
          epred = TRUE) %>% 
  contrast(method = "revpairwise") %>% 
  gather_emmeans_draws()


#Point prediction of contrast with 0,95 interval
contraste_cat_OP %>% median_hdi()

#Proportions
table(contraste_cat_OP$.value>0)/4000

#Plot---------------------------------------------------------

plot_cont_cat_OP <- ggplot(contraste_cat_OP, aes(x = .value, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Combined plot
(plot_cat_OP| plot_cont_cat_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Predictive posterior distributions of 'OCIOP' by 'CATEGORIA'" ,
                  theme = theme_clean()) 

#==========================================================================
#Marginal prediction by "category" across days
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(CATEGORIA, DIA.ENSAYO) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_OP <- 
  ggplot(tidy_epred_OP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days of experiment",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,7)


# difference between categories across days
contraste_cat_dia_OP <- mod_OP %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = "DIA.ENSAYO") %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_OP %>% 
  group_by(DIA.ENSAYO) %>% median_hdi(.value)

#proportions
table(contraste_cat_dia_OP$.value>0,contraste_cat_dia_OP$DIA.ENSAYO)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_OP <- ggplot(contraste_cat_dia_OP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Combined plot
(plot_cat_dia_OP| plot_cont_cat_dia_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Predictive posterior distributions of 'OCIOP' by 'CATEGORIA' across the experiment days",
                  theme = theme_clean()) 


#==========================================================================
#Comparing categories on both places 
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(CATEGORIA, LUGAR) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------
plot_cat_local_OP <- 
  ggplot(tidy_epred_OP, 
         aes(x = .epred, y = LUGAR,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = NULL,
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,8)

# Difference between categories on both places

contraste_cat_local_OP <- mod_OP %>% 
  emmeans(~ CATEGORIA*LUGAR,
          epred = TRUE, re_formula = ~(1|ID)) %>% 
  contrast(method = "revpairwise", by = "LUGAR") %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_local_OP %>% 
  group_by(LUGAR) %>% median_hdi(.value)

#proportions
table(contraste_cat_local_OP$.value>0,contraste_cat_local_OP$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_local_OP <- ggplot(contraste_cat_local_OP, aes(x = .value, y = LUGAR, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Plot combinado
(plot_cat_local_OP| plot_cont_cat_local_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive frequency of 'OCIOP' between 'CATEGORIA' on both localities",
                  theme = theme_clean()) 



#==========================================================================
#Comparing Localities across days
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_dia_local_OP <- 
  ggplot(tidy_epred_OP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predicitive posterior frequency", y = "Days",
       fill = "Locality") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,8)


# Difference between localities across days

contraste_dia_local_OP <- mod_OP %>% 
  emmeans(~ DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = "DIA.ENSAYO") %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_dia_local_OP %>% 
  group_by(DIA.ENSAYO) %>% median_hdi(.value)

#proportions
table(contraste_dia_local_OP$.value>0,contraste_dia_local_OP$DIA.ENSAYO)/4000

#Plot---------------------------------------------------------

plot_cont_dia_local_OP <- ggplot(contraste_dia_local_OP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed")


# Combined plot
(plot_dia_local_OP| plot_cont_dia_local_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(title = "Posterior predictive frequency of 'OCIOP' between 'LUGAR' across days",
                  theme = theme_clean()) 



#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_OP <- 
  ggplot(tidy_epred_OP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_OP <- mod_OP %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_OP %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
p_cat_dia_local_OP = table(contraste_cat_dia_local_OP$.value>0,contraste_cat_dia_local_OP$DIA.ENSAYO, contraste_cat_dia_local_OP$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_OP <- ggplot(contraste_cat_dia_local_OP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_op = (plot_cat_dia_local_OP| plot_cont_cat_dia_local_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OCIOP' between \n categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_op, file = "plots/OCIOP_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_op (for debug)

#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OP %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_OP <- 
  ggplot(tidy_epred_OP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_OP <- mod_OP %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_OP %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_OP$.value>0,contraste_local_dia_cat_OP$DIA.ENSAYO, contraste_local_dia_cat_OP$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_OP <- ggplot(contraste_local_dia_cat_OP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_op = (plot_local_dia_cat_OP| plot_cont_local_dia_cat_OP + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OCIOP' between \n localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_op, file = "plots/OCIOP_btw_loc.pdf", width = 8, height = 6.5)
#g_cdl_op (for debug)


#As we considered a three-way interaction between the factors, 
#we should only focus on the last two comparisons (categories and locations over days).
#In addition, the contrast between days was not computed as it is of secondary interest. 

#Therefore, for all the other behaviours measured, only the last two graphs will be 
#generated and saved in the plots folder.

##-------------------------------------------------------------------------------------------
## Modelling OCIOE
##-------------------------------------------------------------------------------------------

#Fit model
mod_OE <- brm(data = df_cp, family = poisson(link = "log"),
              OCIOE_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_OE)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_OE <- mod_OE %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OE %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_OE <- 
  ggplot(tidy_epred_OE, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_OE <- mod_OE %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_OE %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_OE$.value>0,contraste_cat_dia_local_OE$DIA.ENSAYO, contraste_cat_dia_local_OE$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_OE <- ggplot(contraste_cat_dia_local_OE, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_oe = (plot_cat_dia_local_OE| plot_cont_cat_dia_local_OE + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OCIOE' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_oe, file = "plots/OCIOE_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_oe (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OE %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_OE <- 
  ggplot(tidy_epred_OE, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_OE <- mod_OE %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_OE %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_OE$.value>0,contraste_local_dia_cat_OE$DIA.ENSAYO, contraste_local_dia_cat_OE$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_OE <- ggplot(contraste_local_dia_cat_OE, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_oe = (plot_local_dia_cat_OE| plot_cont_local_dia_cat_OE + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OCIOE' between localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_oe, file = "plots/OCIOE_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_oe (for debug)



##-------------------------------------------------------------------------------------------
## Modelling RUMIP
##-------------------------------------------------------------------------------------------

#Fit model
mod_RP <- brm(data = df_cp, family = poisson(link = "log"),
              RUMIP_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_RP)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_RP <- mod_RP %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_RP %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_RP <- 
  ggplot(tidy_epred_RP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_RP <- mod_RP %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_RP %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_RP$.value>0,contraste_cat_dia_local_RP$DIA.ENSAYO, contraste_cat_dia_local_RP$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_RP <- ggplot(contraste_cat_dia_local_RP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_rp = (plot_cat_dia_local_RP| plot_cont_cat_dia_local_RP + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'RUMIP' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_rp, file = "plots/RUMIP_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_rp (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_RP %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_RP <- 
  ggplot(tidy_epred_RP, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_RP <- mod_RP %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_RP %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_RP$.value>0,contraste_local_dia_cat_RP$DIA.ENSAYO, contraste_local_dia_cat_RP$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_RP <- ggplot(contraste_local_dia_cat_RP, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_rp = (plot_local_dia_cat_RP| plot_cont_local_dia_cat_RP + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'RUMIP' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_rp, file = "plots/RUMIP_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_rp (for debug)


##-------------------------------------------------------------------------------------------
## Modelling RUMIE
##-------------------------------------------------------------------------------------------

#Fit model
mod_RE <- brm(data = df_cp, family = poisson(link = "log"),
              RUMIE_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_RE)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_RE <- mod_RE %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_RE %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_RE <- 
  ggplot(tidy_epred_RE, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_RE <- mod_RE %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_RE %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_RE$.value>0,contraste_cat_dia_local_RE$DIA.ENSAYO, contraste_cat_dia_local_RE$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_RE <- ggplot(contraste_cat_dia_local_RE, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_re = (plot_cat_dia_local_RE| plot_cont_cat_dia_local_RE + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'RUMIE' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_re, file = "plots/RUMIE_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_re (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_RE %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_RE <- 
  ggplot(tidy_epred_RE, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_RE <- mod_RE %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_RE %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_RE$.value>0,contraste_local_dia_cat_RE$DIA.ENSAYO, contraste_local_dia_cat_RE$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_RE <- ggplot(contraste_local_dia_cat_RE, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_re = (plot_local_dia_cat_RE| plot_cont_local_dia_cat_RE + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'RUMIP' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_re, file = "plots/RUMIE_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_re (for debug)


##-------------------------------------------------------------------------------------------
## Modelling COMED
##-------------------------------------------------------------------------------------------

#Fit model
mod_CM <- brm(data = df_cp, family = poisson(link = "log"),
              COMED_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_CM)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_CM <- mod_CM %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_CM %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_CM <- 
  ggplot(tidy_epred_CM, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_CM <- mod_CM %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_CM %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_CM$.value>0,contraste_cat_dia_local_CM$DIA.ENSAYO, contraste_cat_dia_local_CM$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_CM <- ggplot(contraste_cat_dia_local_CM, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_cm = (plot_cat_dia_local_CM| plot_cont_cat_dia_local_CM + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'COMED' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_cm, file = "plots/COMED_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_cm (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_CM %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_CM <- 
  ggplot(tidy_epred_CM, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_CM <- mod_CM %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_CM %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_CM$.value>0,contraste_local_dia_cat_CM$DIA.ENSAYO, contraste_local_dia_cat_CM$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_CM <- ggplot(contraste_local_dia_cat_CM, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_cm = (plot_local_dia_cat_CM| plot_cont_local_dia_cat_CM + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'COMED' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_cm, file = "plots/COMED_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_cm (for debug)

##-------------------------------------------------------------------------------------------
## Modelling BEBED
##-------------------------------------------------------------------------------------------

#Fit model
mod_BB <- brm(data = df_cp, family = poisson(link = "log"),
              BEBED_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_BB)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_BB <- mod_BB %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_BB %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_BB <- 
  ggplot(tidy_epred_BB, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_BB <- mod_BB %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_BB %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_BB$.value>0,contraste_cat_dia_local_BB$DIA.ENSAYO, contraste_cat_dia_local_BB$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_BB <- ggplot(contraste_cat_dia_local_BB, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_bb = (plot_cat_dia_local_BB| plot_cont_cat_dia_local_BB + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'BEBED' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_bb, file = "plots/BEBED_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_bb (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_BB %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_BB <- 
  ggplot(tidy_epred_BB, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_BB <- mod_BB %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_BB %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_BB$.value>0,contraste_local_dia_cat_BB$DIA.ENSAYO, contraste_local_dia_cat_BB$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_BB <- ggplot(contraste_local_dia_cat_BB, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_bb = (plot_local_dia_cat_BB| plot_cont_local_dia_cat_BB + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'BEBED' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_bb, file = "plots/BEBED_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_bb (for debug)

##-------------------------------------------------------------------------------------------
## Modelling CAMIN
##-------------------------------------------------------------------------------------------

#Fit model
mod_CA <- brm(data = df_cp, family = poisson(link = "log"),
              CAMIN_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_CA)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_CA <- mod_CA %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_CA %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_CA <- 
  ggplot(tidy_epred_CA, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_CA <- mod_CA %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_CA %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_CA$.value>0,contraste_cat_dia_local_CA$DIA.ENSAYO, contraste_cat_dia_local_CA$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_CA <- ggplot(contraste_cat_dia_local_CA, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_ca = (plot_cat_dia_local_CA| plot_cont_cat_dia_local_CA + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'CAMIN' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_ca, file = "plots/CAMIN_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_ca (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_CA %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_CA <- 
  ggplot(tidy_epred_CA, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_CA <- mod_CA %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_CA %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_CA$.value>0,contraste_local_dia_cat_CA$DIA.ENSAYO, contraste_local_dia_cat_CA$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_CA <- ggplot(contraste_local_dia_cat_CA, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_ca = (plot_local_dia_cat_CA| plot_cont_local_dia_cat_CA + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'CAMIN' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_ca, file = "plots/CAMIN_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_ca (for debug)


##-------------------------------------------------------------------------------------------
## Modelling SOCIAL
##-------------------------------------------------------------------------------------------

#Fit model
mod_SO <- brm(data = df_cp, family = poisson(link = "log"),
              SOCIAL_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_SO)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_SO <- mod_SO %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_SO %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_SO <- 
  ggplot(tidy_epred_SO, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_SO <- mod_SO %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_SO %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_SO$.value>0,contraste_cat_dia_local_SO$DIA.ENSAYO, contraste_cat_dia_local_SO$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_SO <- ggplot(contraste_cat_dia_local_SO, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_so = (plot_cat_dia_local_SO| plot_cont_cat_dia_local_SO + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'SOCIAL' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_so, file = "plots/SOCIAL_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_so (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_SO %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_SO <- 
  ggplot(tidy_epred_SO, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_SO <- mod_SO %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_SO %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_SO$.value>0,contraste_local_dia_cat_SO$DIA.ENSAYO, contraste_local_dia_cat_SO$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_SO <- ggplot(contraste_local_dia_cat_SO, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_so = (plot_local_dia_cat_SO| plot_cont_local_dia_cat_SO + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'SOCIAL' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_so, file = "plots/SOCIAL_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_so (for debug)


##-------------------------------------------------------------------------------------------
## Modelling AMB
##-------------------------------------------------------------------------------------------

#Fit model
mod_AMB <- brm(data = df_cp, family = poisson(link = "log"),
              AMB_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_AMB)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_AMB <- mod_AMB %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_AMB %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_AMB <- 
  ggplot(tidy_epred_AMB, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_AMB <- mod_AMB %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_AMB %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_AMB$.value>0,contraste_cat_dia_local_AMB$DIA.ENSAYO, contraste_cat_dia_local_AMB$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_AMB <- ggplot(contraste_cat_dia_local_AMB, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_amb = (plot_cat_dia_local_AMB| plot_cont_cat_dia_local_AMB + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'AMB' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_amb, file = "plots/AMB_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_amb (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_AMB %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_AMB <- 
  ggplot(tidy_epred_AMB, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_AMB <- mod_AMB %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_AMB %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_AMB$.value>0,contraste_local_dia_cat_AMB$DIA.ENSAYO, contraste_local_dia_cat_AMB$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_AMB <- ggplot(contraste_local_dia_cat_AMB, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_amb = (plot_local_dia_cat_AMB| plot_cont_local_dia_cat_AMB + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'AMB' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_amb, file = "plots/AMB_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_so (for debug)


##-------------------------------------------------------------------------------------------
## Modelling OTROS
##-------------------------------------------------------------------------------------------

#Fit model
mod_OT <- brm(data = df_cp, family = poisson(link = "log"),
               OTROS_f ~   CATEGORIA*DIA.ENSAYO*LUGAR + (1 | ID:CATEGORIA:LUGAR),
               iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
               prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
               refresh = 0, seed = 123)


summary(mod_OT)

#===========================================================================

#Computing posterior prediction of simulation draws (used for inference)
tidy_epred_OT <- mod_OT %>% 
  epred_draws(newdata = newdata, re_formula = NULL)


#==========================================================================
#Comparing categories across days on both localities
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OT %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_cat_dia_local_OT <- 
  ggplot(tidy_epred_OT, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = CATEGORIA)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Predictive posterior frequency", y = "Days",
       fill = "Category") +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(LUGAR))

# Difference between categories across days on both places

contraste_cat_dia_local_OT <- mod_OT %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","LUGAR")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_cat_dia_local_OT %>% 
  group_by(DIA.ENSAYO, LUGAR) %>% median_hdi(.value)

#Proportions
table(contraste_cat_dia_local_OT$.value>0,contraste_cat_dia_local_OT$DIA.ENSAYO, contraste_cat_dia_local_OT$LUGAR)/4000

#Plot---------------------------------------------------------

plot_cont_cat_dia_local_OT <- ggplot(contraste_cat_dia_local_OT, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Vaq - Vaca)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(LUGAR))


# Combined plot
g_cdl_ot = (plot_cat_dia_local_OT| plot_cont_cat_dia_local_OT + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OTROS' between categories across days on both localities",
                  theme = theme_clean()) 
ggsave(g_cdl_ot, file = "plots/OTROS_btw_cat.pdf", width = 8, height = 6.5)
#g_cdl_ot (for debug)


#==========================================================================
#Comparing localities across days on both Categories
#==========================================================================

#Point prediction with 0,95 interval
tidy_epred_OT %>% 
  group_by(DIA.ENSAYO, LUGAR, CATEGORIA) %>% 
  median_hdi(.epred)

#Plot---------------------------------------------------------

plot_local_dia_cat_OT <- 
  ggplot(tidy_epred_OT, 
         aes(x = .epred, y = DIA.ENSAYO,
             fill = LUGAR)) +
  stat_halfeye(na.rm=T, slab_alpha = 0.75) +
  labs(x = "Posterior predictive frequency", y = "Days",
       fill = "Local") + 
  scale_fill_manual(values = c("thistle", "lightgreen")) +
  theme_clean() +
  theme(legend.position = "bottom")+
  xlim(0,6)+
  facet_wrap(vars(CATEGORIA))

#Difference between localities across days on both categories

contraste_local_dia_cat_OT <- mod_OT %>% 
  emmeans(~ CATEGORIA*DIA.ENSAYO*LUGAR,
          epred = TRUE) %>% 
  contrast(method = "revpairwise", by = c("DIA.ENSAYO","CATEGORIA")) %>% 
  gather_emmeans_draws()

#Point prediction of contrast with 0,95 interval
contraste_local_dia_cat_OT %>% 
  group_by(DIA.ENSAYO, CATEGORIA) %>% median_hdi(.value)

#Proportions
table(contraste_local_dia_cat_OT$.value>0,contraste_local_dia_cat_OT$DIA.ENSAYO, contraste_local_dia_cat_OT$CATEGORIA)/4000

#Plot---------------------------------------------------------

plot_cont_local_dia_cat_OT <- ggplot(contraste_local_dia_cat_OT, aes(x = .value, y = DIA.ENSAYO, fill = after_stat(x < 0))) +
  stat_halfeye() +
  labs(x = "Contrast (Salta - Rafaela)", y = NULL) +
  theme_clean() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(vars(CATEGORIA))


# Combined plot
g_ldc_ot = (plot_local_dia_cat_OT| plot_cont_local_dia_cat_OT + theme(axis.text.y = element_blank())) +
  plot_annotation(#title = "Posterior predictive frequency of 'OTROS' between  localities across days on both categories",
                  theme = theme_clean()) 
ggsave(g_ldc_ot, file = "plots/OTROS_btw_loc.pdf", width = 8, height = 6.5)
#g_ldc_ot (for debug)

#=========================================================================================================================
#END
#=========================================================================================================================
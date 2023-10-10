## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_sc.R
##
## Purpose of script: Modeling animal score of lameness with glm multilevel Bayesian models 
## To run this script use the following command: Rscript script_sc.R 
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

#Defining the prior
prior<-c(set_prior("normal(0, 1.5)", class = "Intercept"),
         set_prior("normal(0, 0.5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:Category:Locality")
)


##-------------------------------------------------------------------------------------------
## Modelling Score limpeza
##-------------------------------------------------------------------------------------------


#Fit model
mod_limp <- brm(data = df_sc, family = cumulative("logit"),
              SCORE_LIMPEZA ~   Category*Day*Locality + (1 | ID:Category:Locality),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123)


summary(mod_limp)

condition1_limp <- make_conditions(mod_limp, c("Locality", "Day"))
condition2_limp <- make_conditions(mod_limp, c("Category", "Day"))

p1_limp = conditional_effects(mod_limp, categorical = T, "Category", 
                    conditions = condition1_limp, method = "posterior_epred") 
  
g1_limp = plot(p1_limp, plot = FALSE)[[1]] 

g1_limp = 
g1_limp +
  theme_clean() +
  labs(title = NULL, x = "Category", 
       fill = "'Limp' score", 
       colour = "'Limp' score") 

ggsave(g1_limp, file = "plots/LIMP_cat.pdf", width = 12, height =8)
#g1_limp (for debug)

p2_limp = conditional_effects(mod_limp, categorical = T, "Locality", 
                    conditions = condition2_limp, method = "posterior_epred")


g2_limp = plot(p2_limp, plot = FALSE)[[1]] 

g2_limp = 
g2_limp +
  theme_clean() +
  labs(title = NULL, x = "Locality", 
       fill = "Lameness score", 
       colour = "Lameness score") 
ggsave(g2_limp, file = "plots/LIMP_loc.pdf", width = 12, height =8)
#g2_limp (for debug)


##-------------------------------------------------------------------------------------------
## Modelling Score podal
##-------------------------------------------------------------------------------------------


#Fit model
mod_pod <- brm(data = df_sc, family = cumulative("logit"),
                SCORE_PODAL ~   Category*Day*Locality + (1 | ID:Category:Locality),
                iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
                prior = prior, control = list(adapt_delta = 0.99, max_treedepth = 13),
                refresh = 0, seed = 123)


summary(mod_pod)

condition1_pod  <- make_conditions(mod_pod, c("Locality", "Day"))
condition2_pod  <- make_conditions(mod_pod, c("Category", "Day"))

p1_pod  = conditional_effects(mod_pod, categorical = T, "Category", 
                         conditions = condition1_pod , method = "posterior_epred") 

g1_pod  = plot(p1_pod , plot = FALSE)[[1]] 

g1_pod  =
g1_pod  +
  theme_clean() +
  labs(title = NULL, x = "Category", 
       fill = "'Podal' score", 
       colour = "'Podal' score") 
ggsave(g1_pod, file = "plots/POD_cat.pdf", width = 12, height =8)
#g1_pod (for debug)

p2_pod  = conditional_effects(mod_pod , categorical = T, "Locality", 
                         conditions = condition2_pod , method = "posterior_epred")

g2_pod  = plot(p2_pod , plot = FALSE)[[1]] 

g2_pod  =
g2_pod  +
  theme_clean() +
  labs(title = NULL, x = "Locality", 
       fill = "'Podal' score", 
       colour = "'Podal' score")
ggsave(g2_pod, file = "plots/POD_loc.pdf", width = 12, height =8)
#g2_pod (for debug)

#---------------------------------------------------------
#For the other two variables only in Salta
#---------------------------------------------------------

##-------------------------------------------------------------------------------------------
## Modelling Score ubere
##-------------------------------------------------------------------------------------------

prior2<-c(set_prior("normal(0, 1.5)", class = "Intercept"),
         set_prior("normal(0, 0.5)", class = "b"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:Category")
)


#Fit model
mod_ube <- brm(data = df_sc, family = cumulative("logit"),
               SCORE_UBERE ~   Category*Day + (1 | ID:Category),
               iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
               prior = prior2, control = list(adapt_delta = 0.99, max_treedepth = 13),
               refresh = 0, seed = 123)


summary(mod_ube)

condition_ube  <- make_conditions(mod_ube, c("Day"))

p_ube  = conditional_effects(mod_ube, categorical = T, "Category", 
                              conditions = condition_ube , method = "posterior_epred") 

g_ube  = plot(p_ube, plot = FALSE)[[1]] 

  g_ube  =
  g_ube  +
  theme_clean() +
  labs(title = NULL, x = "Category", 
       fill = "'Ubere' score", 
       colour = "'Ubere' score") 
ggsave(g_ube, file = "plots/UBE_cat.pdf", width = 10, height =7)
#g_ube (for debug)

##-------------------------------------------------------------------------------------------
## Modelling Score garrones
##-------------------------------------------------------------------------------------------


#Fit model
mod_gar <- brm(data = df_sc, family = cumulative("logit"),
               SCORE_GARRONES ~   Category*Day + (1 | ID:Category),
               iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
               prior = prior2, control = list(adapt_delta = 0.99, max_treedepth = 13),
               refresh = 0, seed = 123)


summary(mod_gar)

condition_gar  <- make_conditions(mod_gar, c("Day"))

p_gar  = conditional_effects(mod_gar, categorical = T, "Category", 
                             conditions = condition_gar , method = "posterior_epred") 

g_gar  = plot(p_gar, plot = FALSE)[[1]] 

  g_gar  =
  g_gar  +
  theme_clean() +
  labs(title = NULL, x = "Category", 
       fill = "'Garrones' score", 
       colour = "'Garrones' score") 
ggsave(g_gar, file = "plots/GAR_cat.pdf", width = 10, height =7)
#g_gar(for debug)

#========================================================================
#END
#========================================================================
## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_priors.R
##
## Purpose of script: Select realiable priors for the bayesian models.
## To run this script use the following command: Rscript script_priors.R 
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
## Modelling the priors 
##-------------------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------
#BEHAVIOR
##-------------------------------------------------------------------------------------------

#Intercept

#Defining vague priors FOR LOG MODELS
prior_aNI<-c(set_prior("normal(0, 10)", class = "Intercept"),
             set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR")#,
)

#Defining a more reliable prior
prior_aI<-c(set_prior("normal(1, 0.3)", class = "Intercept"),
            set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR")#,
)

#Fit intercept-only models with both priors
mod_aNI <- brm(data = df_cp, family = poisson(link = "log"),
             OCIOP_f ~   1 + (1 | ID:CATEGORIA:LUGAR),
             iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
             prior = prior_aNI, control = list(adapt_delta = 0.99, max_treedepth = 13),
             refresh = 0, seed = 123,
             sample_prior = T)

mod_aI<- update(mod_aNI, prior = prior_aI)


# Ploting the prior probability of lambda
bind_rows(prior_draws(mod_aNI),
          prior_draws(mod_aI)) %>% 
  mutate(p = exp(Intercept),
         w = factor(rep(c("NI", "I"), each = n() / 2),
                    levels = c("NI", "I"))) %>% 
  # plot
  ggplot(aes(x = p, fill = w)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  labs(title = "Intercept-only model",
       x = "Prior probability of lambda") +
  xlim(0,50) +
  ylim(0,10)

#======================================================================

#Vector of betas

#Fitting models with treatments 
#Here we have to use the non linear sintax of brms


#1st (vague prior for b) comparing overall difference between "CATEGORIA"
mod_bNI <- 
  brm(data = df_cp, family = poisson(link = "log"),
      bf(OCIOP_f ~ a + b,
         a ~ 1 + (1 | ID:CATEGOR:LUGAR), 
         b ~ 0 + CATEGORIA*DIA.ENSAYO*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(1, 0.3), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:LUGAR)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      sample_prior = T)

#2nd (more reliable prior for b) comparing overall difference between "CATEGORIA" 
mod_bI <- 
  brm(data = df_cp, family = poisson(link = "log"),
      bf(OCIOP_f ~ a + b,
         a ~ 1 + (1 | ID:CATEGORIA:LUGAR), 
         b ~ 0 + CATEGORIA*DIA.ENSAYO*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(1, 0.3), nlpar = a),
                prior(normal(0, 0.5), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 0.5), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:LUGAR)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      sample_prior = T)

# Ploting the prior probability of difference between "CATEGORIA"
prior_diff <-
  bind_rows(prior_draws(mod_bNI),
            prior_draws(mod_bI)) %>% 
  mutate(w  = factor(rep(c("NI", "I"), each = n() / 2),
                     levels = c("NI", "I")),
         p1 = exp(b_a + b_b_CATEGORIAVaq),
         p2 = exp(b_a + b_b_CATEGORIAVaca)) %>% 
  mutate(diff = abs(p1 - p2)) 

# plot
prior_diff %>% 
  ggplot(aes(x = diff, fill = w)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Prior difference between categories") +
  xlim(0,50)

#Selected prior
 
prior_cp <-c(set_prior("normal(1, 0.3)", class = "Intercept"),
             set_prior("normal(0, 0.5)", class = "b"),
             set_prior("cauchy(0, 0.5)", class= "sd", group="ID:CATEGORIA:LUGAR")#,
)


#======================================================================

##-------------------------------------------------------------------------------------------
#Respitatory RATE
##-------------------------------------------------------------------------------------------

#Intercept

#Using already defined vague priors for log poisson models 
prior_aNI<-c(set_prior("normal(0, 10)", class = "Intercept"),
             set_prior("cauchy(0, 25)", class="sd", group="ID:LUGAR")#,
)

#Defining a more reliable prior
prior_aI_fr<-c(set_prior("normal(3.5, 0.5)", class = "Intercept"), # values to delimit FR a valores plausiveis 
               set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR")#,
)

#Fit intercept-only models with both priors

mod_aNI_fr <- brm(data = df_fr, family = poisson(link = "log"),
              FR ~   CATEGORIA*DIA*LUGAR + (1 | ID:CATEGORIA:LUGAR),
              iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
              prior = prior_aNI, control = list(adapt_delta = 0.99, max_treedepth = 13),
              refresh = 0, seed = 123,
              sample_prior = T)



mod_aI_fr <- update(mod_aNI_fr, prior = prior_aI_fr)


# Ploting the prior probability of lambda
bind_rows(prior_draws(mod_aNI_fr),
          prior_draws(mod_aI_fr)) %>% 
  mutate(p = exp(Intercept),
         w = factor(rep(c("NI", "I"), each = n() / 2),
                    levels = c("NI", "I"))) %>% 
  # plot
  ggplot(aes(x = p, fill = w)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  labs(title = "Intercept-only model",
       x = "Prior probability of lambda") +
  xlim(0,100) +
  ylim(0,10)

#======================================================================

#Vector of betas

#Fitting models with treatments 
#Here we have to use the non linear sintax of brms

#1st (vague prior for b) comparing overall difference between "CATEGORIA"


mod_bNI_fr <- 
  brm(data = df_fr, family = poisson(link = "log"),
      bf(FR ~ a + b,
         a ~ 1 + (1 |  ID:CATEGORIA:LUGAR), 
         b ~ 0 + CATEGORIA*DIA*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(3.5, 0.5), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:CATEGORIA:LUGAR)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

#2nd (more reliable prior for b) comparing overall difference between "CATEGORIA" 
mod_bI_fr <- 
  brm(data = df_fr, family = poisson(link = "log"),
      bf(FR ~ a + b,
         a ~ 1 + (1 |  ID:CATEGORIA:LUGAR), 
         b ~ 0 + CATEGORIA*DIA*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(3.5, 0.5), nlpar = a),
                prior(normal(0, 0.5), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 0.5), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:CATEGORIA:LUGAR)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

# Ploting the prior probability of difference between "CATEGORIA"
prior_fr_diff <-
  bind_rows(prior_draws(mod_bNI_fr),
            prior_draws(mod_bI_fr)) %>% 
  mutate(priori = factor(rep(c("NI", "I"), each = n() / 2),
                     levels = c("NI", "I")),
         p1 = exp(b_a + b_b_CATEGORIAVaq),
         p2 = exp(b_a + b_b_CATEGORIAVaca)) %>% 
  mutate(diff = abs(p1 - p2)) 

# plot
prior_fr_diff %>% 
  ggplot(aes(x = diff, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Prior difference between categories") +
  xlim(0,100)
#======================================================================

#Selected prior

prior_fr <-c(set_prior("normal(3.5, 0.5)", class = "Intercept"),
             set_prior("normal(0, 0.5)", class = "b"),
             set_prior("cauchy(0, 0.5)", class= "sd", group="ID:CATEGORIA:LUGAR")#,
)



#======================================================================

##-------------------------------------------------------------------------------------------
#Score Lameness
##-------------------------------------------------------------------------------------------

#Intercept

#Defining some vague priors
prior_aNI_sc<-c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("cauchy(0, 25)", class="sd", group="ID:Category:Locality")#,
)

#Defining a more reliable prior
prior_aI_sc<-c(set_prior("normal(0, 1.5)", class = "Intercept"), # values to delimit FR a valores plausiveis 
               set_prior("cauchy(0, 25)", class="sd", group="ID:Category:Locality")#,
)

#Fit intercept-only models with both priors

mod_aNI_sc <- brm(data = df_sc, family = cumulative("logit"),
                  SCORE_LIMPEZA ~   1 + (1 | ID:Category:Locality),
                  iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
                  prior = prior_aNI_sc, control = list(adapt_delta = 0.99, max_treedepth = 13),
                  refresh = 0, seed = 123,
                  sample_prior = T)


mod_aI_sc <- update(mod_aNI_sc, prior = prior_aI_sc)


# Ploting the prior probability of lambda
bind_rows(prior_draws(mod_aNI_sc),
          prior_draws(mod_aI_sc)) %>% 
  mutate(p = inv_logit_scaled(Intercept),
         priori = factor(rep(c("NI", "I"), each = n() / 2),
                    levels = c("NI", "I"))) %>% 
  # plot
  ggplot(aes(x = p, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  labs(title = "Intercept-only model",
       x = "Prior probability") +
  xlim(0,1) +
  ylim(0,10)

#======================================================================

#Vector of betas

#Fitting models with treatments 
#Here we have to use the non linear sintax of brms

#1st (vague prior for b) comparing overall difference between "CATEGORIA"


mod_bNI_sc <- 
  brm(data = df_sc, family = cumulative("logit"),
      bf(SCORE_LIMPEZA ~ a + b,
         a ~ 1 + (1 | ID:Category:Locality), 
         b ~ 0 + Category*Day*Locality,
         nl = TRUE),
      prior = c(prior(normal(0, 1.5), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = CategoryVaq),
                prior(normal(0, 10), nlpar = b, coef = CategoryVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:Category:Locality)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

#2nd (more reliable prior for b) comparing overall difference between "CATEGORIA" 
mod_bI_fr <- 
  brm(data = df_sc, family = cumulative("logit"),
      bf(SCORE_LIMPEZA ~ a + b,
         a ~ 1 + (1 | ID:Category:Locality), 
         b ~ 0 + Category*Day*Locality,
         nl = TRUE),
      prior = c(prior(normal(0, 1.5), nlpar = a),
                prior(normal(0, 0.5), nlpar = b, coef = CategoryVaq),
                prior(normal(0, 0.5), nlpar = b, coef = CategoryVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:Category:Locality)),
      iter = 2000, warmup = 1000, chains = 4, cores = ncores,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

# Ploting the prior probability of difference between "CATEGORIA"
prior_sc_diff <-
  bind_rows(prior_draws(mod_bNI_sc),
            prior_draws(mod_bI_sc)) %>% 
  mutate(priori  = factor(rep(c("NI", "I"), each = n() / 2),
                     levels = c("NI", "I")),
         p1 = inv_logit_scaled(b_a + b_b_CATEGORIAVaq),
         p2 = inv_logit_scaled(b_a + b_b_CATEGORIAVaca)) %>% 
  mutate(diff = abs(p1 - p2)) 

# plot
prior_sc_diff %>% 
  ggplot(aes(x = diff, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Prior difference between categories") +
  xlim(0,100)
#======================================================================

#Selected prior

prior_sc <-c(set_prior("normal(0, 1.5)", class = "Intercept"),
             set_prior("normal(0, 0.5)", class = "b"),
             set_prior("cauchy(0, 25)", class= "sd", group="ID:Categoty:Locality")#,
)


##-------------------------------------------------------------------------------------------
#Temperature of bed
##-------------------------------------------------------------------------------------------

#Intercept

#Defining some vague priors
prior_aNI_tc<-prior<-c(set_prior("normal(0, 10)", class = "Intercept"),
                       #set_prior("normal(0, 10)", class = "b")#,
                       set_prior("cauchy(0, 25)", class="sigma")
)

#Defining a more reliable prior
prior_aI_tc<-c(set_prior("normal(25, 10)", class = "Intercept"),
               #set_prior("normal(0, 0.5)", class = "b")#,
               set_prior("cauchy(0, 25)", class="sigma")
)

#Fit intercept-only models with both priors

mod_aNI_tc <- brm(data = df_tc, family = gaussian(),
                  TS ~  1,
                  iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
                  prior = prior_aNI_tc, control = list(adapt_delta = 0.99, max_treedepth = 13),
                  refresh = 0, seed = 123,
                  sample_prior = T)


mod_aI_tc <- update(mod_aNI_tc, prior = prior_aI_tc)

# Ploting the prior probability of lambda
bind_rows(prior_draws(mod_aNI_tc),
          prior_draws(mod_aI_tc)) %>% 
  mutate(y = Intercept,
         priori = factor(rep(c("NI", "I"), each = n() / 2),
                         levels = c("NI", "I"))) %>% 
  # plot
  ggplot(aes(x = y, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  labs(title = "Intercept-only model",
       x = "Prior average temperature") 

#======================================================================

#Vector of betas

#Fitting models with treatments 
#Here we have to use the non linear sintax of brms

#1st (vague prior for b) comparing overall difference between "MOMENTO"

mod_bNI_tc <- 
  brm(data = df_tc, family = gaussian(),
      bf(TS ~ a + b,
         a ~ 1 , 
         b ~ 0 + MOMENTO*Day*Locality,
         nl = TRUE),
      prior = c(prior(normal(25, 10), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = MOMENTOMañana),
                prior(normal(0, 10), nlpar = b, coef = MOMENTOTarde),
                prior(cauchy(0, 25), class = sigma)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

#2nd (more reliable prior for b) comparing overall difference between "CATEGORIA" 
mod_bI_tc <- 
  brm(data = df_tc, family = gaussian(),
      bf(TS ~ a + b,
         a ~ 1 , 
         b ~ 0 + MOMENTO*Day*Locality,
         nl = TRUE),
      prior = c(prior(normal(25, 10), nlpar = a),
                prior(normal(0, 5), nlpar = b, coef = MOMENTOMañana),
                prior(normal(0, 5), nlpar = b, coef = MOMENTOTarde),
                prior(cauchy(0, 25), class = sigma)
               ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

# Ploting the prior probability of difference between "CATEGORIA"
prior_tc_diff <-
  bind_rows(prior_draws(mod_bNI_tc),
            prior_draws(mod_bI_tc)) %>% 
  mutate(priori  = factor(rep(c("NI", "I"), each = n() / 2),
                          levels = c("NI", "I")),
         y1 = b_a + b_b_MOMENTOMañana,
         y2 = b_a + b_b_MOMENTOTarde) %>% 
  mutate(diff = y1 - y2) 

# plot
prior_tc_diff %>% 
  ggplot(aes(x = diff, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Prior difference between moments") 
#======================================================================

#Selected prior

prior_tc <-c(set_prior("normal(25, 10)", class = "Intercept"),
             set_prior("normal(0, 5)", class = "b"),
             set_prior("cauchy(0, 25)", class="sigma")
)



##-------------------------------------------------------------------------------------------
#Milk production
##-------------------------------------------------------------------------------------------

#Intercept

#Defining some vague priors
prior_aNI_pl<-c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR"),
                set_prior("cauchy(0, 25)", class="sigma")
)

#Defining a more reliable prior

prior_aI_pl<-c(set_prior("normal(35, 10)", class = "Intercept"),
         set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR"),
         set_prior("cauchy(0, 25)", class="sigma")
)
#Fit intercept-only models with both priors

mod_aNI_pl <- brm(data = df_pl, family = gaussian(),
                  PL_m ~  1 + (1|ID:CATEGORIA:LUGAR),
                  iter = 2000, warmup = 1000, chains = 4, cores = ncores, 
                  prior = prior_aNI_pl, control = list(adapt_delta = 0.99, max_treedepth = 13),
                  refresh = 0, seed = 123,
                  sample_prior = T)


mod_aI_pl <- update(mod_aNI_pl, prior = prior_aI_pl)

# Ploting the prior probability of lambda
bind_rows(prior_draws(mod_aNI_pl),
          prior_draws(mod_aI_pl)) %>% 
  mutate(y = Intercept,
         priori = factor(rep(c("NI", "I"), each = n() / 2),
                         levels = c("NI", "I"))) %>% 
  # plot
  ggplot(aes(x = y, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  labs(title = "Intercept-only model",
       x = "Prior average milk production") 

#======================================================================

#Vector of betas

#Fitting models with treatments 
#Here we have to use the non linear sintax of brms

#1st (vague prior for b) comparing overall difference between "MOMENTO"

mod_bNI_pl <- 
  brm(data = df_pl, family = gaussian(),
      bf(PL_m ~ a + b,
         a ~ 1 + (1|ID:CATEGORIA:LUGAR) , 
         b ~ 0 + CATEGORIA*WEEK*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(35, 10), nlpar = a),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 10), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:CATEGORIA:LUGAR),
                prior(cauchy(0, 25), class = sigma)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

#2nd (more reliable prior for b) comparing overall difference between "CATEGORIA" 
mod_bI_pl <- 
  brm(data = df_pl, family = gaussian(),
      bf(PL_m ~ a + b,
         a ~ 1 + (1|ID:CATEGORIA:LUGAR) , 
         b ~ 0 + CATEGORIA*WEEK*LUGAR,
         nl = TRUE),
      prior = c(prior(normal(35, 10), nlpar = a),
                prior(normal(0, 5), nlpar = b, coef = CATEGORIAVaq),
                prior(normal(0, 5), nlpar = b, coef = CATEGORIAVaca),
                prior(cauchy(0, 25), class = sd, nlpar = a, group = ID:CATEGORIA:LUGAR),
                prior(cauchy(0, 25), class = sigma)
      ),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 11,
      control = list(adapt_delta = 0.99, max_treedepth = 13),
      sample_prior = T)

# Ploting the prior probability of difference between "CATEGORIA"
prior_pl_diff <-
  bind_rows(prior_draws(mod_bNI_pl),
            prior_draws(mod_bI_pl)) %>% 
  mutate(priori  = factor(rep(c("NI", "I"), each = n() / 2),
                          levels = c("NI", "I")),
         y1 = b_a + b_b_CATEGORIAVaq,
         y2 = b_a + b_b_CATEGORIAVaca) %>% 
  mutate(diff = y1 - y2) 

# plot
prior_pl_diff %>% 
  ggplot(aes(x = diff, fill = priori)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Prior difference between categories") 
#======================================================================

#Selected prior

prior_pl <-c(set_prior("normal(35, 10)", class = "Intercept"),
             set_prior("normal(0, 5)", class = "b"),
             set_prior("cauchy(0, 25)", class="sd", group="ID:CATEGORIA:LUGAR"),
             set_prior("cauchy(0, 25)", class="sigma")
)

#======================================================================================
#Erasing files

cat("Erasing unecessary files")


patterns <- paste(c("_aI", "_aNI", "diff", "mod_"), collapse = "|")
rm(list=setdiff(ls(pattern = patterns), lsf.str()))


#=======================================================================================================
#END
#=======================================================================================================

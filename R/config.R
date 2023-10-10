## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: config.R
##
## Purpose of script: Script to configure libraries, plots, and other useful functions
## The script is loaded at the beginning of each analysis with the following command: source("R/config.R")
##
## Author: Edgar de Souza Vismara
##         Frederico Marcio Vieira
##
## Date Created: 2023-09-10

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------


cat("Definindo opções... \n\n", sep = "")
## Defining options...

options(scipen = 999) # disable cientific notation 
options(encoding = "UTF-8") # defining the encoding to UTF-8 


## Installíng and loading libraries.. -------------------------------------------
cat("Installíng and loading libraries... \n\n", sep = "")


packages <- c("tidyverse", "readxl", "rstan", "brms", "parallel", "tidybayes", "patchwork", "emmeans", "lubridate", "scales") # lista de pacotes para carregar
n_packages <- length(packages) # list of required libraries

new.pkg <- packages[!(packages %in% installed.packages())] # Indicate which library is already installed.

# install missing packages... 
if(length(new.pkg)){
  install.packages(new.pkg)
}

# Loading the libraries
for(n in 1:n_packages){
  cat("Carregando pacote #", n, " de ", n_packages, "... Carregando atualmente: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") 
  eval(parse(text = lib_load)) 
}

#Enable Parallel computing
ncores = detectCores()
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Custom ggplot theme to make pretty plots

theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey90", color = NA),
          legend.title = element_text(face = "bold"))
}

## ------------------------------------------------------------------------------------------
## Creating Output dirs
## ------------------------------------------------------------------------------------------

if(!dir.exists("plots/")) {
  dir.create("plots/", showWarnings=FALSE, recursive=TRUE)
}

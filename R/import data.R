## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: script_import_data.R
##
## Purpose of script: Read and pre-process all datasets.
## 
##
## Author: Edgar de Souza Vismara
##         Frederico Marcio Vieira
##
## Date Created: 2023-10-04

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

library(readxl)

## ------------------------------------------------------------------------------------------
## 1. Reading datasets
## ------------------------------------------------------------------------------------------

path = "data/PLANILHA_DADOS_INTA_2023 (completa).xlsx"# defining path to folder data

#--------------------------------------------------------------------------------------------

cat(" - Loading behavior data\n")

df_cp <- read_excel(path, sheet = "COMPORTAMIENTO", 
                    col_types = c("text", "date", "numeric", 
                                  "date", "numeric", "text",
                                  "numeric", "numeric", "numeric",
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"),
                    .name_repair = "universal")
#=============================================================================

cat(" - Loading heart rate data\n")

df_fr <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "FREQ.RESP.", 
                    col_types = c("text", "date", "text", 
                                  "text", "text", "text", 
                                  "numeric"),
                    .name_repair = "universal")
#=============================================================================

cat(" - Loading lameness data\n")

df_sc <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "SCORE LIM. Y PODAL", 
                    col_types = c("text", "date", "numeric", 
                                  "text", "text", "numeric", 
                                  "numeric", "numeric",
                                  "numeric"))
#=============================================================================

cat(" - Loading temperature of bed data\n")

df_tc <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "TEMP_CAMA", 
                    col_types = c("text", "date", "text", 
                                  "text", "text", "numeric", 
                                  "numeric", "numeric",
                                  "numeric", "numeric"))

#=============================================================================

cat(" - Loading weather (met station) data\n")

df_em <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "EST.MET.",
                    trim_ws = TRUE)


cat(" - Loading microclimate data\n")

df_tbt <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "TEMP.BAJO.TINGL.",
                    trim_ws = TRUE)


#=============================================================================

cat(" - Loading milk production  data\n")

df_pl <- read_excel("data/PLANILHA_DADOS_INTA_2023 (completa).xlsx",
                    sheet = "PL DIARIA",
                    trim_ws = TRUE)

#=============================================================================


## ------------------------------------------------------------------------------------------
## Manipulate data
## ------------------------------------------------------------------------------------------

#df_cp --------------------------------------------
#Creating frequency data (counts per time interval)

df_cp = df_cp %>% 
  group_by(LUGAR, DIA.ENSAYO, CATEGORIA, ID) %>% 
  summarise_at(.vars = vars(OCIOP,OCIOE,RUMIP,RUMIE,COMED,BEBED,CAMIN,SOCIAL,AMB,OTROS),
               .funs = c(f = "sum"))%>% 
  mutate(ID = as.factor(ID), DIA.ENSAYO = as.factor(DIA.ENSAYO))

#df_fr --------------------------------------------
#Coverting score to factors

df_fr = df_fr %>% 
  mutate(ID = as.factor(ID), DIA = factor(DIA, levels = c(1:12)))

#df_sc --------------------------------------------
#Coverting score to factors

df_sc = df_sc %>%
  mutate_at(vars(SCORE_LIMPEZA, SCORE_PODAL, SCORE_UBERE, SCORE_GARRONES), 
            ~factor(., levels=c(1:4),ordered=T)) %>% 
  mutate(DIA = factor(DIA, levels = c(1:12))) %>% 
  rename(Day = DIA, Locality = LUGAR, Category = CATEGORIA)

#df_tc --------------------------------------------
#Coverting score to factors
df_tc = df_tc %>%
  mutate(DIA = factor(DIA, levels = c(1:12))) %>% 
  rename(Day = DIA, Locality = LUGAR)


#df_em -------------------------------------------
df_em = df_em %>%#unifying date and time
  mutate(FECHA = as_date(FECHA), 
         HORA = format(HORA, format = "%H:%M:%S"),
         FECHA.HORA = as.POSIXct(paste(FECHA, HORA)),
         HORA = as.POSIXct(HORA, format = "%H:%M:%S")
  )
         


#df_tbt -------------------------------------------
df_tbt = df_tbt %>%#unifying date and time
  mutate(FECHA = as_date(FECHA), 
         HORA = format(HORA, format = "%H:%M:%S"),
         FECHA.HORA = as.POSIXct(paste(FECHA, HORA)),
         HORA = as.POSIXct(HORA, format = "%H:%M:%S")
  )



#df_pl -------------------------------------------
df_pl = df_pl %>%#change N.ORD to factors
  mutate(PL_m = PL/N.ORD,
         FECHA = as.Date(FECHA),
         WEEK = (as.numeric(FECHA-FECHA[1]) %/%2)+1) %>%
  filter(WEEK>0) %>%
  mutate(WEEK = factor(WEEK))
  


## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: descriptive.R
##
## Purpose of script: Perform descriptive analysis of weather data
## To run this script use the following command: Rscript descriptive.R 
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

#-------------------------------------------------------------------------------------------
#wind speed
#-------------------------------------------------------------------------------------------

# Computing daily average 
vv_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(VV)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average wind speed", y = "Average wind speed (m/s)", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day"))
#vv_daily
ggsave(vv_daily, file = "plots/vv_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 
vv_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(VV)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
 #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Hourly average wind speed", y = "Average wind speed (m/s)", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#vv_hourly
ggsave(vv_hourly, file = "plots/vv_hourly.pdf", width = 8.8879, height = 4.7278)

#-------------------------------------------------------------------------------------------
#External temperature
#-------------------------------------------------------------------------------------------

# Computing daily average 
et_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(TEXT)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average external temperature", y = "Temperature (\u00B0C)", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day")) +
  NULL
#et_daily
ggsave(et_daily, file = "plots/et_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 
et_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(TEXT)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
#theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly average external temperature", y = "Temperature (\u00B0C)", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#et_hourly
ggsave(et_hourly, file = "plots/et_hourly.pdf", width = 8.8879, height = 4.7278)


#-------------------------------------------------------------------------------------------
#External umidity
#-------------------------------------------------------------------------------------------

# Computing daily average 
eu_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(HEXT)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average external umidity", y = "Umidity (%)", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day"))
#eu_daily
ggsave(eu_daily, file = "plots/eu_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 
eu_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(HEXT)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly average external umidity", y = "Umidity (%)", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#eu_hourly
ggsave(eu_hourly, file = "plots/eu_hourly.pdf", width = 8.8879, height = 4.7278)


#-------------------------------------------------------------------------------------------
#LLUVIA
#-------------------------------------------------------------------------------------------

# Computing daily average
lu_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(LLUVIA)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average rain", y = "Precipitation (mm)", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day")) +
  NULL
#lu_daily
ggsave(lu_daily, file = "plots/lu_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 
lu_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(LLUVIA)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly average rain", y = "Precipitation (mm)", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#lu_hourly
ggsave(lu_hourly, file = "plots/lu_hourly.pdf", width = 8.8879, height = 4.7278)

#-------------------------------------------------------------------------------------------
#ITH (Thom)
#-------------------------------------------------------------------------------------------

# Computing daily average 
itht_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(ITH_Thom)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average ITH (Thom)", y = "ITH", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day")) +
  NULL
#itht_daily
ggsave(itht_daily, file = "plots/itht_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 

itht_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(ITH_Thom)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly ITH (Thom)", y = "ITH", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#itht_hourly
ggsave(itht_hourly, file = "plots/itht_hourly.pdf", width = 8.8879, height = 4.7278)

#-------------------------------------------------------------------------------------------
#ITH (Hahn)
#-------------------------------------------------------------------------------------------

# Computing daily average 
ithh_daily <- df_em %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(ITH_Hahn)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average ITH (Hahn)", y = "ITH", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day"))
#ithh_daily
ggsave(ithh_daily, file = "plots/ithh_daily.pdf", width = 8.8879, height = 4.7278)# Computing hourly average 

# Computing hourly average
ithh_hourly <- df_em %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(ITH_Hahn)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly average ITH (Hahn)", y = "ITH", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#itht_hourly
ggsave(ithh_hourly, file = "plots/ithh_hourly.pdf", width = 8.8879, height = 4.7278)


#-------------------------------------------------------------------------------------------
#Temperatura bajo tinglado (indoor?) 
#-------------------------------------------------------------------------------------------

# Computing daily average 
tbt_daily <- df_tbt %>%
  group_by(LUGAR, FECHA) %>%
  summarise(dia_mean = mean(TEMP)) %>%
  ggplot(., aes(FECHA, dia_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Daily average indoor temperature", y = "Temperature (\u00B0C)", x = "Days", color =  "Locality" ) +
  scale_x_date(breaks = date_breaks("2 day")) 
#ithh_daily
ggsave(tbt_daily, file = "plots/tbt_daily.pdf", width = 8.8879, height = 4.7278)

# Computing hourly average 
tbt_hourly <- df_tbt %>%
  group_by(LUGAR, HORA) %>%
  summarise(HORA_mean = mean(TEMP)) %>%
  ggplot(., aes(HORA, HORA_mean, color = LUGAR)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Hourly Indoor temperature", y = "Temperature (\u00B0C)", x = "Hours", color =  "Locality") +
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"))
#itht_hourly
ggsave(tbt_hourly, file = "plots/tbt_hourly.pdf", width = 8.8879, height = 4.7278)


#==============================================================================================
#END
#==============================================================================================

library(httr2)
library(lubridate)
library(dplyr)
library(stringr)
library(future.apply)
library(data.table)

###############################################
#Script para consultar API BCCh 
###############################################

# #para borrar archivos anteriores de la carpeta
#  unlink(list.files(path = "data/", full.names = T))

#seleccionar fecha inicio y fin consulta cartola
fecha_inicio <- floor_date(Sys.Date()-years(2), "year")-days(1)
fecha_fin <- Sys.Date()-1

#Usado para medir tiempo consulta
#tictoc::tic("consulta api")


############## DATOS API BCCh #############

#User y Password
{
  user_api = ""   ##Pedir credenciales propias en portal BCCh
  pass_api = ""
} 

#codigo serie
cod_serie <- "F073.TCO.PRE.Z.D" #Es el código del dolar prom. banc.

#Definimos URL
url <- str_c("https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx?",
                 str_c("user=", user_api, 
                 "&pass=", pass_api, 
                 "&firstdate=", fecha_inicio, 
                 "&lastdate=", fecha_fin,
                 "&timeseries=", cod_serie, 
                 "&function=GetSeries"))

json_data <- rjson::fromJSON(file = url)

usd <- rbindlist(future_lapply(json_data$Series$Obs, as_tibble)) %>%
  janitor::clean_names() %>%
  select(date = c(1), value) %>%
  mutate(value = as.numeric(value),
         value = if_else(value == "NaN", NA, value),
         date = parse_date_time(dmy(date), "ymd"))




#tictoc::toc()


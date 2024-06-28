library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(stringr)
library(tictoc)
library(future.apply)
library(future)


fecha_inicio <- ymd("2020-04-01")-1 #seleccionar fecha inicio consulta USD
fecha_fin <- ymd("2021-06-01") #ymd(Sys.Date()
######### CONSULTA DOLAR DIARIO ############

fechas_fx <- seq(fecha_inicio, 
              fecha_fin,
              by = "days") 

#Fx a usar para consultar usd diario
func_usd <- function(rango_dias){
  
    jsonlite::fromJSON(
    str_c("https://mindicador.cl/api/dolar/", format(rango_dias,"%d-%m-%Y")))$serie
  
 }


#Para paralelizar la consulta y fx
#Función tic() y toc() para medir tiempo 
tic("par_lapply")

plan(multisession)
usd <- rbindlist(future_lapply(fechas_fx, func_usd))

toc()


#formato fecha
usd <- usd %>%
  mutate(date = as.Date(fecha)) %>%
  select(-fecha) 

#guardamos
fwrite(usd, "dataR/usd.csv")
writexl::write_xlsx(usd, "dataR/usd.xlsx")
#Código Antiguo loop usd
{
  # 
  # fechas <- seq(fecha_inicio, 
  #               ymd(Sys.Date()),
  #               by = "days") %>%
  #   format("%d-%m-%Y") 
  # 
  
  # tic("for")
  # 
  # USD <- jsonlite::fromJSON(
  #   paste0("https://mindicador.cl/api/dolar/", fechas[1])
  # )
  # 
  # usd <- USD$serie
  # 
  # cat("\n dolar", fechas[1], "hecho! \n" )
  # 
  # #Rescatamos dolar de mindicador.cl
  # for (dia in fechas[-1]) {
  #   
  #   USD <- jsonlite::fromJSON(
  #     stringr::str_c("https://mindicador.cl/api/dolar/", dia)
  #   )
  #   
  #   usd <- bind_rows(usd,USD$serie) 
  #   cat("USD", dia, "check! \n")
  #   
  # }
  # 
  # toc()
}
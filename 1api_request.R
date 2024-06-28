library(httr2)
library(lubridate)
library(dplyr)
library(stringr)


#Script para consultar API CMF en .csv 

fecha_inicio <- ymd("2019-11-30")-1 #seleccionar fecha inicio consulta cartola

tictoc::tic("api")

############## DATOS CMF #############
#User y Password, no disponible para uso público
{
user_api = "NO DISPONIBLE PARA USO PÚBLICO"
pass_api = " "
} 

fechas <- seq(fecha_inicio, (Sys.Date()-1), by = "days") %>% 
  str_replace_all("-", "")


url <- "https://www.cmfchile.cl/sitio/api/fmcfm/consulta_cartola/"

path_dw <- str_c(getwd(), "/data/")

for (dia in fechas) {
  
  req <- request(str_c(url, dia, "/csv")) %>% 
    req_auth_basic(username = user_api, password = pass_api) %>%
    req_perform(path = str_c(path_dw, dia ,".csv"))
  
  cat(str_c(ymd(dia)," ¡Listo! \n" ))
}


tictoc::toc()
###Estadísticas Macro BCCh Chile

########## instalamos packages en caso de no existir

packages <- c("httr", "rjson", "dplyr", "lubridate", "stringr", "data.table")

# Verifica si cada paquete está instalado y, de no ser así, lo instala
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
      library(pkg, character.only = TRUE)
  }
  
}

##Creamos carpeta datos BCCh

dir.create("BCCh")
dir.create("BCCh/data")


###Revisamos series mensuales en API BCCh
user <- "martindiezg@gmail.com"
pass <- "martin10"

url <- paste0("https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx?user=", 
              user, 
              "&",
              "pass=", 
              pass,
              "&frequency=MONTHLY&function=SearchSeries")

json_data <- rjson::fromJSON(file=url)
lista_series <- rbindlist(lapply(json_data$SeriesInfos, as_tibble)) %>%
  filter(year(dmy(lastObservation)) == 2023) #ultima obs 2023

## nos quedamos con algunas estadísticas mensuales generales


series <- c("G073.IPC.V12.2018.M", #IPC Gral hist., var. mismo período año anterior, 2018
            "F019.EPU.IND.31.M", #Indice Incertidumbre Econ.
            "F021.CIR.STO.R.P96.1.M", #Circulante real desestac. prom mensual
            "F032.PIB.FLU.R.CLP.2018.Z.Z.1.M", #PIB precios año anterior encadenado, desestacional., ref. 2018 (MMM$)
            )
            


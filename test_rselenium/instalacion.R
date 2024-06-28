#Previo a esto, debemos instalar JDK Java Azul Zulu, revisar video
# Esta versión funciona mejor con Tabulizer y RSelenium de R
# https://www.youtube.com/watch?v=GnpJujF9dBw&t=10s
# Fijar PATH de Java


library(tidyverse)
library(wdman) #para settear selenium
library(RSelenium)
library(netstat) ##Para encontrar puertos libres "free port" para Selenium

#Necesitamos asegurarnos que cargamos selenium
# selenium() #del package wdman, descargará archivos

selenium_object <- selenium(retcommand = T, check = F)

#Luego de esto, corremos el objeto selenium y rescatamos la direccion de los 
#drivers:
# "C:\\\\Users\\\\Martin\\\\AppData\\\\Local\\\\binman\\\\
#    binman_chromedriver\\\\win32\\\\114.0.5735.90/chromedriver.exe\\\"
#En mi caso. Luego en las carpetas de binman_chromedriver eliminamos
# los archivos LICENSE.chromedriver, esto debemos hacerlo cada vez que
# se actualize Chrome

binman::list_versions("chromedriver") #Instalar binman para encontrar 
#la versión que necesitamos usar abajo

# google_chrome
remote_drive <- rsDriver(browser = "chrome",
                         chromever = "122.0.6261.57",
                         verbose = F,
                         port = free_port())

##En caso de tener problemas, se puede deber a la versión del chromedriver
## https://www.youtube.com/watch?v=BnY4PZyL9cg&t=7s
## Acá encontramos video-ayuda

# close server
remote_drive$server$stop()

################ ACÁ ABAJO FIREFOX CON RSELENIUM, es más fácil de hacer
## funcionar que con Chrome


# # firefox  #Instalar Firefox Primero
# remote_drive <- rsDriver(browser = "firefox",
#                          chromever = NULL,
#                          verbose = F,
#                          port = free_port())
# 
# remDr <- remote_driver$client
# remDr$open()
# remDr$navigate("http://ebay.com")
# 
# remote_driver$server$stop()
# 




# #######################################################################
# ## Solucionar tema de diferencias chromedriver (versión distinta del Selenium
# ## vs la versión que tenemos instalada)
# 
# chromeCommand <- chrome(retcommand = T, verbose = F, check = F)
# chromeCommand

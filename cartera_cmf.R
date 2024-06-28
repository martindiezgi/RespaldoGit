library(dplyr)
library(stringr)
options(scipen = 9999)

a <- list.files("carteras", full.names = T)

ext <- readr::read_csv2(a[[1]]) %>%
  janitor::clean_names() %>%
  select(run = run_fondo, 
         fondo = nombre_fondo,
         val = ffm_6021200,
         moneda = ffm_6021300,
         ptje = ffm_6021513) 

nac <-  readr::read_csv2(a[[2]]) %>%
  janitor::clean_names() %>%
  select(run = run_fondo, 
         fondo = nombre_fondo,
         val = ffm_6011200,
         moneda = ffm_6011300,
         ptje = ffm_6011513,
         ) 

###Monedas a febrero2024
PROM <- 


df <- nac %>%
  full_join(ext) %>%
  mutate(ptje = as.numeric(ptje)/100) %>%
  
unique(df$moneda)

  
#   group_by(run) %>%
#    %>%
#   reframe(run, fondo,
#           val = sum(val_nac + val_ext),
#           ptje_activos = sum(ptje_a_nac + ptje_a_ext, na.rm = T)/100) %>%
#   unique() %>%
#   mutate(AUM = val/ptje_activos) %>%
#   filter(AUM != "Inf")
# 
# 
# AUM_total <- with(df, sum(AUM)/1e6)


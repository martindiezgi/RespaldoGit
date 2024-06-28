library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
#Investigar dtplyr

df <- readr::read_csv2("bbdd/base_2023_bruta.txt")

# df %>% mutate(D011 = if_else(D011=="Yes",1,0)) %>%
#   summarise(D011_2 = sum(D011)/n(),
#             D011 = mean(D011))

df <- df %>%
  select(id = Encuestado, 
         edad = N10,
         D011, 
         #D012,
         # PA1, #Area 
         starts_with(
           c("CUOTA", "NSE","P0122", "P0148", "P0248", "P0222"))) %>%
  rename(sexo = D011, #yes=hombre, no=mujer
         #mujer = D012, 
         # area = PA1, #yes=urbano, no=rural
         ) %>%
  janitor::clean_names() %>%
  rename_with(~str_replace_all(.,"cuota_rango_","" ), contains("edad")) %>%
  rename_with(~str_replace_all(.,"_reagrupado","" ), contains("nse")) %>%
  mutate(across(c(-1,-2), ~case_when(. == "Yes" ~ 1,
                                . == "No" ~ 0,
                                is.na(.) ~ 0)),
         r_etario = case_when(edad1 == 1 ~ "[18, 25]",
                              edad2 == 1 ~ "[26, 39]",
                              edad3 == 1 ~ "[40, 59]",
                              edad4 == 1 ~ "+60"),
         y_tramo = case_when(nse1 == 1 ~ "Alto (ABC1)",  #ABC1
                             nse2 == 1 ~ "Medio (C2, C3)", #C2, c3
                             nse3 == 1 ~ "Bajo (D1, D2, E)"), #D1, D2, E
         sexo = case_when(sexo == 1 ~ "Hombre",
                          sexo == 0 ~ "Mujer")) %>% 
  select(!contains(c("edad", "nse"))) %>%
  rename("Conoce FF.MM." = p0122,
         "Conoce APV" = p0148,
         "Tiene FF.MM." = p0222,
         "Tiene APV" = p0248)

stats_fm_sexo <- df %>% 
  group_by(Sexo = sexo) %>%
  summarise(across(contains(c("Conoce", "Tiene")), ~mean(.))) %>%
  mutate(across(-1, ~str_c(100*round(.,2), "%")))

stats_fm_edad <- df %>%
  group_by("Tramo Edad" = r_etario) %>%
  summarise(across(contains(c("Conoce", "Tiene")), ~mean(.))) %>%
  mutate(across(-1, ~str_c(100*round(.,2), "%"))) %>%
  arrange(`Tramo Edad`) %>%
  na.omit()

stats_fm_y <- df %>%
  group_by("Tramo Ingreso" = y_tramo) %>%
  summarise(across(contains(c("Conoce", "Tiene")), ~mean(.))) %>%
  mutate(across(-1, ~str_c(100*round(.,2), "%"))) %>%
  na.omit() %>%
  arrange(`Conoce FF.MM.`)
  

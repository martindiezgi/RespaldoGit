library(readr)
library(dplyr)
library(haven)
library(stringr)
library(srvyr)

dir <- "2020/Bases_EPS2020_presencial/01_Vivos presencial/"

fw <-  read_dta("Factores de Expansion EPS 2020/Factor_XS_EPS_PRES_in.dta")

#EPS 2020
eps2020 <- read_dta(paste0(dir, "MODULO_A_Entrevistado_in.dta")) %>%
  left_join(read_dta(paste0(dir, "MODULO_A_Carreras_in.dta"))) %>%
  left_join(read_dta(paste0(dir, "MODULO_B_Historia_Laboral_in.dta"))) %>%
  left_join(read_dta(paste0(dir, "MODULO_C_Entrevistado_in.dta"))) %>%
  left_join(read_dta(paste0(dir, "MODULO_D_Entrevistado_in.dta"))) %>%
  rename(fm = d27_a_6, 
         montofm = d27_m_6_ed, 
         apv = d27_a_2, 
         montoapv = d27_m_2_ed) %>% 
 #definimos variables relevantes
  select(!starts_with(c("d")),
         starts_with("b12"),
         folio_n20, 
         sexo = a8, 
         edad = a9, 
         educ = a12_n_ed, 
         escolaridad = escolaridad_ed,
         fm, 
         montofm) %>%
  mutate(tramo_edad = case_when(edad > 0 & edad < 35 ~ "1) [22, 34]",
                                edad >= 35 & edad <= 49 ~ "2) [35, 49]",
                                edad >= 50 & edad < 65 ~ "3) [50, 64]",
                                edad >= 65 & edad <= 100 ~ "4) [65, 99]"),
         sexo = as.numeric(sexo), # 1 hombre, 2 mujer
         sexo = case_when(sexo == 2 ~ "Mujer",
                          TRUE ~ "Hombre"),
         sexo = factor(sexo),
         montofm = na_if(montofm, 8), 
         montofm = na_if(montofm, 9),
         fmapv = case_when(fm == 1 & apv == 1 ~ "FF.MM. y APV",
                           fm == 1 & apv == 2 ~ "Solo FF.MM.",
                           fm == 2 & apv == 1 ~ "Solo APV",
                           TRUE ~ "Ninguno/NS/NR"),
         fmapv2 = case_when(fm == 1 | apv == 1 ~ "Tiene FF.MM. y/o APV",
                            fm == 2 & apv == 2 ~ "Ninguno",
                            TRUE ~ "NS/NR"),
         fm = case_when(fm == 8 ~ "No responde",
                        fm == 9 ~ "No sabe",
                        fm == 1 ~ "Si",
                        fm == 2 ~ "No"),
         apv = case_when(apv == 8 ~ "No responde",
                        apv == 9 ~ "No sabe",
                        apv == 1 ~ "Si",
                        apv == 2 ~ "No"),
         fm = factor(fm),
         tramo_edad = factor(tramo_edad),
         tramo_esc = case_when(escolaridad == 80 | 
                              (escolaridad >=  0 & 
                                 escolaridad <=  12) ~ "1) [0, 12]",
                               escolaridad >= 13 &
                                 escolaridad <  80 ~ "2) [13, 22]",
                               escolaridad >= 80 ~ "No Tiene/NS/NR"),
         esc_normal = escolaridad,
         tramo_esc = factor(tramo_esc),
         tramo_ingreso = b12_t) %>%
  left_join(fw) #pegamos todas las bases eps relevantes

#Usaremos escolaridad: Promedio de años de estudio efectivamente cursados


######### exploramos EPS 2020
eps2020_s <- as_survey(eps2020, ids = cluster_ID, strata = strata_ID, weights = factor_XS)
options(survey.adjust.domain.lonely=TRUE) #/// Opciones para solucionar 
options(survey.lonely.psu="adjust") #remove or adjust     #///  Primary Sampling Unit = 1
# https://www.practicalsignificance.com/posts/bugs-with-singleton-strata/
# Link de interés

#calculo hombres y mujeres con fact expansion para corroborar
sexo <- eps2020_s %>%
  group_by(sexo) %>%
  summarise(proportion = survey_mean(),
            total = survey_total()) %>%
  mutate(TOT = cumsum(total))
#correcto, documento eps oficial da 48,5% H y 51,5% M

fm_esc <- eps2020_s %>%
  group_by(interact(fm, tramo_esc)) %>%
  summarise(total = survey_total(), pctje = 100*survey_prop()) 

fm_edad <- eps2020_s %>%
  group_by(interact(fm, tramo_edad)) %>%
  summarise(total = survey_total(), pctje = 100*survey_prop()) 

fm_sexo <- eps2020_s %>%
  group_by(interact(fm, sexo)) %>% #interact separa % por total, y no por los grupos per se
  summarise(total = survey_total(), pctje = 100*survey_prop()) 

fmapv <- eps2020_s %>%
  group_by(fmapv) %>%
  summarise(pctje = 100*survey_mean(), total = survey_total())

fmapv2 <- eps2020_s %>%
  group_by(fmapv2) %>%
  summarise(pctje = 100*survey_mean(), total = survey_total())
  
montofm <- eps2020_s %>% ###Mucha desagregación genera problemas de representatividad
  summarise(montofm = survey_mean(montofm, na.rm = T, vartype = c("se")),
            min_monto = unweighted(min(montofm, na.rm = T)),
            max_monto = unweighted(max(montofm, na.rm = T)))

montofm_sexo <- eps2020_s %>%
  group_by(sexo) %>%
  summarise(montofm = survey_mean(montofm, na.rm = T, vartype = c("se")))

montofm_edad <- eps2020_s %>%
  group_by(tramo_edad) %>%
  summarise(montofm = survey_mean(montofm, na.rm = T, vartype = c("se")))

montofm_esc <- eps2020_s %>%
  group_by(tramo_esc) %>%
  summarise(montofm = survey_mean(montofm, na.rm = T, vartype = c("se")))

resultados <- list()

resultados[["FFMM"]] <- fm
resultados[["FFMM-Escolaridad"]] <-  fm_esc
resultados[["FFMM-Edad"]] <- fm_edad
#resultados[["FFMM-Ingreso"]] <- fm_ingreso
resultados[["FFMM-Sexo"]] <- fm_sexo
resultados[["MontoFFMM"]] <-  montofm
resultados[["Monto-Edad"]] <- montofm_edad
resultados[["Monto-Sexo"]] <- montofm_sexo
resultados[["Monto-Escolaridad"]] <- montofm_esc

writexl::write_xlsx(resultados, "DescripStatsFFMM.xlsx")


#La muestra es representativa de la población adulta >= 22 años a nivel nacional.


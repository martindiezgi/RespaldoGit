# Fijar path java y java_home:
# Instalación acá -> https://www.youtube.com/watch?v=nlsWjezvsg8

# Instalamos packages
# install.packages(c("remotes", "rJava"))

# Instalamos tabulizer desde github
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

library(dplyr)
library(tabulizer)

ps <- "premio_salmon.pdf" #nombre archivo

# extrae las tablas del pdf en formato matríz ("method" usado fue ensayo y error)
df <- tabulizer::extract_tables(ps,
                                 method = "stream",
                                 output = c("matrix")) 

# fondos money market tienen problemas de extracción, matrices
# quedan con 7 columnas en vez de 9 y falla el rbind, se extraerán aparte

nomm <- do.call(rbind, df[-c(16:18)]) %>%
  as_tibble(.name_repair = "unique") %>%
  setNames(unlist(.[1,])) %>%
  janitor::clean_names() %>%
  filter(run != "RUN") %>%
  mutate(serie = if_else(run == 9238 & serie == "LARGO", "LARGOPLAZO", serie),
         across((rar12:puntaje), ~as.numeric(.))) %>%
  filter(!is.na(puntaje))


# eliminamos 2 columnas de una de las matrices
mm1 <- as.matrix(df[[16]]) %>%
  .[, c(-7,-8)] 

# ahora que todas las matrices son de la misma dimensión, unimos
mm <- do.call(rbind, df[c(17:18)]) %>%
  rbind(mm1) %>%
  as_tibble(.name_repair = "unique") %>%
  janitor::clean_names() %>%
  arrange(desc(x1)) %>%  
  setNames(unlist(.[1,])) %>%
  janitor::clean_names() %>%
  filter(run != "RUN") %>%
  mutate(across((rar12:puntaje), ~as.numeric(.))) %>%
  filter(!is.na(puntaje)) %>%
  mutate(rar36 = NA,
         pto36 = NA)

psalmon <- rbind(mm, nomm) %>%
  rename(AGF = administradora) %>%
  mutate(run = as.numeric(run),
         serie = stringr::str_replace(serie, "Á", "A"),
         serie = stringr::str_replace(serie, "É", "E"),
         serie = stringr::str_replace(serie, "Í", "I"),
         serie = stringr::str_replace(serie, "Ó", "O"),
         serie = stringr::str_replace(serie, "Ú", "U")) %>%
  writexl::write_xlsx("premio_salmon.xlsx")




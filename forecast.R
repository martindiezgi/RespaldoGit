library(dplyr)
library(lubridate)
library(forecast)
library(readr)
library(ggplot2)
library(afm)
library(tidyr)
library(tibble)
library(data.table)
library(stringr)
library(seasonal)
library(future.apply)
library(fable)

rm(list = ls())

#FUENTE: https://otexts.com/fpp3
##################
##Fecha
fecha <- ymd("2023-09-01") ##Actualizar
fecha_estimacion <- fecha + months(1)
fecha_fin <- ymd("2024-12-01") ##Actualizar
año_filtro <- 2004

##datos 
afm <- fread("D:/Users/Martin Diez/OneDrive/Escritorio/Inf_Mensual_APV/Data/consol.csv")


##############
##Series Industria
rent <- afm %>%
  filter(aum > 0,
         nominal < 60,
         nominal > -0.80,
         year(date) >= año_filtro
  ) %>% 
  group_by(date) %>%
  summarise(r = weighted.mean(nominal/100, aum))    

df <- afm %>%
  group_by(date) %>%
  summarise(participes = sum(participes),
            aum = sum(aum)/1000) %>%
  left_join(rent) %>%
  mutate(fn = (aum - lag(aum)*(1+r))/lag(aum),
         fc = "IND") %>%
  filter(aum > 0,
         year(date) >= año_filtro
         )


ind <- ts(df[c(2:5)], start = c(año_filtro,1), freq=12)



#############
#Separación MM y no MM, categoría CMF y AFM
## Series Money Market 
rent_mm <- afm %>%
  filter(str_detect(svs, "90"),
         aum > 0,
         nominal < 60,
         nominal > -0.80,
         year(date) >= año_filtro
  ) %>% 
  group_by(date) %>%
  summarise(r = weighted.mean(nominal/100, aum))    

df_mm <- afm %>%
  filter(str_detect(svs, "90")) %>%
  group_by(date) %>%
  summarise(participes = sum(participes),
            aum = sum(aum)/1000) %>%
  left_join(rent_mm) %>%
  mutate(fn = (aum - lag(aum)*(1+r))/lag(aum),
         fc = "MMF") %>%
  filter(aum > 0,
         year(date) >= año_filtro
         )


mm <- ts(df_mm[c(2:5)], start = c(año_filtro,1), freq=12)




###################
## Series NO Money Market 
rent_nomm <- afm %>%
  filter(!str_detect(svs, "90"),
         aum > 0,
         nominal < 60,
         nominal > -0.80,
         year(date) >= año_filtro
  ) %>% 
  group_by(date) %>%
  summarise(r = weighted.mean(nominal/100, aum))    

df_nomm <- afm %>%
  filter(!str_detect(svs, "90")) %>%
  group_by(date) %>%
  summarise(participes = sum(participes),
            aum = sum(aum)/1000) %>%
  left_join(rent_nomm) %>%
  mutate(fn = (aum - lag(aum)*(1+r))/lag(aum),
         fc = "NO_MMF") %>%
  filter(aum > 0,
         year(date) >= año_filtro
         )


no_mm <- ts(df_nomm[c(2:5)], start = c(año_filtro,1), freq=12)


#################
###UNIMOS SERIES, Estadística Descriptiva
df_final <- bind_rows(df_mm, df_nomm, df)

rm(afm, df_mm, df_nomm, df, rent, rent_mm, rent_nomm)


df_final %>%
  mutate(fc = case_when(fc == "IND" ~ "Industria",
                        fc == "MMF" ~ "Money Market",
                        fc == "NO_MMF" ~ "No Money Market")) %>%
  ggplot(aes(x=date, y=aum, color=fc))+
  geom_line(size = 1) +
  theme_afm() +
  scale_color_afm() +
  scale_x_date(breaks = scales::pretty_breaks(7))+
  scale_y_continuous(breaks = scales::pretty_breaks(7),
                     labels = scales::label_number(big.mark = "."))+
  labs(x = "Fecha", 
       y = "AUM (MMM$ CLP)",
       color = "") +
  guides(color = guide_legend(
    nrow = 2  # Set the number of rows in the legend
  )) 
ggsave("figuras/evol_aum.png")


df_final %>%
  mutate(fc = case_when(fc == "IND" ~ "Industria",
                        fc == "MMF" ~ "Money Market",
                        fc == "NO_MMF" ~ "No Money Market")) %>%
  ggplot(aes(x=date, y=participes, color=fc))+
  geom_line(size = 1) +
  theme_afm() +
  scale_color_afm() +
  scale_x_date(breaks = scales::pretty_breaks(7))+
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     labels = scales::label_number(big.mark = "."))+
  labs(x = "Fecha", 
       y = "Partícipes",
       color = "") +
  guides(color = guide_legend(
    nrow = 2  # Set the number of rows in the legend
  )) 
ggsave("figuras/evol_p.png")



################################################
###Parámetros Forecast, Gráficos, colores, etc
n_include <- 60 #nro obs mensual incluidas en plot
#n_include2 <- c(2018,1) #para el window autoplot
n_h <- interval(fecha, fecha_fin) %/% months(1) #nro meses forecast
colora <- "#4B384C"
colorp <- "#F0B323"
w <- 700
h <- 400

{
# ind2 <- window(ind_aum, start = 2012, end = c(2022, 12))
# fit.ind2 <- tslm(ind2 ~ trend + season)
# fcast <- forecast(fit.ind2)
# 
# autoplot(window(ind_aum, start = c(2018, 1)))+
#   autolayer(fcast, showgap = FALSE, PI = F)
# 
# ind_fit1 <- meanf(ind2, h=9)
# ind_fit2 <- rwf(ind2, drift = T, h=9)
# ind_fit3 <- snaive(ind2, h=9)

# autoplot(window(ind_aum, start=c(2022,1)))+
#   autolayer(ind_fit1, PI = F, series = "Mean")+
#   autolayer(ind_fit2, PI = F, series = "naive")+
#   autolayer(ind_fit3, PI = F, series = "S.Naive")

#Descomposición Serie
# fit <- ind_aum %>% seas()#x11="")
# autoplot(fit) #+ theme_afm()
# 
# ind_aum %>% Arima(order = c(1,1,1), seasonal = c(1,1,1)) %>% residuals() %>% ggtsdisplay()
}




#Función Forecast

fc <- function(var_interes) {
  
  auto.arima(var_interes, 
             lambda = "auto", 
             biasadj = T, 
             approximation = F, 
             stepwise = F,
             
             
  ) %>%
    forecast(h = n_h, 
             level = 80,  
             bootstrap = T,
             biasadj = T)
  
}

## Estimación Forecast Patrimonio y Partícipes  #############
## Industria, MMF y no MMF

tss_full <- cbind(ind, mm, no_mm) %>%
  as_tibble()  

tss <- tss_full %>%
  select(ends_with("aum") | ends_with("participes")) %>%
  ts(start = c(año_filtro,1), freq = 12)

#Guardamos para ARIMAX
saveRDS(tss_full, file = "rdata/ts_series.rds")
saveRDS(df_final, file = "rdata/df_forecast.rds")

#Estimamos, paralelizando para más rapidez
plan(multisession)
forecasts <- future_lapply(tss, fc, future.seed = TRUE)







#Tabla y gráfico IND AUM
a <- df_final %>% 
  filter(fc == "IND",
         date == fecha) %>%  .[[1,"aum"]]

b_m <-  as.numeric(tail(forecasts[[1]]$mean,1))
b_l <- as.numeric(tail(forecasts[[1]]$lower,1))
b_h <- as.numeric(tail(forecasts[[1]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_ind_aum <- forecasts[[1]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("AUM Estimado $MMM" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/ind_aum.png", width = w, height = h)
plot(forecasts[[1]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección AUM Industria\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "AUM (MMM$)",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()









#Tabla y gráfico MMF AUM
a <- df_final %>% 
  filter(fc == "MMF",
         date == fecha) %>%  .[[1,"aum"]]

b_m <-  as.numeric(tail(forecasts[[2]]$mean,1))
b_l <- as.numeric(tail(forecasts[[2]]$lower,1))
b_h <- as.numeric(tail(forecasts[[2]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_mm_aum <- forecasts[[2]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("AUM Estimado $MMM" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/ind_mm.png", width = w, height = h)
plot(forecasts[[2]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección AUM MMF\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "AUM (MMM$)",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()









#Tabla y gráfico MMF NO AUM
a <- df_final %>% 
  filter(fc == "NO_MMF",
         date == fecha) %>%  .[[1,"aum"]]

b_m <-  as.numeric(tail(forecasts[[3]]$mean,1))
b_l <- as.numeric(tail(forecasts[[3]]$lower,1))
b_h <- as.numeric(tail(forecasts[[3]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_nomm_aum <- forecasts[[3]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("AUM Estimado $MMM" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/ind_no_mm.png", width = w, height = h)
plot(forecasts[[3]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección AUM No MMF\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "AUM (MMM$)",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()










#Tabla y gráfico PP IND
a <- df_final %>% 
  filter(fc == "IND",
         date == fecha) %>%  .[[1,"participes"]]

b_m <-  as.numeric(tail(forecasts[[4]]$mean,1))
b_l <- as.numeric(tail(forecasts[[4]]$lower,1))
b_h <- as.numeric(tail(forecasts[[4]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_ind_p <- forecasts[[4]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("Partícipes Estimados" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/ind_p.png", width = w, height = h)
plot(forecasts[[4]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección Partícipes Industria\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "Partícipes",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()









#Tabla y gráfico PP MMF
a <- df_final %>% 
  filter(fc == "MMF",
         date == fecha) %>%  .[[1,"participes"]]

b_m <-  as.numeric(tail(forecasts[[5]]$mean,1))
b_l <- as.numeric(tail(forecasts[[5]]$lower,1))
b_h <- as.numeric(tail(forecasts[[5]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_mm_p <- forecasts[[5]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("Partícipes Estimados" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/mm_p.png", width = w, height = h)
plot(forecasts[[5]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección Partícipes MMF\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "Partícipes",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()








#Tabla y gráfico PP MMF
a <- df_final %>% 
  filter(fc == "NO_MMF",
         date == fecha) %>%  .[[1,"participes"]]

b_m <-  as.numeric(tail(forecasts[[6]]$mean,1))
b_l <- as.numeric(tail(forecasts[[6]]$lower,1))
b_h <- as.numeric(tail(forecasts[[6]]$upper,1))
crec <- c(pdif(b_m,a[1]), pdif(b_l,a[1]), pdif(b_h,a[1]))

hoy <- c(a[1], "", "")

df_nomm_p <- forecasts[[6]] |>
  as_tibble() |>
  mutate(date = monyear(seq(fecha_estimacion, by = "month", length.out = n_h), abbr = T)) |>
  rename("Partícipes Estimados" = `Point Forecast`,
         Fecha = date,
         "Límite Inferior" = `Lo 80`,
         "Límite Superior" = `Hi 80`) |>
  column_to_rownames(var = "Fecha") |>
  t() |>
  as_tibble(rownames = " ") |>
  select(c(1), `Dic 2023`, `Dic 2024`) |>
  add_column("Crecimiento Estimado\n(%)" = crec ) |>
  add_column("Mes Actual" = hoy )  |>
  select(c(1,5),c(2:4)) |>
  mutate(across(c(2:4), ~ round(as.numeric(.), digits = 0))) %>%
  mutate(across(c(5), ~ round(as.numeric(.), digits = 2)))


#gráfico
png("./figuras/no_mm_p.png", width = w, height = h)
plot(forecasts[[6]], 
     include = n_include, 
     showgap = F, 
     shaded = T,
     shadecols = "#FFDEAD",
     col = colora, 
     fcol = "#FF7518", 
     main = "Proyección Partícipes No MMF\nModelo ARIMA",
     xlab = "Fecha", 
     ylab = "Partícipes",
     lwd = 2,
     flwd = 3,
     flty = 1,
     frame = F)
grid(lty = 2,
     col = "gray",
     lwd = 1)

dev.off()



{
# gráfico MANERA 2
# ggplot2::autoplot(forecasts[[1]], include=2,
#                   ts.colour = colora,
#                   predict.colour = colorp,
#                   ts.connect = TRUE,
#                   conf.int.alpha = 0.15) +
#                   theme_afm() +
#                   scale_y_continuous(
#                   labels = format_afm,
#                   breaks = seq(5000,30000,5000)) +
#                   scale_x_date(breaks = "2 year", date_labels = "%Y") +
#                                labs(x = "Fecha",
#                   y = "AUM Money Market\n(Miles de Millones)") +
#                   theme(plot.title = element_text(hjust = 0.5))
}  

res1 <- checkresiduals(forecasts[[1]])
res2 <- checkresiduals(forecasts[[2]])
res3 <- checkresiduals(forecasts[[3]])
res4 <- checkresiduals(forecasts[[4]])
res5 <- checkresiduals(forecasts[[5]])
res6 <- checkresiduals(forecasts[[6]])

columns <-  c(res1$method, 
              "AUM Industria", 
              "AUM Money Market", 
              "AUM No Money Market", 
              "Partícipes Industria", 
              "Partícipes Money Market", 
              "Partícipes No Money Market")

lb_test = data.frame(matrix(nrow = 2, ncol = length(columns) ))
colnames(lb_test) = columns

lb_test[1,1] <- "Modelo"
lb_test[1,2] <- res1$data.name
lb_test[1,3] <- res2$data.name
lb_test[1,4] <- res3$data.name
lb_test[1,5] <- res4$data.name
lb_test[1,6] <- res5$data.name
lb_test[1,7] <- res6$data.name


lb_test[2,1] <- "P-value"
lb_test[2,2] <- format(res1$p.value, digits = 2, decimal.mark = ".")
lb_test[2,3] <- format(res2$p.value, digits = 2, decimal.mark = ".")
lb_test[2,4] <- format(res3$p.value, digits = 2, decimal.mark = ".")
lb_test[2,5] <- format(res4$p.value, digits = 2, decimal.mark = ".")
lb_test[2,6] <- format(res5$p.value, digits = 2, decimal.mark = ".")
lb_test[2,7] <- format(res6$p.value, digits = 2, decimal.mark = ".")




lb_test_f <- lb_test %>%
  t() %>%
  as_tibble(rownames = "x", .name_repair = "universal") %>%
  janitor::clean_names() %>%
  mutate(x1 = str_remove(x1, "Residuals from "),
         x1 = str_replace(x1, "with", "+")) %>%
  janitor::row_to_names(1) %>%
  mutate(across(c(3), ~ as.numeric(.)))

View(lb_test_f)
  
resumen <- list()

resumen[["Industria AUM"]] <- df_ind_aum
resumen[["Industria Partícipes"]] <- df_ind_p
resumen[["MM AUM"]] <- df_mm_aum
resumen[["MM Partícipes"]] <- df_mm_p
resumen[["No MM AUM"]] <- df_nomm_aum
resumen[["No MM Partícipes"]] <- df_nomm_p
resumen[["Ljung-Box Test"]] <- lb_test_f
writexl::write_xlsx(resumen, "tablas/resultados_arima.xlsx")



{ #graph ggplot format
  # autoplot(window(tss[,"ind.aum"], start = n_include2))+
  #   autolayer(forecasts[[1]], showgap = F, PI = F)+
  #   autolayer(forecasts[[1]]$lower, color = colorp)+
  #   autolayer(forecasts[[1]]$upper, color = colorp)+
  #   theme_afm()
}


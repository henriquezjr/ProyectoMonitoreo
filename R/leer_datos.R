# Lectura y limpieza de los datos de sensores – Chaparano
# CSV separado por ";" y con punto decimal, un registro cada 20 min.

library(dplyr)
library(tidyr)
library(lubridate)

datos <- read.csv2("datos/datos_chaparano.csv", dec = ".") |>
  # Las fechas de medianoche vienen sin hora ("2026-10-27"); truncated = 3 las lee igual
  mutate(fecha = ymd_hms(fecha, truncated = 3, tz = "UTC")) |>
  rename(Sector  = sector,
         temp    = temp.od,
         sal     = sal.psu,
         oxigeno = od.mgl) |>
  select(Sector, fecha, temp, sal, oxigeno) |>
  # Quitar los registros del inicio sin temperatura ni oxígeno (instalación del equipo)
  filter(!(is.na(temp) & is.na(oxigeno))) |>
  arrange(fecha)

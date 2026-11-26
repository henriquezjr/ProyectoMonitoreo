# Lectura y limpieza de los datos de sensores
# CSV separado por ";" y con punto decimal, un registro cada 20 min.

library(dplyr)
library(tidyr)
library(lubridate)

# --- Información de los sectores (una fila por centro) -----------------------
sectores <- read.csv2("datos/sectores.csv", dec = ".") |>
  rename(Sector = sector) |>
  mutate(fecha_instalacion = as.Date(fecha_instalacion),
         # Profundidad con coma decimal para mostrar: "2,5 metros"
         prof_txt = paste0(sub(".", ",", as.character(profundidad), fixed = TRUE), " metros"))

# --- Mediciones ---------------------------------------------------------------
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

# Sectores con datos (para marcar en el mapa los que aún no tienen)
sectores <- sectores |>
  mutate(con_datos = Sector %in% unique(datos$Sector))

library(tidyverse)

# 1. Leer el CSV (separado por ";")
datos <- read_delim("datos/Sensores.Semillas.csv", delim = ";",
                    show_col_types = FALSE) |>
  rename(
    temp       = Temperature.C,
    oxigeno    = Dissolved.Oxygen.mg.l,
    saturacion = Dissolved.Oxygen.Saturation
  ) |>
  mutate(
    fecha  = as.POSIXct(paste(fecha, hora), tz = "UTC"),   # hora del sensor, sin cambio de horario
    sensor = paste(Sector, Tipo, sep = " - ")
  ) |>
  select(-hora) |>
  arrange(sensor, fecha)

# 2. Control de calidad: valores imposibles pasan a NA
datos <- datos |>
  mutate(
    oxigeno    = if_else(oxigeno < 0 | oxigeno > 20, NA_real_, oxigeno),
    saturacion = if_else(saturacion < 0 | saturacion > 250, NA_real_, saturacion),
    temp       = if_else(temp < 0 | temp > 30, NA_real_, temp)
  )

# 3. Recortar las primeras y últimas 2 horas de cada sensor
#    (estabilización al instalar y retiro)
datos <- datos |>
  group_by(sensor) |>
  filter(fecha > min(fecha) + 2 * 3600,
         fecha < max(fecha) - 2 * 3600) |>
  ungroup()

# 4. Promedios horarios para el dashboard (más liviano)
datos_hora <- datos |>
  mutate(fecha = floor_date(fecha, "hour")) |>
  group_by(Sector, Tipo, Profundidad, sensor, fecha) |>
  summarise(
    oxigeno_min = suppressWarnings(min(oxigeno, na.rm = TRUE)),
    oxigeno     = mean(oxigeno, na.rm = TRUE),
    saturacion  = mean(saturacion, na.rm = TRUE),
    temp        = mean(temp, na.rm = TRUE),
    .groups = "drop"
  )

count(datos, sensor)
summary(datos_hora)

# 5. Resumen diario por sensor
datos_dia <- datos |>
  mutate(dia = as.Date(fecha, tz = "UTC")) |>
  group_by(Sector, Tipo, Profundidad, sensor, dia) |>
  summarise(
    n_registros  = sum(!is.na(oxigeno)),
    O2_media     = mean(oxigeno, na.rm = TRUE),
    O2_min       = min(oxigeno, na.rm = TRUE),
    O2_max       = max(oxigeno, na.rm = TRUE),
    sat_media    = mean(saturacion, na.rm = TRUE),
    T_media      = mean(temp, na.rm = TRUE),
    T_min        = min(temp, na.rm = TRUE),
    T_max        = max(temp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(cobertura = round(100 * n_registros / 288)) |>      # 288 registros = día completo
  filter(n_registros > 0)

head(datos_dia)
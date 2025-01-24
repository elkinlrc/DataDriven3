library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(mltools)
library(data.table)
library(tidyr)
library(lubridate)

procesar_dataset <- function(file_path = "datos/epa-http.csv") {
  # Leer y procesar el archivo
  dataset <- suppressWarnings(read_table(
    file_path,
    col_names = FALSE,
    na = c("", "NA", "NULL"),
    col_types = cols(
      X1 = col_character(),
      X2 = col_character(),
      X3 = col_character(),
      X4 = col_character(),
      X5 = col_character(),
      X6 = col_integer(),
      X7 = col_integer()
    )
  ))
  
  # Reemplazar valores NA
  dataset$X6[is.na(dataset$X6)] <- 0
  dataset$X7[is.na(dataset$X7)] <- 0
  
  # Cambiar nombres de columnas
  colnames(dataset) <- c("Site", "Timestamp", "Metodo", "Endpoint", "Protocolo", "Respuesta_http", "Bytes")
  
  # Limpiar columnas
  dataset$Metodo <- str_replace_all(dataset$Metodo, '"', "")
  dataset$Protocolo <- str_replace_all(dataset$Protocolo, '"', "")
  dataset$Timestamp <- str_replace_all(dataset$Timestamp, "\\[|\\]", "")
  # Procesar el Timestamp (separar y convertir)
 
  #no se encontro una el anyo ni el dia entonces se toma por defecto 2025-01
  # Dividir el Timestamp en Día, Hora, Minuto, Segundo
  dataset <- dataset %>%
    mutate(
      Dia = as.numeric(str_split_fixed(Timestamp, ":", 4)[, 1]),
      Hora = as.numeric(str_split_fixed(Timestamp, ":", 4)[, 2]),
      Minuto = as.numeric(str_split_fixed(Timestamp, ":", 4)[, 3]),
      Segundo = as.numeric(str_split_fixed(Timestamp, ":", 4)[, 4])
    )
  
  # Crear la columna FechaHora
  dataset <- dataset %>%
    mutate(
      FechaHora = as.POSIXct("2025-01-01 00:00:00", tz = "UTC") +
        days(Dia - 1) + hours(Hora) + minutes(Minuto) + seconds(Segundo)
    )
  
  
  
  # Crear columna adicional para longitud de Endpoint
  dataset <- dataset %>% mutate(Longitud_Endpoint = str_length(Endpoint))
  
  return(dataset)
}




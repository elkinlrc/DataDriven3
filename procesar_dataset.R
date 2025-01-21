library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(mltools)
library(data.table)

procesar_dataset <- function(file_path = "epa-http.csv") {
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
  dataset$Timestamp <- as.POSIXct(dataset$Timestamp, format = "[%d/%b/%Y:%H:%M:%S %z]", tz = "UTC")
  
  # Crear columna adicional para longitud de Endpoint
  dataset <- dataset %>% mutate(Longitud_Endpoint = str_length(Endpoint))
  
  return(dataset)
}

# Procesar y describir el dataset
dataset <- procesar_dataset()
summary(dataset)

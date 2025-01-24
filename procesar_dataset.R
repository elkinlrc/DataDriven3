library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(mltools)
library(data.table)

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
  #dataset$Timestamp <- as.POSIXct(dataset$Timestamp, format = "[%d/:%H:%M:%S]", tz = "UTC")
  # Crear columna adicional para longitud de Endpoint
  dataset <- dataset %>% mutate(Longitud_Endpoint = str_length(Endpoint))
  
  return(dataset)
}

# Procesar y describir el dataset


dataset <- procesar_dataset()
summary(dataset)

#Crear un grafico en base a respuestas del servidor
# Resumir códigos de estado
Respuesta_summary <- table(dataset$Respuesta_http)
Respuesta_percent <- prop.table(Respuesta_summary) * 100
# Darle color al pastel
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 

# Crear gráfico de pastel
pie_chart <- pie(
  Respuesta_percent,
  labels = paste0(names(Respuesta_percent), " (", round(Respuesta_percent, 1), "%)"),
  main = "Distribución de Códigos Respuestas Http",border="white", col=myPalette
)


#Crear un grafico en base a los Metodos usados
# Resumir códigos de estado
Metodo_summary <- table(dataset$Metodo)
Metodo_percent <- prop.table(Metodo_summary) * 100
# Darle color al pastel
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set3") 

# Crear gráfico de pastel
pie_chart <- pie(
  Metodo_percent,
  labels = paste0(names(Metodo_percent), " (", round(Metodo_percent, 1), "%)"),
  main = "Distribución de Metodos",border="white", col=myPalette
)
# main.R

# Cargar el archivo que contiene la función procesar_dataset
source("procesar_dataset.R")

# Función para descomprimir el archivo
descomprimir_archivo <- function(zip_path, output_dir = "datos") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir) # Crear directorio si no existe
  }
  unzip(zip_path, exdir = output_dir)
  cat("Archivo descomprimido en:", output_dir, "\n")
  
  # Retornar la ruta del archivo extraído
  list.files(output_dir, full.names = TRUE)
}

# Función para describir el dataset
describir_datos <- function(dataset) {
  cat("\n--- Descripción de los datos ---\n")
  cat("1. Fuente: Logs de servidor Apache\n")
  cat("2. Tipología: Peticiones HTTP procesadas por el servidor\n")
  cat("3. Campos:\n")
  cat("   - Site: Dirección IP del cliente\n")
  cat("   - Timestamp: Fecha y hora de la petición\n")
  cat("   - Metodo: Método HTTP usado (GET, POST, etc.)\n")
  cat("   - Endpoint: Recurso solicitado\n")
  cat("   - Protocolo: Versión del protocolo HTTP\n")
  cat("   - Respuesta_http: Código de estado HTTP\n")
  cat("   - Bytes: Tamaño de la respuesta en bytes\n")
  cat("\nValores de ejemplo:\n")
  print(head(dataset))
}

# Ejecutar el proceso principal
main <- function() {
  # Ruta del archivo ZIP
  zip_path <- "epa-http.zip"
  
  # Paso 1: Descomprimir el archivo
  archivos_extraidos <- descomprimir_archivo(zip_path)
  
  # Verificar que se descomprimió un archivo válido
  if (length(archivos_extraidos) > 0) {
    file_path <- archivos_extraidos[1] # Usamos el primer archivo descomprimido
    
    # Paso 2: Procesar el dataset con la función del archivo procesar_dataset.R
    dataset <- procesar_dataset(file_path)
    
    # Paso 3: Describir el dataset
    describir_datos(dataset)
    
    # Retornar el dataset para uso posterior
    return(dataset)
  } else {
    cat("No se encontraron archivos en el ZIP.\n")
    return(NULL)
  }
}

# Llamar a la función principal y guardar el dataset en una variable global
dataset <- main()


explorar_datos <- function(dataset) {
  # Crear una columna para clasificar si la respuesta tiene error
  dataset <- dataset %>%
    mutate(
      Tiene_error = Respuesta_http >= 400,
      Tipo_error = case_when(
        Respuesta_http >= 400 & Respuesta_http < 500 ~ "4xx (Error del cliente)",
        Respuesta_http >= 500 ~ "5xx (Error del servidor)",
        TRUE ~ "Sin error"
      )
    )
  
  # Contar usuarios únicos que han tenido errores o no
  usuarios_por_error <- dataset %>%
    group_by(Tiene_error) %>%
    summarise(Usuarios_unicos = n_distinct(Site)) %>%
    mutate(Descripcion = if_else(Tiene_error, "Con error", "Sin error"))
  
  # Desglose de usuarios según el tipo de error
  usuarios_por_tipo_error <- dataset %>%
    filter(Tiene_error) %>%
    group_by(Tipo_error) %>%
    summarise(Usuarios_unicos = n_distinct(Site))
  
  # Mostrar resultados
  cat("\nNúmero de usuarios únicos según si tuvieron errores:\n")
  print(usuarios_por_error)
  
  cat("\nDesglose de usuarios únicos por tipo de error:\n")
  print(usuarios_por_tipo_error)
  
  # Retornar tablas para su uso posterior
  list(usuarios_por_error = usuarios_por_error, usuarios_por_tipo_error = usuarios_por_tipo_error)
}

# Llamar a la función con el dataset procesado
resultados_exploracion <- explorar_datos(dataset)


analizar_peticiones_http <- function(dataset) {
  # Frecuencia de métodos HTTP en general
  frecuencia_metodos <- dataset %>%
    group_by(Metodo) %>%
    summarise(Frecuencia = n()) %>%
    arrange(desc(Frecuencia))
  
  # Filtrar peticiones que corresponden a imágenes
  extensiones_imagen <- c(".jpg", ".jpeg", ".png", ".gif", ".bmp", ".webp", ".svg")
  dataset_imagenes <- dataset %>%
    filter(str_detect(tolower(Endpoint), paste(extensiones_imagen, collapse = "|")))
  
  # Frecuencia de métodos HTTP para peticiones a imágenes
  frecuencia_metodos_imagenes <- dataset_imagenes %>%
    group_by(Metodo) %>%
    summarise(Frecuencia = n()) %>%
    arrange(desc(Frecuencia))
  
  # Mostrar resultados
  cat("\nFrecuencia de métodos HTTP en general:\n")
  print(frecuencia_metodos)
  
  cat("\nFrecuencia de métodos HTTP para recursos de tipo imagen:\n")
  print(frecuencia_metodos_imagenes)
  
  # Retornar los resultados para uso posterior
  list(
    general = frecuencia_metodos,
    imagenes = frecuencia_metodos_imagenes
  )
}

# Llamar a la función con el dataset procesado
resultados_peticiones <- analizar_peticiones_http(dataset)





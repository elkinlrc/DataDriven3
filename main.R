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

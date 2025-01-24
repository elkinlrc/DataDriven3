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
    # Construir la ruta del archivo descomprimido
    file_path <- "epa-http.csv"  # Nombre del archivo dentro del directorio 'datos'
    dataset <- procesar_dataset(file_path = file.path("datos", file_path))
    
    
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
#para pode trabajar con ella de ahi en adelante
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


#######Ejercicio 6



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

#######Ejercicio 7
# Agrupar por intervalo de tiempo (por ejemplo, cada hora) y contar las peticiones
dataset_FrecFecha <- dataset %>%
  mutate(Hour = floor_date(FechaHora, "hour")) %>%
  group_by(Hour) %>%
  summarise(Count = n())


ggplot(dataset_FrecFecha, aes(x = Hour, y = Count)) +
  geom_line(color = "blue") +
  labs(title = "Número de Peticiones Servidas a lo Largo del Tiempo",
       x = "Fecha y Hora",
       y = "Número de Peticiones") +
  theme_minimal()


#########ejercicio 8
analizar_clustering <- function(dataset, valores_k = c(3, 5)) {
  # Paso 1: Generar la columna Longitud_Endpoint
  dataset <- dataset %>%
    mutate(Longitud_Endpoint = str_length(Endpoint))
  
  # Paso 2: Convertir columnas categóricas a numéricas con one_hot
  dataset_one_hot <- one_hot(as.data.table(dataset), sparsifyNAs = TRUE)
  
  # Paso 3: Seleccionar solo columnas numéricas
  dataset_numerico <- dataset_one_hot %>%
    select_if(is.numeric) %>%
    mutate_all(~replace(., is.na(.), 0)) # Imputar NAs con 0
  
  # Almacenar resultados de clustering
  resultados_kmeans <- list()
  
  # Paso 4: Ejecutar k-means con distintos valores de k
  for (k in valores_k) {
    set.seed(123) # Para reproducibilidad
    modelo_kmeans <- kmeans(dataset_numerico, centers = k, nstart = 10)
    
    # Guardar resultados en el dataset
    dataset_numerico <- dataset_numerico %>%
      mutate(Cluster = modelo_kmeans$cluster)
    
    # Guardar resultados en la lista
    resultados_kmeans[[paste0("k=", k)]] <- list(
      modelo = modelo_kmeans,
      dataset_clusterizado = dataset_numerico
    )
  }
  
  return(resultados_kmeans)
}

# Paso 5: Visualizar resultados de clustering
visualizar_clustering <- function(resultados_kmeans, k) {
  # Extraer dataset clusterizado para el valor de k especificado
  dataset_clusterizado <- resultados_kmeans[[paste0("k=", k)]]$dataset_clusterizado
  
  # Graficar los primeros dos componentes principales para visualización
  ggplot(dataset_clusterizado, aes(x = Longitud_Endpoint, y = Bytes, color = factor(Cluster))) +
    geom_point(alpha = 0.7) +
    labs(
      title = paste("Clustering con k =", k),
      x = "Longitud del Endpoint",
      y = "Bytes Servidos",
      color = "Clúster"
    ) +
    theme_minimal()
}

# Llamar a las funciones con tu dataset procesado
valores_k <- c(3, 5) # Elegir valores para k
resultados_kmeans <- analizar_clustering(dataset, valores_k)

# Visualizar los resultados para k = 3
visualizar_clustering(resultados_kmeans, k = 3)


####ejercicio 9 
# Función para graficar clústeres
graficar_clusters <- function(resultados_kmeans, k, var_x, var_y) {
  # Extraer dataset clusterizado para el valor de k especificado
  dataset_clusterizado <- resultados_kmeans[[paste0("k=", k)]]$dataset_clusterizado
  
  # Crear scatter plot
  ggplot(dataset_clusterizado, aes_string(x = var_x, y = var_y, color = "factor(Cluster)")) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      title = paste("Clustering con k =", k),
      x = var_x,
      y = var_y,
      color = "Clúster"
    ) +
    theme_minimal()
}

# Función para seleccionar ejemplos representativos
mostrar_ejemplos <- function(resultados_kmeans, k, n = 5) {
  # Extraer dataset clusterizado para el valor de k especificado
  dataset_clusterizado <- resultados_kmeans[[paste0("k=", k)]]$dataset_clusterizado
  
  # Seleccionar ejemplos aleatorios por clúster
  ejemplos <- dataset_clusterizado %>%
    group_by(Cluster) %>%
    sample_n(min(n(), n)) %>%
    ungroup()
  
  return(ejemplos)
}

# Generar gráficos para k = 3 y k = 5
graficar_clusters(resultados_kmeans, k = 3, var_x = "Longitud_Endpoint", var_y = "Bytes")
graficar_clusters(resultados_kmeans, k = 5, var_x = "Longitud_Endpoint", var_y = "Bytes")

# Mostrar ejemplos representativos para k = 3
ejemplos_k3 <- mostrar_ejemplos(resultados_kmeans, k = 3, n = 5)
print("Ejemplos representativos para k = 3:")
print(ejemplos_k3)

# Mostrar ejemplos representativos para k = 5
ejemplos_k5 <- mostrar_ejemplos(resultados_kmeans, k = 5, n = 5)
print("Ejemplos representativos para k = 5:")
print(ejemplos_k5)


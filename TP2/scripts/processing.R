########## Procesamiento de texto - Comunicados OEA

# Paquetes
library(tidyverse)
library(tidytext)
library(udpipe)
library(stopwords)
library(here)

message("Iniciando procesamiento de texto")

# Crear carpeta /output si no existe
output_dir <- here("TP2/output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Creando el directorio: ", output_dir)
} else {
  message("El directorio ya existe: ", output_dir)
}


####################################################################
# 1. Leer la tabla generada por scraping_oea.R

message("Leyendo datos scrapeados")

comunicados <- readRDS(here("TP2/data/comunicados_oea.rds"))

message("Comunicados cargados: ", nrow(comunicados))

################# 
# 2. Limpiamos el texto del cuerpo

# Combinamos titulo y cuerpo en un unico texto
# y limpiamos caracteres especiales, saltos de linea y espacios multiples.
comunicados_limpio <- comunicados |>
  mutate(
    texto_completo = str_c(titulo, ". ", cuerpo),
    # Eliminamos saltos de linea, tabulaciones y retornos de carro
    texto_completo = str_replace_all(texto_completo, "[\\r\\n\\t]+", " "),
    # Eliminamos signos de puntuacion
    texto_completo = str_replace_all(texto_completo, "[[:punct:]]", " "),
    # Eliminamos numeros
    texto_completo = str_replace_all(texto_completo, "[[:digit:]]", " "),
    # Eliminamos caracteres especiales (no ASCII)
    texto_completo = str_replace_all(texto_completo, "[^[:alpha:][:space:]]", " "),
    # Eliminamos espacios multiples que quedaron
    texto_completo = str_squish(texto_completo)
  )
#
# 3. Lematizacion con udpipe
# procesa el texto y asigna categoría gramatical (upos) a cada token

message("Cargando modelo de lematizacion en español")

m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

message("Lematizando el corpus del comunicado")

# udpipe_annotate() lematiza y asigna la categoria gramatical (upos)
# a cada palabra del texto. doc_id permite rastrear a que comunicado
# pertenece cada token.
comunicados_lemas <- udpipe_annotate(
  modelo_es,
  x      = comunicados_limpio$texto_completo,
  doc_id = comunicados_limpio$id
) |>
  as.data.frame() |>
  # Convertimos doc_id a entero para poder hacer joins
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

# Agregamos el titulo para identificar cada comunicado
comunicados_lemas <- comunicados_lemas |>
  left_join(
    comunicados_limpio |> select(id, titulo),
    by = "id"
  )

message("Lematizacion finalizada")

###############################################################3
# 5. Filtrar sustantivos, verbos y adjetivos

message("Filtrando sustantivos (NOUN), verbos (VERB) y adjetivos (ADJ)...")

comunicados_lemas <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  # Pasamos todo a minuscula
  mutate(lemma = str_to_lower(lemma))

# 
# 6. Eliminacion de stopwords

# Cargamos stopwords en espanol e ingles (hay citas en ingles
# en algunos comunicados de la org)
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")

stop_words <- tibble(lemma = c(stop_es, stop_en))

comunicados_lemas <- comunicados_lemas |>
  anti_join(stop_words, by = "lemma") |>
  # Eliminamos tokens que sean solo numeros o de menos de 3 caracteres
  filter(
    !str_detect(lemma, "^\\d+$"),
    nchar(lemma) > 2
  )

message("stopwords eliminadas correctamente")

# ==============================================================
# 7. Guardo el resultado en /output

rds_path <- file.path(output_dir, "processed_text.rds")
saveRDS(comunicados_lemas, rds_path)
message("Texto procesado guardado en: ", rds_path)

########## Scraping base OEA

# Paquetes
library(rvest)
library(tidyverse)
library(here)

message("Iniciando scraping de la pagina de la OEA")

# La consigna pide los comunicados de los meses de enero,
# febrero, marzo y abril 2026

url_base <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"
meses <- 1:4

# ==============================================================
# 1. Crear carpeta /data si no existe

data_dir <- here("TP2/data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Creando el directorio: ", data_dir)
} else {
  message("El directorio ya existe: ", data_dir)
}

# ==============================================================
# 2. Funcion para scrapear titulos y links de un mes

# IMPORTANTE: el robots.txt de la OEA indica Crawl-delay: 10,
# por lo que se agrega Sys.sleep(10) entre cada request.

scrapear_mes <- function(mes){
  
  url <- paste0(url_base, "?nMes=", mes, "&nAnio=2026")
  
  message("Scrapeando mes: ", mes)
  
  # Descarga el HTML de la pagina
  html <- read_html(url)
  
  # Extraigo los nodos con selector identificado con SelectorGadget
  titulos_nodos <- html |>
    html_elements(".itemmenulink")
  
  titulos <- titulos_nodos |>
    html_text2() |>
    str_trim()
  
  
  links <- titulos_nodos |>
    html_attr("href")
  
  # Creo la tabla, filtro solo comunicados y convierto links relativos a absolutos.
  # url_absolute() es mas robusto que paste0 manual para construir URLs completas.
  tabla_mes <- tibble(titulo = titulos, link = links) |>
    filter(!is.na(link)) |>
    filter(str_detect(link, "comunicado")) |>
    mutate(link = paste0("https://www.oas.org/es/centro_noticias/", link)) |>
    distinct()
  
  return(tabla_mes)
}

# Itero sobre los 4 meses y llevo los resultados a una sola tabla
tabla_links <- map_dfr(meses, scrapear_mes)

message("Links obtenidos: ", nrow(tabla_links), " comunicados en total")

# ==============================================================
# 3. Funcion para scrapear el cuerpo de cada comunicado

scrapear_cuerpo <- function(link) {
  
  message("Scrapeando comunicado: ", link)
  
  html_comunicado <- read_html(link)
  
  # Selector "#rightmaincol p": busca los <p> DENTRO de #rightmaincol.
  cuerpo <- html_comunicado |>
    html_elements("#rightmaincol") |>
    html_elements("p")|>
    html_text2() |>
    str_squish() |>       # elimina espacios multiples internos
    str_subset(".+") |>   # descarta parrafos vacios
    str_c(collapse = " ") # colapsa en un unico string
  
  # Si no se encontro contenido devuelvo NA para no romper map_chr
  if (length(cuerpo) == 0 || cuerpo == "") cuerpo <- NA_character_
  
  return(cuerpo)
}

#
# 4. Agrego el cuerpo y construyo la tabla final con id, titulo, cuerpo

message("Iniciando scraping de los cuerpos de los comunicados...")

tabla_comunicados <- tabla_links |>
  mutate(cuerpo = map_chr(link, scrapear_cuerpo)) |>
  mutate(id = row_number()) |>
  select(id, titulo, cuerpo)

message("Scraping finalizado. Total de comunicados: ", nrow(tabla_comunicados))


# 5. Guardar la tabla la carpeta data

rds_path <- file.path(data_dir, "comunicados_oea.rds")
saveRDS(tabla_comunicados, rds_path)
message("Tabla guardada en: ", rds_path)


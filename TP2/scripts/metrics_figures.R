########## Metricas y figuras - Comunicados OEA

# Paquetes
library(tidyverse)
library(tidytext)
library(here)

message("Iniciando calculo de metricas y generacion de figuras")

# ==============================================================
# 1. Leer el texto procesado generado por processing.R

message("Leyendo texto procesado...")

comunicados_lemas <- readRDS(here("TP2/output/processed_text.rds"))

# ==============================================================
# 2. Matriz de Frecuencia de Terminos (DTM)

# Contamos cuantas veces aparece cada lemma por documento
# y calculamos el TF-IDF con bind_tf_idf() de tidytext
comunicados_tfidf <- comunicados_lemas |>
  count(id, titulo, lemma) |>
  bind_tf_idf(lemma, id, n)

message("DTM calculada")

# ==============================================================
# 3. Frecuencia total de 5 terminos seleccionados

# Elegimos 5 terminos relevantes para el contexto institucional de la OEA.
# VEo cuales son los terminos más mencionados de los comunicados
comunicados_lemas |> count(lemma, sort = TRUE) |> head(20)

# Seleccion  5 terminos que me interesan
terminos_seleccionados <- c("democrático", "derecho", "misión", "persona", "país")

# Filtramos la DTM para quedarnos solo con esos terminos
# y sumamos la frecuencia total a lo largo de todos los comunicados
frecuencia_terminos <- comunicados_tfidf |>
  filter(lemma %in% terminos_seleccionados) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n)) |>
  arrange(desc(frecuencia_total))

message("Frecuencia de terminos calculada")
print(frecuencia_terminos)

# 
# 4. Creo un grafico de barras con ggplot2

ggplot(frecuencia_terminos, aes(x = frecuencia_total,
                                y = reorder(lemma, frecuencia_total))) +
  geom_col(fill = "#2171B5", alpha = 0.8) +
  labs(
    title = "Frecuencia de terminos seleccionados",
    subtitle = "Comunicados de prensa OEA - Enero a Abril 2026",
    x = "Frecuencia total",
    y = NULL,
    caption = "Fuente: oas.org"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Guardamos la figura en /output
output_dir <- here("TP2/output")

ggsave(
  filename = file.path(output_dir, "frecuencia_terminos.png"),
  width  = 8,
  height = 5,
  dpi    = 300
)

message("Figura guardada en: ", file.path(output_dir, "frecuencia_terminos.png"))

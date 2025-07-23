# Forzar una versión compatible de Matrix (si se ejecuta localmente)
if (packageVersion("Matrix") > "1.5.4") {
  warning("Matrix > 1.5.4 puede requerir R >= 4.4.0. Considera hacer downgrade.")
  # Puedes descomentar la línea de abajo si estás en entorno local
  # remotes::install_version("Matrix", version = "1.5-4", repos = "http://cran.us.r-project.org")
}

library(shiny)
library(dplyr)
library(tidyr)
library(fmsb)
library(ggplot2)
library(ggtext)
library(readxl)
library(leaflet)
library(sf)


# Ruta de los archivos
data_path <- "Data"

# Cargar base
excel_path<- file.path(data_path, "datos_indice.xlsx")
datos <- read_excel(excel_path)
forma <- file.path(data_path, "00ent.shp")

estados_shp <- st_read(forma, options = "UTF-8") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))

DOMINIOS <- c("General", "Laboral", "Económico", "Humano", "Salud", "Vivienda", "Seguridad",
              "Gobierno", "Discriminación", "Tecnología")

# Radar: valores máximo y mínimo
RADAR_MAX <- as.data.frame(t(rep(1, length(DOMINIOS))))
RADAR_MIN <- as.data.frame(t(rep(0, length(DOMINIOS))))
colnames(RADAR_MAX) <- colnames(RADAR_MIN) <- DOMINIOS

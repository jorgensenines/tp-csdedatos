library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)

source("utils/es_outlier_iqr.R")
source("utils/guardar_tabla.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

base_latam <- readRDS("input/base_latam.rds")

# Analizamos valores faltantes
faltantes_variable <- base_latam %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(porcentaje = round(100 * n_faltantes / nrow(base_latam), 2)) %>%
  arrange(desc(n_faltantes))
faltantes_variable

# ¿Dónde se concentran los NA? (por país y por año)
vars_analisis <- c("valor_exportacion", "rca_exportacion",
                   "indice_complejidad_producto", "distancia", "ganancia_oportunidad")

base_latam %>%
  group_by(codigo_iso3_pais) %>%
  summarise(across(all_of(vars_analisis), ~ sum(is.na(.x)))) %>%
  filter(if_any(all_of(vars_analisis), ~ .x > 0))

base_latam %>%
  group_by(anio) %>%
  summarise(across(all_of(vars_analisis), ~ sum(is.na(.x)))) %>%
  filter(if_any(all_of(vars_analisis), ~ .x > 0))

# ¿Están los 19 países en los 55 años?
anios_por_pais <- base_latam %>%
  group_by(codigo_iso3_pais) %>%
  summarise(primer_anio = min(anio), ultimo_anio = max(anio),
            n_anios = n_distinct(anio))
anios_por_pais

# Combinaciones que no aparecen
n_paises    <- n_distinct(base_latam$codigo_iso3_pais)
n_productos <- n_distinct(base_latam$codigo_sitc_producto)
n_anios     <- n_distinct(base_latam$anio)

cat("Filas esperadas en un panel completo:", n_paises * n_productos * n_anios, "\n")
cat("Filas observadas:", nrow(base_latam), "\n")

combinaciones_ausentes <- base_latam %>%
  count(codigo_iso3_pais, anio, name = "productos_registrados") %>%
  mutate(productos_ausentes = n_productos - productos_registrados)

# Gráfico: cuántos productos no aparecen por país y año?

grafico_ausentes <- ggplot(combinaciones_ausentes,
       aes(x = anio, y = codigo_iso3_pais, fill = productos_ausentes)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#b2182b") +
  labs(title = "Productos sin registro por país y año",
       x = "Año", y = NULL, fill = "Productos\nausentes")

grafico_ausentes
ggsave("output/graficos/productos_ausentes.png", plot = grafico_ausentes,
       width = 10, height = 7, dpi = 300, bg = "white")

# Reportamos los valores que son cero
base_latam %>%
  summarise(
    expo_cero = sum(valor_exportacion == 0, na.rm = TRUE),
    pct_expo_cero = round(100 * mean(valor_exportacion == 0, na.rm = TRUE), 1),
    rca_cero = sum(rca_exportacion == 0, na.rm = TRUE),
    pct_rca_cero = round(100 * mean(rca_exportacion == 0, na.rm = TRUE), 1)
  )

#  Parte 1: RCA · cambiamos escala de IQR
base_latam %>%
  summarise(pct_outliers_escala_original =
              round(100 * mean(es_outlier_iqr(rca_exportacion), na.rm = TRUE), 1))

#  Parte 2: RCA normalizado: RCA_n = (RCA − 1) / (RCA + 1): va de −1 a 1 y el 0 equivale a RCA = 1.
rca_norm <- base_latam %>%
  mutate(outlier = es_outlier_iqr(rca_normalizado))

rca_norm %>%
  summarise(n_registros     = n(),
            mediana         = round(median(rca_normalizado, na.rm = TRUE), 3),
            pct_con_ventaja = round(100 * mean(rca_normalizado >= 0, na.rm = TRUE), 1),
            n_outliers      = sum(outlier, na.rm = TRUE),
            pct_outliers    = round(100 * mean(outlier, na.rm = TRUE), 2),
            outliers_altos  = sum(outlier & rca_normalizado > 0, na.rm = TRUE))

#  Parte 3: RCA · ¿cuáles son los casos más extremos?
rca_norm %>%
  arrange(desc(rca_exportacion)) %>%
  select(codigo_iso3_pais, codigo_sitc_producto, anio,
         rca_exportacion, rca_normalizado, valor_exportacion, cuota_mercado_global) %>%
  head(15)

# ¿En qué países se concentran los outliers altos?
rca_norm %>%
  filter(outlier, rca_normalizado > 0) %>%
  count(codigo_iso3_pais, sort = TRUE)

#  Parte 4: PCI (2024, un valor por producto)
pci_producto <- base_latam %>%
  filter(anio == 2024) %>%
  distinct(codigo_sitc_producto, .keep_all = TRUE) %>%
  select(codigo_sitc_producto, indice_complejidad_producto) %>%
  mutate(outlier = es_outlier_iqr(indice_complejidad_producto))

sum(pci_producto$outlier)
pci_producto %>% filter(outlier) %>% arrange(indice_complejidad_producto)

#  Parte 5: Distancia (2024)
distancia_2024 <- base_latam %>%
  filter(anio == 2024, !is.na(distancia)) %>%
  mutate(outlier = es_outlier_iqr(distancia))

sum(distancia_2024$outlier)
distancia_2024 %>%
  filter(outlier) %>%
  count(codigo_iso3_pais, sort = TRUE)

# ¿Qué países concentran los casos?
distancia_2024 %>%
  filter(outlier) %>%
  arrange(distancia) %>%
  select(codigo_iso3_pais, codigo_sitc_producto, distancia, rca_exportacion) %>%
  head(10)

# Parte 2: tablas de la sección 2 (faltantes y outliers)
guardar_tabla(faltantes_variable, "s2_faltantes_por_variable")

rca_extremos <- rca_norm %>%
  arrange(desc(rca_exportacion)) %>%
  select(pais = codigo_iso3_pais, producto = codigo_sitc_producto, anio,
         rca = rca_exportacion, rca_normalizado, valor_exportado = valor_exportacion,
         cuota_mundial = cuota_mercado_global) %>%
  mutate(rca = round(rca, 1), rca_normalizado = round(rca_normalizado, 4),
         cuota_mundial = round(cuota_mundial, 3)) %>%
  head(15)
guardar_tabla(rca_extremos, "s2_rca_casos_extremos")

outliers_rca_pais <- rca_norm %>%
  filter(outlier, rca_normalizado > 0) %>%
  count(pais = codigo_iso3_pais, name = "outliers_altos", sort = TRUE)
guardar_tabla(outliers_rca_pais, "s2_outliers_rca_por_pais")

outliers_pci <- pci_producto %>%
  filter(outlier) %>%
  arrange(indice_complejidad_producto) %>%
  transmute(producto = codigo_sitc_producto, pci = round(indice_complejidad_producto, 2))
guardar_tabla(outliers_pci, "s2_outliers_pci")

outliers_distancia_pais <- distancia_2024 %>%
  filter(outlier) %>%
  count(pais = codigo_iso3_pais, name = "outliers", sort = TRUE)
guardar_tabla(outliers_distancia_pais, "s2_outliers_distancia_por_pais")

saveRDS(list(combinaciones_ausentes = combinaciones_ausentes, n_productos = n_productos),
        "input/resultados_faltantes.rds")

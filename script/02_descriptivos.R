library(dplyr)
library(ggplot2)

source("utils/guardar_grafico.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

base_latam        <- readRDS("input/base_latam.rds")
datos_2024_mundo  <- readRDS("input/datos_2024_mundo.rds")

tabla_paises <- base_latam %>%
  count(codigo_iso3_pais, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

tabla_paises
g_registros_pais <- ggplot(tabla_paises, aes(x = reorder(codigo_iso3_pais, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Registros por país", x = "País (ISO3)", y = "Cantidad de registros")
guardar_grafico(g_registros_pais, "s1_registros_por_pais")

set.seed(123)

muestra_comercio <- base_latam %>%
  filter(valor_exportacion > 0, valor_importacion > 0) %>%
  slice_sample(n = 5000)
head(muestra_comercio)
g_expo_impo <- ggplot(muestra_comercio, aes(x = valor_exportacion, y = valor_importacion)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Exportaciones vs. importaciones (muestra de 5.000 registros)",
       x = "Valor de exportación (USD, escala log)",
       y = "Valor de importación (USD, escala log)")
guardar_grafico(g_expo_impo, "s1_exportaciones_vs_importaciones")

base_latam %>%
  summarise(
    media   = mean(rca_exportacion, na.rm = TRUE),
    mediana = median(rca_exportacion, na.rm = TRUE),
    desvio  = sd(rca_exportacion, na.rm = TRUE),
    iqr     = IQR(rca_exportacion, na.rm = TRUE),
    minimo  = min(rca_exportacion, na.rm = TRUE),
    maximo  = max(rca_exportacion, na.rm = TRUE)
  )
g_rca <- base_latam %>%
  filter(rca_exportacion > 0) %>%
  ggplot(aes(x = rca_exportacion)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Distribución del RCA de exportación (escala log)",
       x = "RCA de exportación (escala log)",
       y = "Cantidad de registros")
guardar_grafico(g_rca, "s1_distribucion_rca")

top_productos <- base_latam %>%
  filter(anio == 2024, rca_exportacion > 1) %>%
  group_by(codigo_sitc_producto) %>%
  summarise(
    valor_total = sum(valor_exportacion, na.rm = TRUE),
    n_paises = n_distinct(codigo_iso3_pais)
  ) %>%
  arrange(desc(valor_total)) %>%
  head(10)

top_productos

pci_producto <- base_latam %>%
  filter(anio == 2024) %>%
  distinct(codigo_sitc_producto, .keep_all = TRUE) %>%
  select(codigo_sitc_producto, indice_complejidad_producto)

nrow(pci_producto)

pci_producto %>%
  summarise(
    media   = mean(indice_complejidad_producto, na.rm = TRUE),
    mediana = median(indice_complejidad_producto, na.rm = TRUE),
    desvio  = sd(indice_complejidad_producto, na.rm = TRUE),
    iqr     = IQR(indice_complejidad_producto, na.rm = TRUE),
    minimo  = min(indice_complejidad_producto, na.rm = TRUE),
    maximo  = max(indice_complejidad_producto, na.rm = TRUE)
  )

# Histograma de cantidad de productos y PCI
g_pci_hist <- ggplot(pci_producto, aes(x = indice_complejidad_producto)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución del PCI de los productos (2024)",
       x = "Índice de complejidad del producto (PCI)",
       y = "Cantidad de productos")

# Boxplot de PCI
g_pci_box <- ggplot(pci_producto, aes(x = indice_complejidad_producto)) +
  geom_boxplot() +
  labs(title = "Boxplot del PCI (2024)",
       x = "Índice de complejidad del producto (PCI)")
guardar_grafico(g_pci_hist, "s1_pci_histograma")
guardar_grafico(g_pci_box, "s1_pci_boxplot")

pci_producto %>%
  arrange(indice_complejidad_producto) %>%
  head(10)

pci_producto %>%
  arrange(desc(indice_complejidad_producto)) %>%
  head(10)

top_pci_codigos <- pci_producto %>%
  arrange(desc(indice_complejidad_producto)) %>%
  head(10) %>%
  pull(codigo_sitc_producto)

base_latam %>%
  filter(anio == 2024, codigo_sitc_producto %in% top_pci_codigos) %>%
  group_by(codigo_sitc_producto) %>%
  summarise(
    valor_region   = sum(valor_exportacion, na.rm = TRUE),
    paises_con_rca = sum(rca_exportacion > 1, na.rm = TRUE)
  ) %>%
  arrange(desc(valor_region))

base_latam %>%
  filter(anio == 2024) %>%
  summarise(
    media   = mean(distancia, na.rm = TRUE),
    mediana = median(distancia, na.rm = TRUE),
    desvio  = sd(distancia, na.rm = TRUE),
    iqr     = IQR(distancia, na.rm = TRUE),
    minimo  = min(distancia, na.rm = TRUE),
    maximo  = max(distancia, na.rm = TRUE)
  )

datos_2024 <- base_latam %>% filter(anio == 2024)
nrow(datos_2024)

# Histograma de registros de productos
g_dist_hist <- ggplot(datos_2024, aes(x = distancia)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución de la distancia (2024)",
       x = "Distancia", y = "Cantidad de registros")

# Boxplot de distancia
g_dist_box <- ggplot(datos_2024, aes(x = distancia)) +
  geom_boxplot() +
  labs(title = "Boxplot de la distancia (2024)", x = "Distancia")
guardar_grafico(g_dist_hist, "s1_distancia_histograma")
guardar_grafico(g_dist_box, "s1_distancia_boxplot")

# Mediana de LATAM
datos_2024 %>%
  summarise(mediana_region = median(distancia, na.rm = TRUE))

# Mediana a nivel global
datos_2024_mundo %>%
  filter(anio == 2024) %>%
  summarise(mediana_mundo = median(distancia, na.rm = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gridExtra)

source("utils/v_test.R")
source("utils/guardar_tabla.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

base_latam     <- readRDS("input/base_latam.rds")
nombres_paises <- readRDS("input/nombres_paises.rds")

# Sección 4 · Cluster de países según su estructura de RCA por período
# Método: HCPC = componentes principales + cluster
# jerárquico (Ward.D2) + caracterización de grupos con v-test.

secciones_sitc <- c(
 sec_0 = "Alimentos", sec_1 = "Bebidas y tabaco", sec_2 = "Materias primas",
 sec_3 = "Combustibles", sec_4 = "Aceites y grasas", sec_5 = "Químicos",
 sec_6 = "Manuf. por material", sec_7 = "Maquinaria y transporte",
 sec_8 = "Manuf. diversas", sec_9 = "Otros (oro, transacc. especiales)",
 diversidad = "Diversidad (n° productos RCA ≥ 1)",
 pci_especializacion = "PCI de la especialización"
)

#  Parte 2: períodos y sección SITC
base_p2 <- base_latam %>%
 mutate(
 periodo = cut(anio,
 breaks = c(1969, 1979, 1989, 1999, 2009, 2019, 2024),
 labels = c("1970-79", "1980-89", "1990-99",
 "2000-09", "2010-19", "2020-24")),
 seccion = substr(codigo_sitc_producto, 1, 1)
 )

#  Parte 3: promedio por país–período–producto
pp <- base_p2 %>%
 group_by(codigo_iso3_pais, periodo, codigo_sitc_producto, seccion) %>%
 summarise(
 rca = mean(rca_normalizado, na.rm = TRUE),
 expo = sum(valor_exportacion, na.rm = TRUE),
 pci = mean(indice_complejidad_producto, na.rm = TRUE),
 .groups = "drop"
 )

#  Parte 4: variables del cluster (una fila por país–período)
# (a) De lo que el país exporta con ventaja (RCA normalizado ≥ 0), qué proporción
estructura <- pp %>%
 filter(rca >= 0) %>%
 group_by(codigo_iso3_pais, periodo, seccion) %>%
 summarise(expo = sum(expo), .groups = "drop") %>%
 group_by(codigo_iso3_pais, periodo) %>%
 mutate(share = expo / sum(expo)) %>%
 ungroup() %>%
 select(-expo) %>%
 pivot_wider(names_from = seccion, values_from = share,
 names_prefix = "sec_", values_fill = 0)

# (b) Diversidad y complejidad de la especialización
resumen <- pp %>%
 filter(rca >= 0, !is.na(pci)) %>%
 group_by(codigo_iso3_pais, periodo) %>%
 summarise(diversidad = n(),
 pci_especializacion = weighted.mean(pci, expo),
 .groups = "drop")

datos_cluster <- estructura %>%
 inner_join(resumen, by = c("codigo_iso3_pais", "periodo")) %>%
 na.omit() %>%
 arrange(codigo_iso3_pais, periodo)

nrow(datos_cluster)

# Parte 5: matriz estandarizada
mat <- datos_cluster %>%
 select(starts_with("sec_"), diversidad, pci_especializacion) %>%
 as.data.frame()
rownames(mat) <- paste(datos_cluster$codigo_iso3_pais, datos_cluster$periodo)
mat <- mat[, sapply(mat, sd) > 0]
X <- scale(mat)

# Parte 6: componentes principales
pca <- prcomp(X)
var_exp <- pca$sdev^2 / sum(pca$sdev^2)

tabla_pca <- data.frame(
 componente = paste0("CP", seq_along(var_exp)),
 autovalor = round(pca$sdev^2, 2),
 pct_varianza = round(100 * var_exp, 1),
 pct_acumulado = round(100 * cumsum(var_exp), 1)
)
tabla_pca

# Nos quedamos con los componentes que acumulan al menos 80% de la varianza
ncp <- which(cumsum(var_exp) >= 0.80)[1]
ncp

# Cargas: qué variables pesan en cada componente
round(pca$rotation[, 1:min(ncp, 4)], 2)

# Parte 7: cluster jerárquico (Ward.D2) sobre los componentes
set.seed(2026)
coords <- pca$x[, 1:ncp]
hc <- hclust(dist(coords, method = "euclidean"), method = "ward.D2")

# Saltos de inercia: cada barra es el "costo" de pasar de k a k-1 grupos.
# Conviene cortar en el k cuya barra es alta y la siguiente (k+1) es baja.
alturas <- rev(hc$height)[1:9]

barplot(alturas, names.arg = 2:10,
        main = "Saltos de inercia (Ward.D2)",
        xlab = "Cantidad de clusters (k)", ylab = "Altura de la fusión")

png("output/graficos/saltos_inercia.png", width = 8, height = 5, units = "in", res = 300)
barplot(alturas, names.arg = 2:10,
        main = "Saltos de inercia (Ward.D2)",
        xlab = "Cantidad de clusters (k)", ylab = "Altura de la fusión")
dev.off()

# Parte 8: corte del dendrograma
k <- 4
datos_cluster$cluster <- factor(cutree(hc, k = k))
table(datos_cluster$cluster)

plot(hc, labels = rownames(X), cex = 0.4, hang = -1,
     main = "Dendrograma (Ward.D2)", xlab = "", ylab = "Altura", sub = "")
rect.hclust(hc, k = k, border = 2:(k + 1))

png("output/graficos/dendrograma.png", width = 14, height = 7, units = "in", res = 300)
plot(hc, labels = rownames(X), cex = 0.5, hang = -1,
     main = "Dendrograma (Ward.D2)", xlab = "", ylab = "Altura", sub = "")
rect.hclust(hc, k = k, border = 2:(k + 1))
dev.off()

colores_cluster <- c("1" = "#66a61e",   # verde: agroalimentarios
                     "2" = "#bf812d",   # Naranja: soja y minería
                     "3" = "#8c8c8c",   # gris: petroleros
                     "4" = "#2171b5")   # azul: manufactureros

nombres_cluster <- c("1" = "1 · Agroindustria y minería", "2" = "2 · Alimentos tropicales",
                     "3" = "3 · Petroleros",              "4" = "4 · Manufactureros")

# Parte 8b: estadísticas de los grupos
# Distancia de cada país–década al centro de su grupo:
# los más cercanos son los más representativos
centros <- apply(coords, 2, function(columna) tapply(columna, datos_cluster$cluster, mean))
datos_cluster$dist_centro <- sqrt(rowSums((coords - centros[as.character(datos_cluster$cluster), ])^2))

# Exportaciones totales por país–década
expo_pp <- pp %>%
  group_by(codigo_iso3_pais, periodo) %>%
  summarise(expo_total = sum(expo), .groups = "drop")

resumen_grupos <- datos_cluster %>%
  group_by(cluster) %>%
  summarise(observaciones    = n(),
            paises_distintos = n_distinct(codigo_iso3_pais),
            diversidad_media = round(mean(diversidad)),
            pci_medio        = round(mean(pci_especializacion), 2),
            .groups = "drop")

mas_representativos <- datos_cluster %>%
  group_by(cluster) %>%
  slice_min(dist_centro, n = 3, with_ties = FALSE) %>%
  summarise(mas_representativos = paste(unname(nombres_paises[codigo_iso3_pais]), periodo, collapse = "; "),
            .groups = "drop")

paises_2020 <- datos_cluster %>%
  filter(periodo == "2020-24") %>%
  group_by(cluster) %>%
  summarise(paises_2020_24 = paste(sort(unname(nombres_paises[codigo_iso3_pais])), collapse = ", "),
            .groups = "drop")

mayores_2020 <- datos_cluster %>%
  filter(periodo == "2020-24") %>%
  left_join(expo_pp, by = c("codigo_iso3_pais", "periodo")) %>%
  group_by(cluster) %>%
  slice_max(expo_total, n = 2, with_ties = FALSE) %>%
  summarise(mayores_exportadores_2020_24 = paste(unname(nombres_paises[codigo_iso3_pais]), collapse = ", "),
            .groups = "drop")

tabla_grupos <- resumen_grupos %>%
  left_join(mas_representativos, by = "cluster") %>%
  left_join(paises_2020,         by = "cluster") %>%
  left_join(mayores_2020,        by = "cluster") %>%
  mutate(cluster = unname(nombres_cluster[as.character(cluster)])) %>%
  rename(perfil = cluster)
tabla_grupos

# Parte 9: caracterización de los clusters (medias y v-test)
# v-test: compara la media de la variable en el cluster con la media general.

tabla_vtest <- sapply(colnames(mat), function(v) v_test(mat[[v]], datos_cluster$cluster))
tabla_vtest <- round(t(tabla_vtest), 2)
rownames(tabla_vtest) <- secciones_sitc[rownames(tabla_vtest)]
tabla_vtest

medias_cluster <- datos_cluster %>%
 group_by(cluster) %>%
 summarise(n = n(),
 across(c(starts_with("sec_"), diversidad, pci_especializacion),
 ~ round(mean(.x), 2)))
medias_cluster

# Mapa de calor de la composición de cada cluster

grafico_mapa_calor <- medias_cluster %>%
  select(cluster, starts_with("sec_")) %>%
  pivot_longer(-cluster, names_to = "seccion", values_to = "share") %>%
  mutate(seccion = secciones_sitc[seccion]) %>%
  ggplot(aes(x = cluster, y = seccion, fill = share)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), size = 3) +
  scale_fill_gradient(low = "white", high = "#1f6f78", labels = scales::percent) +
  scale_x_discrete(labels = nombres_cluster) +
  labs(title = "Composición de las exportaciones con ventaja comparativa, por cluster",
       x = "Cluster", y = NULL, fill = "Proporción")

grafico_mapa_calor
ggsave("output/graficos/mapa_calor_clusters.png", plot = grafico_mapa_calor,
       width = 10, height = 7, dpi = 300, bg = "white")

# Parte 10: mapa de los dos primeros componentes

grafico_plano_cp <- data.frame(CP1 = pca$x[, 1], CP2 = pca$x[, 2],
                               cluster = datos_cluster$cluster, etiqueta = rownames(X)) %>%
  ggplot(aes(CP1, CP2, color = cluster, label = etiqueta)) +
  geom_point() +
  geom_text(size = 2, vjust = -0.6, show.legend = FALSE) +
  scale_color_manual(values = colores_cluster, labels = nombres_cluster) +
  labs(title = "Países–período en el plano de los dos primeros componentes",
       x = paste0("CP1 (", round(100 * var_exp[1], 1), "%)"),
       y = paste0("CP2 (", round(100 * var_exp[2], 1), "%)"),
       color = "Cluster")

grafico_plano_cp
ggsave("output/graficos/plano_componentes.png", plot = grafico_plano_cp,
       width = 12, height = 8, dpi = 300, bg = "white")

# Parte 11: ¿los países cambian de grupo?
grafico_trayectorias <- datos_cluster %>%
  mutate(pais = unname(nombres_paises[codigo_iso3_pais])) %>%
  ggplot(aes(x = periodo, y = reorder(pais, desc(pais)), fill = cluster)) +
  geom_tile(color = "white") +
  geom_text(aes(label = cluster), size = 3) +
  scale_fill_manual(values = colores_cluster, labels = nombres_cluster) +
  labs(title = "Cluster de cada país en cada período", x = "Período", y = NULL, fill = "Cluster")

grafico_trayectorias
ggsave("output/graficos/cluster_pais_periodo.png", plot = grafico_trayectorias,
       width = 10, height = 8, dpi = 300, bg = "white")

cambios <- datos_cluster %>%
 arrange(codigo_iso3_pais, periodo) %>%
 group_by(codigo_iso3_pais) %>%
 summarise(cambios_de_grupo = sum(cluster != lag(cluster), na.rm = TRUE),
 trayectoria = paste(cluster, collapse = " → ")) %>%
 mutate(pais = unname(nombres_paises[codigo_iso3_pais])) %>%
 arrange(desc(cambios_de_grupo))
cambios

# Parte 12: outliers y sensibilidad del cluster
filas_outlier <- rownames(X)[rowSums(abs(X) > 3) > 0]
filas_outlier # países–período con alguna variable extrema

if (length(filas_outlier) > 0) {
 mat_sin <- mat[!rownames(mat) %in% filas_outlier, ]
 mat_sin <- mat_sin[, sapply(mat_sin, sd) > 0]
 X_sin <- scale(mat_sin)
 pca_sin <- prcomp(X_sin)
 var_sin <- pca_sin$sdev^2 / sum(pca_sin$sdev^2)
 ncp_sin <- which(cumsum(var_sin) >= 0.80)[1]
 hc_sin <- hclust(dist(pca_sin$x[, 1:ncp_sin]), method = "ward.D2")
 cluster_sin <- cutree(hc_sin, k = k)

 table(con_outliers = datos_cluster$cluster[!rownames(mat) %in% filas_outlier],
 sin_outliers = cluster_sin)
}

# Parte 4: tablas de la sección 4 (cluster)
guardar_tabla(tabla_pca, "s4_componentes_principales")

cargas <- round(pca$rotation[, 1:min(ncp, 4)], 2)
rownames(cargas) <- secciones_sitc[rownames(cargas)]
guardar_tabla(cargas, "s4_cargas_componentes", filas = TRUE)

vtest_export <- tabla_vtest
colnames(vtest_export) <- nombres_cluster[colnames(vtest_export)]
guardar_tabla(vtest_export, "s4_vtest_clusters", filas = TRUE)

guardar_tabla(medias_cluster, "s4_medias_por_cluster")

trayectorias_tabla <- cambios %>%
  select(pais, trayectoria, cambios_de_grupo)
guardar_tabla(trayectorias_tabla, "s4_trayectorias_paises")

saveRDS(list(datos_cluster = datos_cluster, colores_cluster = colores_cluster,
             nombres_cluster = nombres_cluster, hc = hc, k = k, alturas = alturas,
             tabla_vtest = tabla_vtest, medias_cluster = medias_cluster, secciones_sitc = secciones_sitc,
             pca = pca, var_exp = var_exp, ncp = ncp, coords = coords, X = X, mat = mat,
             pp = pp, cambios = cambios, expo_pp = expo_pp),
        "input/resultados_cluster.rds")

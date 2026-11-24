library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

source("utils/envolver.R")
source("utils/guardar_grafico.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

base_latam      <- readRDS("input/base_latam.rds")
datos_test      <- readRDS("input/datos_test.rds")
nombres_paises  <- readRDS("input/nombres_paises.rds")
list2env(readRDS("input/resultados_faltantes.rds"), envir = environment())
list2env(readRDS("input/resultados_cluster.rds"),   envir = environment())

gris      <- "grey75"
destacado <- "#d7301f"
fuente    <- "Fuente: elaboración propia con datos del Atlas de Complejidad Económica (Growth Lab, Harvard)."

theme_set(
  theme_minimal(base_size = 13) +
    theme(plot.title            = element_text(face = "bold", size = 15, lineheight = 1.1),
          plot.subtitle         = element_text(color = "grey30", lineheight = 1.1),
          plot.caption          = element_text(color = "grey50", size = 9, hjust = 0),
          plot.title.position   = "plot",
          plot.caption.position = "plot",
          panel.grid.minor      = element_blank(),
          legend.position       = "bottom",
          plot.margin           = margin(12, 16, 8, 12))
)

formals(envolver)$ancho <- 75

colores_cluster <- c("1" = "#66a61e", "2" = "#bf812d", "3" = "#8c8c8c", "4" = "#2171b5")
nombres_cluster <- c("1" = "Agroindustria y minería", "2" = "Alimentos tropicales",
                     "3" = "Petroleros",              "4" = "Manufactureros")
nombres_cortos  <- c("1" = "Agro-min.", "2" = "Alim. trop.", "3" = "Petro", "4" = "Manuf.")
decadas_cortas  <- c("70s", "80s", "90s", "00s", "10s", "20s")

ausentes_pais <- combinaciones_ausentes %>%
  group_by(codigo_iso3_pais) %>%
  summarise(pct = 100 * mean(productos_ausentes) / n_productos, .groups = "drop") %>%
  mutate(pais     = unname(nombres_paises[codigo_iso3_pais]),
         destacar = rank(-pct, ties.method = "first") <= 2)

top_ausentes <- ausentes_pais %>% filter(destacar) %>% arrange(desc(pct)) %>% pull(pais)

g_ausentes <- ggplot(ausentes_pais, aes(x = pct, y = reorder(pais, pct), fill = destacar)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
            hjust = -0.15, size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = destacado, `FALSE` = gris), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = envolver(paste0(top_ausentes[1], " y ", top_ausentes[2],
                               " concentran la mayor proporción de productos sin registro"), 60),
       subtitle = envolver("Promedio anual de productos sin fila en la base, como % de los 786 productos (1970–2024)", 80),
       x = "% de productos sin registro", y = NULL, caption = fuente)

guardar_grafico(g_ausentes, "s2_productos_ausentes", ancho = 10, alto = 7)

medida <- "mediana"

resumen_region <- datos_test %>%
  group_by(region) %>%
  summarise(valor = if (medida == "mediana") median(pci_especializacion) else mean(pci_especializacion),
            n = n(), .groups = "drop") %>%
  mutate(es_al = region == "América Latina",
         etiqueta_region = paste0(region, " (", n, ")"))

g_regiones <- ggplot(resumen_region, aes(x = valor, y = reorder(etiqueta_region, valor), fill = es_al)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, color = "grey30") +
  geom_text(aes(label = format(round(valor, 2), decimal.mark = ","),
                hjust = ifelse(valor < 0, 1.15, -0.15)), size = 4) +
  scale_fill_manual(values = c(`TRUE` = destacado, `FALSE` = gris), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.25, 0.25))) +
  labs(title = envolver("América Latina se especializa en productos menos complejos que Europa y Norteamérica", 60),
       subtitle = envolver(paste0(ifelse(medida == "mediana", "Mediana", "Promedio"),
                                  " del PCI de la especialización por región, 2024. Entre paréntesis, cantidad de países. ",
                                  "Kruskal-Wallis: p < 0,001."), 85),
       x = "PCI de la especialización  (← menos complejo · más complejo →)", y = NULL,
       caption = fuente)

guardar_grafico(g_regiones, "s3_regiones_barras", ancho = 11, alto = 6)

k_elegido <- k
saltos <- data.frame(k_grupos = 2:10, altura = alturas) %>%
  mutate(elegido = k_grupos == k_elegido)

g_saltos <- ggplot(saltos, aes(x = factor(k_grupos), y = altura, fill = elegido)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`TRUE` = destacado, `FALSE` = gris), guide = "none") +
  labs(title = paste0("Se eligieron ", k_elegido, " grupos"),
       subtitle = envolver("Cada barra mide cuánta homogeneidad se pierde al pasar de k a k − 1 grupos. La caída es gradual: la elección es exploratoria.", 80),
       x = "Cantidad de grupos (k)", y = "Altura de la fusión", caption = fuente)

guardar_grafico(g_saltos, "s4_saltos_inercia", ancho = 9, alto = 5.5)

orden_izq_der <- unique(cutree(hc, k = k)[hc$order])
bordes        <- colores_cluster[as.character(orden_izq_der)]
altura_corte  <- mean(rev(hc$height)[c(k - 1, k)])

png("output/graficos/s4_dendrograma.png", width = 12, height = 6, units = "in", res = 300)
plot(hc, labels = FALSE, hang = -1, sub = "",
     main = paste0("Cortando el árbol a esta altura quedan ", k, " grupos"),
     xlab = "114 observaciones país–período", ylab = "Altura (distancia de Ward)")
rect.hclust(hc, k = k, border = bordes)
abline(h = altura_corte, lty = 2, col = destacado, lwd = 2)
invisible(dev.off())

sectores_clave <- c("sec_0", "sec_2", "sec_3", "sec_6", "sec_7", "sec_8")

composicion <- medias_cluster %>%
  select(cluster, all_of(sectores_clave)) %>%
  pivot_longer(-cluster, names_to = "seccion", values_to = "share") %>%
  mutate(sector = factor(secciones_sitc[seccion], levels = secciones_sitc[sectores_clave]),
         color  = ifelse(share >= 0.25, colores_cluster[as.character(cluster)], gris))

g_composicion <- ggplot(composicion, aes(x = cluster, y = share, fill = color)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = -0.4, size = 3.5) +
  facet_wrap(~ sector, nrow = 2) +
  scale_fill_identity() +
  scale_x_discrete(labels = nombres_cortos) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Cada perfil se define por uno o dos sectores dominantes",
       subtitle = envolver("% de las exportaciones con ventaja comparativa (RCA ≥ 1) que corresponde a cada sector. En color, los sectores que pesan 25% o más en el perfil.", 95),
       x = NULL, y = NULL,
       caption = paste0("No se muestran bebidas y tabaco, aceites, químicos ni otros (oro): no superan el 10% en ningún perfil.\n", fuente)) +
  theme(strip.text = element_text(face = "bold", size = 12),
        panel.spacing = unit(1.2, "lines"))

guardar_grafico(g_composicion, "s4_composicion_perfiles", ancho = 12, alto = 7.5)

pci_cluster <- datos_cluster %>%
  group_by(cluster) %>%
  summarise(pci = mean(pci_especializacion), .groups = "drop") %>%
  mutate(color = ifelse(pci == max(pci) | pci == min(pci),
                        colores_cluster[as.character(cluster)], gris))

g_pci_cluster <- ggplot(pci_cluster, aes(x = reorder(cluster, pci), y = pci, fill = color)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_text(aes(label = format(round(pci, 2), decimal.mark = ","),
                vjust = ifelse(pci < 0, 1.5, -0.5)), size = 4.5) +
  scale_fill_identity() +
  scale_x_discrete(labels = nombres_cluster) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(title = envolver("Petroleros y manufactureros están en los extremos de la complejidad", 60),
       subtitle = "PCI promedio de la especialización de cada perfil (todos los períodos)",
       x = NULL, y = "PCI de la especialización", caption = fuente)

guardar_grafico(g_pci_cluster, "s4_pci_por_perfil", ancho = 9, alto = 6)

evolucion <- datos_cluster %>% count(periodo, cluster, .drop = FALSE)

trop_ini <- evolucion %>% filter(cluster == "2", periodo == "1970-79") %>% pull(n)
trop_fin <- evolucion %>% filter(cluster == "2", periodo == "2020-24") %>% pull(n)
manuf_ini <- evolucion %>% filter(cluster == "4", periodo == "1970-79") %>% pull(n)
manuf_fin <- evolucion %>% filter(cluster == "4", periodo == "2020-24") %>% pull(n)

g_evolucion <- ggplot(evolucion, aes(x = periodo, y = n, fill = cluster)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  facet_wrap(~ cluster, nrow = 1, labeller = as_labeller(nombres_cluster)) +
  scale_fill_manual(values = colores_cluster, guide = "none") +
  scale_x_discrete(labels = decadas_cortas) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = envolver(paste0("El perfil de alimentos tropicales pasó de ", trop_ini, " países a ", trop_fin,
                        "; el manufacturero, de ", manuf_ini, " a ", manuf_fin), 70),
       subtitle = "Cantidad de países de América Latina en cada perfil, por década (19 países)",
       x = NULL, y = "Cantidad de países", caption = fuente) +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 10),
        panel.spacing = unit(1.5, "lines"),
        panel.grid.major.x = element_blank())

guardar_grafico(g_evolucion, "s4_evolucion_perfiles", ancho = 13, alto = 5.5)

trayectorias <- datos_cluster %>%
  group_by(codigo_iso3_pais) %>%
  mutate(grupo = ifelse(n_distinct(cluster) > 1, "Cambiaron de perfil", "Nunca cambiaron")) %>%
  ungroup() %>%
  mutate(pais  = unname(nombres_paises[codigo_iso3_pais]),
         grupo = factor(grupo, levels = c("Cambiaron de perfil", "Nunca cambiaron")))

n_estables <- trayectorias %>% filter(grupo == "Nunca cambiaron") %>%
  distinct(codigo_iso3_pais) %>% nrow()

g_trayectorias <- ggplot(trayectorias,
                         aes(x = periodo, y = reorder(pais, desc(pais)), fill = cluster, alpha = grupo)) +
  geom_tile(color = "white", linewidth = 0.8) +
  facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = colores_cluster, labels = nombres_cluster) +
  scale_alpha_manual(values = c("Cambiaron de perfil" = 1, "Nunca cambiaron" = 0.4), guide = "none") +
  scale_x_discrete(labels = decadas_cortas) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = envolver(paste0(n_estables, " de 19 países nunca cambiaron de perfil exportador en 55 años"), 60),
       subtitle = "Perfil de especialización de cada país, por década",
       x = NULL, y = NULL, fill = NULL, caption = fuente) +
  theme(strip.text = element_text(face = "bold", size = 12, hjust = 0),
        panel.grid = element_blank())

guardar_grafico(g_trayectorias, "s4_trayectorias", ancho = 9, alto = 9)

signo <- ifelse(pca$rotation["pci_especializacion", 1] < 0, -1, 1)

plano <- data.frame(codigo  = datos_cluster$codigo_iso3_pais,
                    periodo = datos_cluster$periodo,
                    cluster = datos_cluster$cluster,
                    CP1     = signo * pca$x[, 1],
                    CP2     = pca$x[, 2])

paises_destacados <- c("MEX", "CRI", "COL")
orden_paneles     <- unname(nombres_paises[paises_destacados])

fondo <- merge(plano[, c("CP1", "CP2")], data.frame(pais = orden_paneles), by = NULL) %>%
  mutate(pais = factor(pais, levels = orden_paneles))

recorridos <- plano %>%
  filter(codigo %in% paises_destacados) %>%
  arrange(codigo, periodo) %>%
  mutate(pais = factor(unname(nombres_paises[codigo]), levels = orden_paneles))

extremos <- bind_rows(
  recorridos %>% group_by(pais) %>% slice_head(n = 1) %>% mutate(etiqueta = "1970"),
  recorridos %>% group_by(pais) %>% slice_tail(n = 1) %>% mutate(etiqueta = "2024")
)

g_plano <- ggplot() +
  geom_point(data = fondo, aes(CP1, CP2), color = "grey85", size = 1.6) +
  geom_path(data = recorridos, aes(CP1, CP2, group = pais), color = "grey25",
            arrow = arrow(length = unit(0.22, "cm"), type = "closed")) +
  geom_point(data = recorridos, aes(CP1, CP2, color = cluster), size = 3) +
  geom_text(data = extremos, aes(CP1, CP2, label = etiqueta), vjust = -1, size = 3.3, fontface = "bold") +
  facet_wrap(~ pais, nrow = 1) +
  scale_color_manual(values = colores_cluster, labels = nombres_cluster, drop = FALSE) +
  labs(title = envolver("México y Costa Rica se movieron hacia la manufactura; Colombia, hacia el petróleo", 75),
       subtitle = envolver("Cada punto gris es un país en una década. La flecha muestra el recorrido del país entre 1970 y 2024; el color, su perfil en cada década.", 100),
       x = paste0("CP1 (", round(100 * var_exp[1], 1), "%):  ← menos diversidad y complejidad · más →"),
       y = paste0("CP2 (", round(100 * var_exp[2], 1), "%)"),
       color = NULL, caption = fuente) +
  theme(strip.text = element_text(face = "bold", size = 13),
        panel.spacing = unit(1.5, "lines"))

guardar_grafico(g_plano, "s4_plano_recorridos", ancho = 13, alto = 6)

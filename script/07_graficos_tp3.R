library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(ggtext)

source("utils/envolver.R")
source("utils/theme_owid.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

datos_test      <- readRDS("input/datos_test.rds")
nombres_paises  <- readRDS("input/nombres_paises.rds")
list2env(readRDS("input/resultados_cluster.rds"), envir = environment())

familia       <- "sans"
familia_serif <- "serif"
update_geom_defaults("text", list(family = familia))
update_geom_defaults("text_repel", list(family = familia))

fuente <- "Fuente: elaboración propia con datos del Atlas de Complejidad Económica (Growth Lab, Harvard)."
coma <- label_number(big.mark = ".", decimal.mark = ",")

gris_txt <- "#5b5b5b"

# GRÁFICO 1 · EXPLORATORIO
exp_df <- datos_test %>%
  mutate(es_al = region == "América Latina",
         expo_mm = expo_total / 1e9,
         etiqueta = NA_character_)

extra_al <- c()
al_destacados <- exp_df %>%
  filter(es_al) %>%
  { c(slice_max(., expo_mm, n = 3, with_ties = FALSE)$codigo_iso3_pais,
      slice_max(., pci_especializacion, n = 1, with_ties = FALSE)$codigo_iso3_pais,
      slice_min(., pci_especializacion, n = 1, with_ties = FALSE)$codigo_iso3_pais,
      slice_max(., n_productos_rca, n = 1, with_ties = FALSE)$codigo_iso3_pais,
      slice_min(., n_productos_rca, n = 1, with_ties = FALSE)$codigo_iso3_pais,
      extra_al) } %>% unique()

raros <- exp_df %>%
  filter(!es_al) %>%
  { bind_rows(slice_max(., pci_especializacion, n = 4, with_ties = FALSE),
              slice_max(., n_productos_rca, n = 3, with_ties = FALSE)) } %>%
  distinct(codigo_iso3_pais) %>% pull()

exp_df <- exp_df %>%
  mutate(etiqueta = case_when(es_al & codigo_iso3_pais %in% al_destacados ~ unname(nombres_paises[codigo_iso3_pais]),
                              !es_al & codigo_iso3_pais %in% raros ~ unname(nombres_paises[codigo_iso3_pais]),
                              TRUE ~ NA_character_))

med_x <- median(exp_df$n_productos_rca)
med_y <- median(exp_df$pci_especializacion)
n_paises <- nrow(exp_df)

colores_region <- c("América Latina"                  = "#E8711A",
                    "Europa y Norteamérica"           = "#286BBB",
                    "Asia Oriental y Pacífico"        = "#A8E0C8",
                    "Asia del Sur"                    = "#7A1F4D",
                    "Medio Oriente y Norte de África" = "#C9B458",
                    "África subsahariana"             = "#2E1A5E")

exp_df <- exp_df %>%
  mutate(region = factor(region, levels = names(colores_region))) %>%
  arrange(desc(region))

g_exploratorio <- ggplot(exp_df, aes(x = n_productos_rca, y = pci_especializacion)) +
  geom_vline(xintercept = med_x, color = "#8a8a8a", linewidth = 0.5) +
  geom_hline(yintercept = med_y, color = "#8a8a8a", linewidth = 0.5) +
  geom_point(aes(fill = region, size = expo_mm), shape = 21, color = "white",
             stroke = 0.4, alpha = 0.85) +
  geom_text_repel(data = filter(exp_df, !is.na(etiqueta)),
                  aes(label = etiqueta, color = es_al),
                  size = 3.1, fontface = "bold", min.segment.length = 0.1,
                  segment.color = "grey60", segment.size = 0.3,
                  box.padding = 0.35, max.overlaps = Inf, seed = 123,
                  show.legend = FALSE) +
  annotate("text", x = Inf, y = Inf, label = "Más diversos y más complejos",
           hjust = 1.05, vjust = 1.6, size = 3.2, color = "grey50", fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, label = "Más diversos, menos complejos",
           hjust = 1.05, vjust = -0.8, size = 3.2, color = "grey50", fontface = "italic") +
  annotate("text", x = -Inf, y = Inf, label = "Menos diversos, más complejos",
           hjust = -0.05, vjust = 1.6, size = 3.2, color = "grey50", fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf, label = "Menos diversos, menos complejos",
           hjust = -0.05, vjust = -0.8, size = 3.2, color = "grey50", fontface = "italic") +
  scale_color_manual(values = c(`TRUE` = "#B5530F", `FALSE` = "#3c3c3c"), guide = "none") +
  scale_fill_manual(values = colores_region, name = "Región", drop = FALSE) +
  scale_size_area(max_size = 11, guide = "none") +
  scale_x_continuous(labels = coma, expand = expansion(mult = c(0.03, 0.06))) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
    guides(fill = guide_legend(order = 1, nrow = 2, override.aes = list(size = 4, alpha = 1))) +
    labs(title = paste0("Diversidad, complejidad y tamaño de las exportaciones de ",
                      n_paises, " países, 2024"),
       subtitle = paste0("Cada burbuja es un país y su tamaño indica cuánto exporta. ",
                         "Las líneas grises marcan la mediana de cada eje."),
       x = "Diversidad: productos exportados con ventaja comparativa ",
       y = "Complejidad de lo que exporta con ventaja\n(PCI promedio)",
       caption = paste(fuente, "Grupo 19, TP Ciencia de Datos.")) +
  theme_owid(12) +
  theme(plot.title = element_text(size = 15),
        legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
        legend.justification = "left", legend.spacing.y = unit(0.1, "cm"),
        legend.title = element_text(size = 9, face = "bold", color = gris_txt),
        legend.text = element_text(size = 9), panel.grid.major.x = element_blank())

ggsave("output/graficos/grupo_19_exploratorio.jpg", plot = g_exploratorio,
       width = 24, height = 17, units = "cm", dpi = 300, bg = "white")

# GRÁFICO 2 · COMUNICATIVO
periodos <- levels(datos_cluster$periodo)

tray <- datos_cluster %>%
  mutate(manuf = cluster == "4") %>%
  group_by(codigo_iso3_pais) %>%
  summarise(primera = ifelse(any(manuf), min(which(manuf)), Inf),
            hoy     = manuf[periodo == periodos[length(periodos)]],
            alguna  = any(manuf), .groups = "drop") %>%
  mutate(pais = unname(nombres_paises[codigo_iso3_pais]),
         grupo = case_when(hoy ~ 1, alguna ~ 2, TRUE ~ 3)) %>%
  arrange(grupo, primera, pais)

n_hoy    <- sum(tray$grupo == 1)
n_dejo   <- sum(tray$grupo == 2)
n_nunca  <- sum(tray$grupo == 3)
n_total  <- nrow(tray)

pl <- datos_cluster %>%
  mutate(pais = unname(nombres_paises[codigo_iso3_pais]),
         manuf = cluster == "4") %>%
  mutate(pais = factor(pais, levels = rev(tray$pais)))

cuenta <- pl %>% group_by(periodo) %>% summarise(n = sum(manuf), .groups = "drop")
envolver_x <- setNames(paste0(sub("-", "–", cuenta$periodo), "\n", cuenta$n, " de ", n_total), cuenta$periodo)

y_hoy   <- c(n_total + 0.4, n_total - n_hoy + 0.6)
y_nunca <- c(n_nunca + 0.4, 0.6)
x_der   <- length(periodos) + 0.65

col_manuf <- "#286BBB"
col_resto <- "#e4e4e4"

g_comunicativo <- ggplot(pl, aes(x = periodo, y = pais, fill = manuf)) +
  geom_tile(color = "white", linewidth = 1.6) +
  scale_fill_manual(values = c(`TRUE` = col_manuf, `FALSE` = col_resto), guide = "none") +
  scale_x_discrete(labels = c(sub("-", "–", periodos[-length(periodos)]),
                              paste0(sub("-", "–", periodos[length(periodos)]), "\n(hoy)")),
                   position = "top") +

  annotate("rect", xmin = length(periodos) - 0.5, xmax = length(periodos) + 0.5,
           ymin = 0.5, ymax = n_total + 0.5, fill = NA, color = "#3c3c3c",
           linetype = "dashed", linewidth = 0.5) +

  annotate("text", x = length(periodos), y = n_total - n_hoy - (n_dejo - 1) / 2,
           label = "dejó de serlo", size = 2.7, color = "#3c3c3c", fontface = "italic") +

  annotate("segment", x = x_der, xend = x_der, y = y_hoy[2], yend = y_hoy[1], color = col_manuf, linewidth = 1) +
  annotate("text", x = x_der + 0.12, y = mean(y_hoy), hjust = 0, vjust = 0.5, lineheight = 1.05,
           size = 3.6, color = col_manuf, fontface = "bold",
           label = paste0(n_hoy, " manufactureros\nen 2020–24")) +
  annotate("segment", x = x_der, xend = x_der, y = y_nunca[1], yend = y_nunca[2], color = "grey55", linewidth = 1) +
  annotate("text", x = x_der + 0.12, y = mean(y_nunca), hjust = 0, vjust = 0.5, lineheight = 1.05,
           size = 3.6, color = "grey40", fontface = "bold",
           label = paste0(n_nunca, " nunca lo\nfueron")) +
  coord_cartesian(clip = "off") +
  labs(title = paste0("Solo ", n_hoy, " de ", n_total, " países latinoamericanos tienen un ",
                      "<span style='color:", col_manuf, "'>perfil exportador manufacturero</span>"),
       x = NULL, y = NULL,
       caption = paste0("Perfil según clusterización por estructura de ventajas comparativas \n",
                        "Fuente: elaboración propia con datos del Atlas de Complejidad Económica (Growth Lab, Harvard).\nGrupo 19, TP Ciencia de Datos")) +
  theme_owid(12) +
    theme(plot.title = element_textbox_simple(size = 18, family = familia_serif, face = "bold",
                                            color = "#3c3c3c", lineheight = 0.3,
                                            margin = margin(b = 12)),
        panel.grid.major = element_blank(),
        axis.text.x.top = element_text(size = 10, color = gris_txt),
        axis.text.y = element_text(size = 10.5, color = "#3c3c3c"),
        plot.margin = margin(12, 150, 10, 12))

ggsave("output/graficos/grupo_19_comunicativo.jpg", plot = g_comunicativo,
       width = 24, height = 17, units = "cm", dpi = 300, bg = "white")

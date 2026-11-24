library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

source("utils/guardar_tabla.R")
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

datos_test        <- readRDS("input/datos_test.rds")
datos_2024_mundo  <- readRDS("input/datos_2024_mundo.rds")

#  Parte 4: descriptivos y gráfico
datos_test %>%
 group_by(region) %>%
 summarise(n = n(),
 media = round(mean(pci_especializacion), 3),
 mediana = round(median(pci_especializacion), 3),
 desvio = round(sd(pci_especializacion), 3)) %>%
 arrange(desc(mediana))

grafico_boxplot_regiones <- ggplot(datos_test, aes(x = reorder(region, pci_especializacion, median),
                                                   y = pci_especializacion,
                                                   fill = region == "América Latina")) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("grey80", "#d7301f")) +
  coord_flip() +
  labs(title = "Complejidad de la especialización por región (2024)",
       x = NULL,
       y = "PCI promedio de productos con RCA normalizado ≥ 0 (ponderado por valor)")

grafico_boxplot_regiones
ggsave("output/graficos/boxplot_regiones.png", plot = grafico_boxplot_regiones,
       width = 10, height = 6, dpi = 300, bg = "white")

#  Parte 5: supuestos del ANOVA
modelo_anova <- aov(pci_especializacion ~ region, data = datos_test)

# Normalidad de los residuos (H0: los residuos son normales)
shapiro.test(residuals(modelo_anova))

# Normalidad dentro de cada región
datos_test %>%
  group_by(region) %>%
  summarise(p_shapiro = round(shapiro.test(pci_especializacion)$p.value, 4))

# Homogeneidad de varianzas: test de Levene (versión con la mediana)
datos_levene <- datos_test %>%
  group_by(region) %>%
  mutate(desvio_abs = abs(pci_especializacion - median(pci_especializacion))) %>%
  ungroup()
summary(aov(desvio_abs ~ region, data = datos_levene))

qqnorm(residuals(modelo_anova),
       main = "Q-Q de los residuos del ANOVA",
       xlab = "Cuantiles teóricos (distribución normal)",
       ylab = "Cuantiles observados (residuos)")
qqline(residuals(modelo_anova))

png("output/graficos/qq_residuos.png", width = 8, height = 6, units = "in", res = 300)
qqnorm(residuals(modelo_anova),main = "Q-Q de los residuos del ANOVA",
       xlab = "Cuantiles teóricos (distribución normal)",
       ylab = "Cuantiles observados (residuos)")
qqline(residuals(modelo_anova))
dev.off()

#  Parte 6: ANOVA (como referencia)
summary(modelo_anova)
ss <- summary(modelo_anova)[[1]][["Sum Sq"]]
eta2 <- ss[1] / sum(ss)
cat("Eta cuadrado:", round(eta2, 3), "\n")

#  Parte 7: Kruskal-Wallis (si no se cumplen los supuestos)
kw <- kruskal.test(pci_especializacion ~ region, data = datos_test)
kw

# Tamaño del efecto: épsilon cuadrado = H / (n - 1)
n <- nrow(datos_test)
epsilon2 <- unname(kw$statistic) / (n - 1)
cat("Épsilon cuadrado:", round(epsilon2, 3), "\n")

#  Parte 8: post-hoc · comparaciones de a pares con corrección de Holm
pairwise.wilcox.test(datos_test$pci_especializacion, datos_test$region,
 p.adjust.method = "holm")

#  Parte 9 (robustez): mismo test con PCI sin ponderar
pci_sin_ponderar <- datos_2024_mundo %>%
 filter(rca_normalizado >= 0, !is.na(indice_complejidad_producto)) %>%
 group_by(codigo_iso3_pais) %>%
 summarise(pci_simple = mean(indice_complejidad_producto), .groups = "drop")

datos_test %>%
 left_join(pci_sin_ponderar, by = "codigo_iso3_pais") %>%
 kruskal.test(pci_simple ~ region, data = .)

# Parte 3: Inferencia: complejidad de la especialización por región (2024)
descriptivos_region <- datos_test %>%
  group_by(region) %>%
  summarise(paises  = n(),
            mediana = round(median(pci_especializacion), 2),
            media   = round(mean(pci_especializacion), 2),
            desvio  = round(sd(pci_especializacion), 2)) %>%
  arrange(desc(mediana))
guardar_tabla(descriptivos_region, "s3_descriptivos_por_region")

# Resumen de los tests (supuestos y resultados)
sw      <- shapiro.test(residuals(modelo_anova))
levene  <- summary(aov(desvio_abs ~ region, data = datos_levene))[[1]]
anova_t <- summary(modelo_anova)[[1]]

resumen_tests <- data.frame(
  test = c("Shapiro-Wilk (residuos)", "Levene", "ANOVA", "Kruskal-Wallis"),
  estadistico = c(paste0("W = ", round(sw$statistic, 3)),
                  paste0("F = ", round(levene[1, "F value"], 2)),
                  paste0("F = ", round(anova_t[1, "F value"], 2)),
                  paste0("χ² = ", round(kw$statistic, 2))),
  gl = c("—",
         paste(levene[1, "Df"], "y", levene[2, "Df"]),
         paste(anova_t[1, "Df"], "y", anova_t[2, "Df"]),
         as.character(kw$parameter)),
  p_valor = c(format.pval(sw$p.value, digits = 3),
              format.pval(levene[1, "Pr(>F)"], digits = 3),
              format.pval(anova_t[1, "Pr(>F)"], digits = 3),
              format.pval(kw$p.value, digits = 3)),
  tamanio_efecto = c("—", "—",
                     paste0("η² = ", round(eta2, 2)),
                     paste0("ε² = ", round(epsilon2, 2)))
)
guardar_tabla(resumen_tests, "s3_resumen_tests")

# Post-hoc: p-valores ajustados por Holm
posthoc <- pairwise.wilcox.test(datos_test$pci_especializacion, datos_test$region,
                                p.adjust.method = "holm")
m <- posthoc$p.value
posthoc_tabla <- ifelse(is.na(m), "—",
                        formatC(m, format = "f", digits = 4, decimal.mark = ","))
guardar_tabla(posthoc_tabla, "s3_posthoc_holm", filas = TRUE)

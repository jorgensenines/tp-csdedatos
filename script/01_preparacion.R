library(dplyr)
library(readr)
library(tidyr)
library(countrycode)

dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("input", showWarnings = FALSE)

base_mundial <- read_csv("raw/sitc_country_product_year_4.csv")

#Antes de analizar las variables, vamos a renombrarlas
base_mundial <- base_mundial %>%
  rename(
    id_pais = country_id,
    codigo_iso3_pais = country_iso3_code,
    id_producto = product_id,
    codigo_sitc_producto = product_sitc_code,
    anio = year,
    valor_exportacion = export_value,
    valor_importacion = import_value,
    cuota_mercado_global = global_market_share,
    rca_exportacion = export_rca,
    distancia = distance,
    ganancia_oportunidad = cog,
    indice_complejidad_producto = pci
  ) %>%
  mutate(rca_normalizado = (rca_exportacion - 1) / (rca_exportacion + 1))

codigos_latam <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER", "URY", "VEN")

base_latam <- base_mundial %>%
  filter(codigo_iso3_pais %in% codigos_latam) %>%
  # Filtramos años y eliminamos valores NA
  filter(anio >= 1970 & anio <= 2024 & !is.na(anio))
codigos_mundo <- unique(base_mundial$codigo_iso3_pais)
nombres_paises <- countrycode(codigos_mundo, origin = "iso3c", destination = "cldr.name.es",
                              custom_match = c(DOM = "Rep. Dominicana"), warn = FALSE)
nombres_paises <- setNames(coalesce(nombres_paises, codigos_mundo), codigos_mundo)

datos_2024_mundo <- base_mundial %>% filter(anio == 2024)

# Exportaciones totales de cada país
expo_pais <- datos_2024_mundo %>%
 group_by(codigo_iso3_pais) %>%
 summarise(expo_total = sum(valor_exportacion, na.rm = TRUE), .groups = "drop")

# PCI promedio de los productos con RCA > 0, ponderado por valor exportado
pci_pais <- datos_2024_mundo %>%
 filter(rca_normalizado >= 0, valor_exportacion > 0,
 !is.na(indice_complejidad_producto)) %>%
 group_by(codigo_iso3_pais) %>%
 summarise(
 pci_especializacion = weighted.mean(indice_complejidad_producto, valor_exportacion),
 n_productos_rca = n(),
 .groups = "drop"
 ) %>%
 left_join(expo_pais, by = "codigo_iso3_pais")

#  Parte 3: asignamos la región segun la clasificacion del Banco Mundial.
pci_pais <- pci_pais %>%
 mutate(
 region_bm = countrycode(codigo_iso3_pais, origin = "iso3c", destination = "region"),
 region = case_when( codigo_iso3_pais %in% codigos_latam ~ "América Latina",
 region_bm == "Latin America & Caribbean" ~ NA_character_,
 region_bm %in% c("Europe & Central Asia",
 "North America") ~ "Europa y Norteamérica",
 region_bm == "East Asia & Pacific" ~ "Asia Oriental y Pacífico",
 region_bm == "South Asia" ~ "Asia del Sur",
 region_bm %in% c("Middle East, North Africa, Afghanistan & Pakistan") ~ "Medio Oriente y Norte de África",
 region_bm == "Sub-Saharan Africa" ~ "África subsahariana"
 )
 )

# Países sin región asignada

# Filtramos: países con exportaciones de al menos USD 1.000 millones
datos_test <- pci_pais %>%
 filter(!is.na(region), expo_total >= 1e9) %>%
 mutate(region = factor(region))

saveRDS(base_latam,        "input/base_latam.rds")
saveRDS(datos_test,        "input/datos_test.rds")
saveRDS(datos_2024_mundo,  "input/datos_2024_mundo.rds")
saveRDS(nombres_paises,    "input/nombres_paises.rds")

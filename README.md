# Ventajas comparativas de América Latina: estructura exportadora y complejidad económica

## Integrantes, grupo 19:

- Bianca Vitale
- Maria Inés Jorgensen
- Zoe Zeitoune

## Objetivo: 

Describir la estructura exportadora de 19 países de América Latina y analizar en qué productos se concentran sus ventajas comparativas. La hipótesis del trabajo es que la ausencia de procesos sostenidos de industrialización dejó a América Latina con una estructura exportadora basada en recursos naturales. 

## Base:

- **Fuente:** [Atlas de Complejidad Económica. Growth Lab, Harvard](https://atlas.hks.harvard.edu/data-downloads/)
- **Clasificación:** SITC Rev. 2, 4 dígitos
- **Unidad de observación:** país × producto × año
- **Recorte utilizado:** 19 países de América Latina, 1970–2024 

## Análisis realizado:

1. Limpieza y recorte de la base.
2. Exploración de distribuciones de las variables principales.
3. Análisis de valores faltantes y de outliers.
4. Método inferencial: test de Kruskal-Wallis. 
5. Método cuantitativo: clustering de países según su estructura de ventajas comparativas. 
6. Visualizaciones.

## Estructura del repositorio

```
tp-csdedatos/
├── raw/              # Datos originales del Atlas
├── auxiliar/         # Archivos de apoyo
├── input/            # Datos procesados
├── output/
│   ├── tablas/       # Tablas de resultados
│   └── graficos/     # Visualizaciones
├── script/           # Scripts de R
├── utils/            # Funciones propias
└── README.md
```
## Reproducción
 
### Paquetes necesarios
```r
install.packages(c("dplyr", "readr", "tidyr", "ggplot2", "scales", "ggrepel",
                   "countrycode", "gridExtra", "ggtext"))
```
El análisis se desarrolló en Google Colab, con R. Los scripts también se pueden correr en RStudio o en cualquier instalación de R; `grid` y `stats` vienen incluidos en R.
 
### Paso manual: descarga de la base

La base es demasiado grande para subirla a GitHub (521,9 MB). Descargar el archivo `sitc_country_product_year_4.csv` desde [este link de Drive](https://drive.google.com/file/d/1l6mz0B7js6X0esEcms8PTiFkLuNlFt4v/view?usp=drive_link), que es una copia del archivo original, sin modificar, de la [fuente oficial del Growth Lab](https://atlas.hks.harvard.edu/data-downloads/), y guardarlo en la carpeta `raw/`.

En Google Colab, también se puede descargar automáticamente ejecutando script/00_descarga_base.R.
 
### Orden de ejecución
Todos los scripts se corren desde la carpeta raíz del repositorio, en este orden. 
 
| # | Script | Qué hace | Lee | Escribe |
|---|---|---|---|---|
| 0 | script/00_descarga_base.R | Descarga la base desde Drive a raw/ (opcional si ya se descargó a mano) | Drive | raw/ |
| 1 | `script/01_preparacion.R` | Lee la base, renombra variables, calcula el RCA normalizado, recorta los 19 países de América Latina y arma la tabla país × región | `raw/` | `input/` |
| 2 | `script/02_descriptivos.R` | Distribuciones de registros, comercio, RCA, PCI y distancia | `input/` | `output/graficos/s1_*` |
| 3 | `script/03_faltantes_outliers.R` | Valores faltantes, ceros y outliers (IQR) | `input/` | `output/graficos/`, `output/tablas/s2_*` |
| 4 | `script/04_kruskal_regiones.R` | Test de Kruskal-Wallis (y ANOVA de referencia) de la complejidad de la especialización por región, con post-hoc de Holm | `input/` | `output/graficos/`, `output/tablas/s3_*` |
| 5 | `script/05_clustering.R` | Clustering de países por período (PCA + Ward.D2 + v-test) | `input/` | `input/resultados_cluster.rds`, `output/graficos/`, `output/tablas/s4_*` |
| 6 | `script/06_graficos_presentacion.R` | Gráficos de la presentación | `input/` | `output/graficos/s2_*`, `s3_*`, `s4_*` |
| 7 | `script/07_graficos_tp3.R` | Gráficos exploratorio y comunicativo de la 3.ª entrega (JPG) | `input/` | `output/graficos/grupo_19_*.jpg` |

Para correrlo en Colab: crear un notebook con entorno de ejecución R, clonar el repositorio (`!git clone https://github.com/jorgensenines/tp-csdedatos.git`), subir el CSV a `raw/` y ejecutar los scripts en orden desde la carpeta raíz del repo con `source("script/01_preparacion.R")`, y así con los demás.

## Conclusiones principales

Las mayores ventajas comparativas de América Latina se concentran en recursos naturales, la región tiene poca presencia en los productos más complejos. 
El clustering de países por década identificó cuatro perfiles: manufactureros, petroleros, materias primas y alimentos, y un perfil histórico que hoy ya no tiene ningún país. En 2020-24, diez países se ubican en materias primas y alimentos, tres en petroleros y solo seis tienen un perfil manufacturero. Doce países nunca tuvieron perfil manufacturero y Panamá dejó de tenerlo en 2020-24.
Estos resultados apoyan en parte nuestra hipótesis de trabajo: la región tiene una estructura exportadora concentrada en recursos naturales y pocos países con perfil manufacturero. Sin embargo, el análisis es descriptivo: muestra qué exportan los países y cómo cambió con el tiempo, pero no permite establecer las causas de la estructura exportadora latinoamericana. 

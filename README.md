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
├── script/           # Script 
├── utils/            # Funciones propias
└── README.md
```

## Reproducción:

### Paquetes necesarios

```r
install.packages(c("tidyverse", "scales", "ggrepel"))
```

### Paso manual

1. Descargar la base del Atlas desde [atlas.hks.harvard.edu/data-downloads](https://atlas.hks.harvard.edu/data-downloads).
2. Guardarlo en la carpeta `raw/` con el nombre `sitc_country_product_year_4.csv`.

### Orden de ejecución

1. `script/01_...R` — _qué hace_
2. `script/02_...R` — _qué hace_
3. `script/03_...R` — _qué hace_

## Conclusiones principales

América Latina tiene ventajas comparativas concentradas en recursos naturales y poca presencia en productos complejos. Solo 4 de los 19 países contemplados exportan hoy con perfil manufacturero, y 14 nunca lo hicieron. Estos resultados son consistentes con nuestra hipótesis de trabajo. 

dir.create("raw", showWarnings = FALSE)

destino <- "raw/sitc_country_product_year_4.csv"

if (!file.exists(destino)) {
  system("pip install -q gdown")
  system(paste("gdown 1l6mz0B7js6X0esEcms8PTiFkLuNlFt4v -O", destino))
}

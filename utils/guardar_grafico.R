guardar_grafico <- function(grafico, nombre, ancho = 10, alto = 6, carpeta = "output/graficos") {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(carpeta, paste0(nombre, ".png")), plot = grafico,
         width = ancho, height = alto, dpi = 300, bg = "white")
}

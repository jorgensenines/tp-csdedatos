guardar_tabla <- function(tabla, nombre, filas = FALSE, tamanio = 10) {
  df <- as.data.frame(tabla)
  if (filas) df <- cbind(" " = rownames(df), df)
  rownames(df) <- NULL
  dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
  if (nrow(df) == 0) { message("Tabla vacía, no se guarda: ", nombre); return(invisible(NULL)) }

  write_excel_csv2(df, paste0("output/tablas/", nombre, ".csv"))

  # Para la imagen: encabezados largos en varias líneas
  df_img <- df
  names(df_img) <- sapply(names(df_img), function(x) paste(strwrap(x, 18), collapse = "\n"))

  tema <- ttheme_default(base_size = tamanio)
  imagen <- tableGrob(df_img, rows = NULL, theme = tema)

  # El tamaño de la imagen sale del tamaño real de la tabla
  ancho <- grid::convertWidth(sum(imagen$widths), "in", valueOnly = TRUE) + 0.4
  alto  <- grid::convertHeight(sum(imagen$heights), "in", valueOnly = TRUE) + 0.4

  ggsave(paste0("output/tablas/", nombre, ".png"), imagen,
         width = ancho, height = alto, dpi = 300, bg = "white", limitsize = FALSE)
  cat("Guardada:", nombre, "\n")
}

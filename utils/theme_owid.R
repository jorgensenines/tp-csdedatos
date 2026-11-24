theme_owid <- function(base = 12, familia = "sans", familia_serif = "serif", gris_txt = "#5b5b5b") {
  theme_minimal(base_size = base, base_family = familia) +
    theme(text = element_text(color = gris_txt),
          plot.title = element_text(family = familia_serif, face = "bold", size = base + 6,
                                    color = "#3c3c3c", lineheight = 1.05, margin = margin(b = 4)),
          plot.subtitle = element_text(size = base - 1.5, color = gris_txt, lineheight = 1.2,
                                       margin = margin(b = 10)),
          plot.caption = element_text(size = 8, color = "#7a7a7a", hjust = 0, lineheight = 1.2,
                                      margin = margin(t = 10)),
          plot.title.position = "plot", plot.caption.position = "plot",
          panel.grid.major = element_line(color = "#dddddd", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = base - 2, color = gris_txt),
          axis.text = element_text(size = base - 2, color = gris_txt),
          legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(12, 16, 10, 12))
}

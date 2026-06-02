# ============================================================
# utils.R — Funciones compartidas para los gráficos de
#            El Quantificador / ENIGHUR
# Usar con: source("scripts/utils.R")  (desde la raíz del proyecto)
# ============================================================

source("scripts/packages.R")
ensure_packages(c("ggplot2", "cowplot"))

# ---- Constantes ----
LOGO_PATH <- "quantificador.png"

# ---- Tema base ----
#' Tema ggplot2 compartido para todos los gráficos del proyecto
theme_quantificador <- function() {
  theme_classic() +
    theme(
      axis.text             = element_text(colour = "grey20", size = 7.5),
      axis.title.x          = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
      axis.title.y          = element_text(size = 7, margin = margin(r = 6), hjust = 1),
      plot.title            = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle         = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption          = element_text(colour = "grey30", size = 5, lineheight = 1.1, hjust = 0,
                                           margin = margin(t = 6, r = 0, b = 0, l = 0)),
      axis.line             = element_line(colour = "grey60"),
      legend.position       = "none",
      panel.grid            = element_blank(),
      plot.margin           = margin(6, 36, 6, 16),
      plot.title.position   = "plot",
      plot.caption.position = "plot"
    )
}

# ---- Variante con leyenda ----
#' Como theme_quantificador() pero con leyenda visible dentro del panel
theme_quantificador_legend <- function(legend.position = c(0.82, 0.82)) {
  theme_quantificador() +
    theme(
      legend.position   = legend.position,
      legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
      legend.key        = element_blank(),
      legend.text       = element_text(size = 7.5, colour = "grey20"),
      legend.title      = element_text(size = 7.5, colour = "grey20")
    )
}

# ---- Logo overlay ----
#' Superpone el logo sobre un ggplot usando cowplot
#' @param plot       Un objeto ggplot
#' @param logo_path  Ruta al archivo de imagen del logo
#' @param x, y       Posición de la esquina inferior-izquierda del logo (fracción 0–1)
#' @param width      Ancho del logo (fracción del área del gráfico)
add_logo <- function(plot,
                     logo_path = LOGO_PATH,
                     x = 0.88, y = 0.07,
                     width = 0.10) {
  if (!file.exists(logo_path)) {
    message("Logo no encontrado en: ", logo_path, " — se omite.")
    return(plot)
  }
  ggdraw() +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    ) +
    draw_plot(plot, x = 0, y = 0, width = 1, height = 1) +
    draw_image(logo_path, x = x, y = y, width = width)
}

# ---- Exportar ----
#' Wrapper de ggsave con defaults del proyecto
#' @param filename  Ruta relativa dentro de output/figures/
#' @param plot      Objeto ggplot (por defecto last_plot())
#' @param width     Ancho en pulgadas (default 10)
#' @param height    Alto en pulgadas (default 6)
#' @param dpi       Resolución (default 300)
save_figure <- function(filename, plot = last_plot(), width = 10, height = 6, dpi = 300) {
  path <- file.path("output", "figures", filename)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(path)
}

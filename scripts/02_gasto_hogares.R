suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggalluvial)
})

source("scripts/utils.R")

options(scipen = 999)
pdf(NULL)

# ==============================================================================
# Number of households
# ==============================================================================

load("data/enighur/2025/Enighur_Bases_de_datos_R/Bases de trabajo/Bases_trabajo_R/Bases_trabajo.RData")
n_hh <- sum(as.numeric(ENIGHUR2025_HOGARES_AGREGADOS[["Fexp"]]))
rm(list = setdiff(ls(), c("n_hh", "theme_quantificador", "theme_quantificador_legend",
                           "add_logo", "save_figure", "LOGO_PATH")))

# ==============================================================================
# Cuadro 2.1.3 — per-household monthly averages
# ==============================================================================

raw <- read_excel(
  "data/enighur/2025/Tabulados_ENIGHUR_2024-2025.xlsx",
  sheet = "CUADRO 2.1.3", col_names = FALSE
)

vals <- setNames(as.numeric(raw[[2]][9:26]), trimws(raw[[1]][9:26]))
hh   <- vals / n_hh    # monthly USD per household

# ==============================================================================
# Sankey data
# ==============================================================================

cat_labels <- c(
  "Alimentos y bebidas",
  "Bebidas alc. y tabaco",
  "Prendas de vestir",
  "Vivienda y servicios",
  "Muebles y hogar",
  "Salud",
  "Transporte",
  "Información y comunicación",
  "Recreación y cultura",
  "Educación",
  "Restaurantes y alojamiento",
  "Seguros y serv. financieros",
  "Cuidado personal",
  "Gasto de no consumo",
  "No monetario"
)

cat_vals <- c(
  hh["Alimentos y bebidas no alcohólicas"],
  hh["Bebidas alcohólicas, tabaco y estupefacientes"],
  hh["Prendas de vestir y calzado"],
  hh["Vivienda, agua, electricidad, gas y otros combustibles"],
  hh["Muebles, artículos para el hogar y para la conservación ordinaria del hogar"],
  hh["Salud"],
  hh["Transporte"],
  hh["Información y comunicación"],
  hh["Recreación, deporte y cultura"],
  hh["Servicios Educativos"],
  hh["Servicios de restaurantes y alojamientos"],
  hh["Seguros y servicios financieros"],
  hh["Cuidado personal, previsión social y bienes y servicios diversos"],
  hh["Gasto de no consumo"],
  hh["Gasto corriente no monetario"]
)

# Colour palette — one per category in label order
colours <- c(
  "#2D9E55", "#7D3C98", "#F39C12", "#2471A3", "#1ABC9C",
  "#E74C3C", "#E67E22", "#5DADE2", "#9B59B6", "#1E8449",
  "#D35400", "#717D7E", "#E91E63", "#AAB7B8", "#CCD1D1"
)
palette  <- setNames(colours, cat_labels)

sankey <- tibble(
  nivel1 = paste0("Gasto corriente total\n$", round(hh["Gasto corriente total del hogar"]), "/mes"),
  nivel2 = c(
    rep(paste0("Monetario\n$", round(hh["Gasto corriente de consumo"] + hh["Gasto de no consumo"]), "/mes"), 14),
    paste0("No monetario\n$", round(hh["Gasto corriente no monetario"]), "/mes")
  ),
  nivel3 = factor(cat_labels, levels = cat_labels),
  value  = cat_vals
)

# ==============================================================================
# Chart
# ==============================================================================

sankey_plot <- ggplot(sankey,
  aes(axis1 = nivel1, axis2 = nivel2, axis3 = nivel3, y = value)) +

  geom_alluvium(aes(fill = nivel3), width = 0.2, alpha = 0.82, knot.pos = 0.35) +
  geom_stratum(width = 0.2, fill = "grey93", colour = "white", linewidth = 0.4) +

  # Axes 1 & 2: centred label (suppress axis3 by returning "" for cat labels)
  geom_text(stat = "stratum",
            aes(label = ifelse(as.character(after_stat(stratum)) %in% cat_labels,
                               "", as.character(after_stat(stratum)))),
            size = 2.8, colour = "grey20", lineheight = 0.9) +

  # Axis 3: right-side label with dollar amount
  geom_text(stat = "stratum",
            aes(label = ifelse(as.character(after_stat(stratum)) %in% cat_labels,
                               paste0(as.character(after_stat(stratum)),
                                      "  $", round(after_stat(count))),
                               "")),
            hjust = 0, nudge_x = 0.12, size = 2.5, colour = "grey20") +

  scale_fill_manual(values = palette) +
  scale_x_discrete(limits = c("nivel1", "nivel2", "nivel3"),
                   expand = expansion(add = c(0.7, 2.8))) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$", suffix = "/mes")) +
  coord_cartesian(clip = "off") +

  labs(
    title    = "¿En qué gastan su ingreso los hogares ecuatorianos?",
    subtitle = "Estructura del gasto corriente del hogar, promedio mensual por hogar — 2024-2025",
    x        = NULL,
    y        = "USD mensuales por hogar",
    caption  = paste0(
      "Fuente: Instituto Nacional de Estadística y Censos (INEC) — ENIGHUR 2024-2025, Cuadro 2.1.3.\n",
      "Nota: Valores promedio mensual por hogar. Total de hogares expandidos: ",
      formatC(round(n_hh), format = "d", big.mark = ","), "."
    )
  ) +

  theme_quantificador() +
  theme(
    legend.position      = "none",
    axis.text.x          = element_blank(),
    axis.ticks.x         = element_blank(),
    axis.line.x          = element_blank(),
    axis.title.y         = element_text(size = 9, hjust = 0.5),
    plot.caption         = element_text(size = 7, colour = "grey40", hjust = 0, lineheight = 1.2),
    panel.grid.major.y   = element_line(colour = "grey90", linewidth = 0.3),
    plot.margin          = margin(6, 160, 6, 16)
  )

save_figure("sankey_gasto_hogares_2025.png", sankey_plot, width = 13, height = 8)
cat("Guardado: output/figures/sankey_gasto_hogares_2025.png\n")

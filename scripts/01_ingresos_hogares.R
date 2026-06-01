suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(janitor)
})

options(scipen = 999)

# ==============================================================================
# 2025
# ==============================================================================

zip_path_2025 <- file.path(
  "data", "enighur", "2025",
  "Enighur_Bases_de_datos_R",
  "Bases de trabajo",
  "Bases_trabajo_R.zip"
)

if (!file.exists(zip_path_2025)) stop("Zip 2025 not found: ", zip_path_2025, call. = FALSE)

load(unz(zip_path_2025, "Bases_trabajo.RData"))

# INEC codes -1 as NO SABE / NO RESPONDE — treat as NA then drop
ingresos_2025 <- ENIGHUR2025_INGRESOS_H |>
  rename(ingresos_totales_monetarios = i1701097) |>
  clean_names() |>
  mutate(ingresos_totales_monetarios = na_if(ingresos_totales_monetarios, -1)) |>
  filter(!is.na(ingresos_totales_monetarios), ingresos_totales_monetarios > 0)

# ==============================================================================
# 2012
# ==============================================================================

data_path_2012 <- file.path(
  "data", "enighur", "2012",
  "bbd_ingresos_gastos_2011-2012",
  "2011-2012", "Ingresos_Gastos",
  "02 BASE DE DATOS", "02 TABLAS DE TRABAJO",
  "04 ENIGHUR11_INGRESOS_H.sav"
)

if (!file.exists(data_path_2012)) stop("Dataset 2012 not found: ", data_path_2012, call. = FALSE)

ingresos_2012 <- read_sav(data_path_2012) |>
  # i1401097: equivalent section 14.01 total wages, same naming pattern as i1701097 in 2025
  rename(ingresos_totales_monetarios = i1401097,
         fexp                        = fexp_cen2010) |>
  clean_names() |>
  mutate(ingresos_totales_monetarios = na_if(ingresos_totales_monetarios, -1)) |>
  filter(!is.na(ingresos_totales_monetarios), ingresos_totales_monetarios > 0)

# ==============================================================================
# Histograms
# ==============================================================================

hist_plot <- function(data, anio, fill_colour) {
  data |>
    ggplot(aes(x = ingresos_totales_monetarios, weight = fexp)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins      = 60,
      fill      = fill_colour,
      colour    = "white",
      linewidth = 0.2
    ) +
    scale_x_continuous(
      labels = scales::label_dollar(prefix = "$", big.mark = ","),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste("Distribución del ingreso monetario — ENIGHUR", anio),
      x     = "Ingreso monetario corriente mensual (USD)",
      y     = "Densidad"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

print(hist_plot(ingresos_2025, "2025", "#2D6A9F"))
print(hist_plot(ingresos_2012, "2012", "#E07B39"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(janitor)
})

source("scripts/utils.R")

options(scipen = 999)
pdf(NULL)   # suppress the default Rplots.pdf that R opens when no device is active

# ==============================================================================
# 2025
# ==============================================================================

rdata_path_2025 <- file.path(
  "data", "enighur", "2025",
  "Enighur_Bases_de_datos_R",
  "Bases de trabajo",
  "Bases_trabajo_R",
  "Bases_trabajo.RData"
)

if (!file.exists(rdata_path_2025)) stop("RData 2025 not found: ", rdata_path_2025, call. = FALSE)

load(rdata_path_2025)

# ENIGHUR2025_HOGARES_AGREGADOS has ing_mon_cor pre-computed by INEC:
# net wages + net self-employment + capital/property + transfers + other current income
ingresos_2025 <- ENIGHUR2025_HOGARES_AGREGADOS |>
  select(fexp = Fexp, ing_mon_cor) |>
  filter(!is.na(ing_mon_cor), ing_mon_cor > 0)

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

# Mirror the INEC 2025 derivation using 2012 section numbering (14xx = 17xx, 17xx = 20xx):
#   ing_asal_mon_net  = i1401097 (wages) + i1404xxx (annual bonuses) - i1701097 (deductions)
#   ing_ind_mon_net   = i1406099 (cuenta propia) + i1407099 (socio)
#   ing_ter_ocu       = a1443001 (other work income)
#   tranf_cor         = sum(i1444001:i1444008)
#   ing_ren_prop_cap  = sum(i1445001:i1445007)
#   otro_ing_cor      = b1443001
# Agricultural income from persons file omitted (no 2012 person-level file available here).
ingresos_2012 <- read_sav(data_path_2012) |>
  mutate(across(where(is.numeric), ~ ifelse(!is.na(.) & . < 0, 0, .))) |>
  mutate(
    ing_asal_mon_net = pmax(
      rowSums(cbind(i1401097, i1404001, i1404002, i1404003, i1404004, i1404005, i1404006), na.rm = TRUE) -
        coalesce(i1701097, 0),
      0
    ),
    ing_ind_mon_net  = pmax(
      rowSums(cbind(i1406099, i1407099), na.rm = TRUE),
      0
    ),
    # Agricultural monetary sales — same 8 variables as 2025 (i1708097 etc.), present in HH file
    ing_ag_mon       = rowSums(cbind(i1408097, i1409097, i1416097, i1421097,
                                     i1424097, i1428097, i1431097, i1436097), na.rm = TRUE),
    ing_ter_ocu      = coalesce(a1443001, 0),
    tranf_cor        = rowSums(cbind(i1444001, i1444002, i1444003, i1444004,
                                     i1444005, i1444006, i1444007, i1444008), na.rm = TRUE),
    ing_ren_prop_cap = rowSums(cbind(i1445001, i1445002, i1445003,
                                     i1445004, i1445005, i1445006, i1445007), na.rm = TRUE),
    otro_ing_cor     = coalesce(b1443001, 0),
    ing_mon_cor      = ing_asal_mon_net + ing_ind_mon_net + ing_ag_mon + ing_ter_ocu +
                       tranf_cor + ing_ren_prop_cap + otro_ing_cor
  ) |>
  select(fexp = Fexp_cen2010, ing_mon_cor) |>
  filter(!is.na(ing_mon_cor), ing_mon_cor > 0) |>
  # Deflate to 2025 prices using INEC's official spliced IPC series (base 2014=100,
  # ipc_ind_nac_reg_ciud_emp_clase_04_2026.xlsx, sheet "1. NACIONAL", row General).
  # IPC_2012 = avg(Jan–Dec 2012) = 93.30  |  IPC_2025 = avg(Jan–Dec 2025) = 113.86
  # Deflator = 113.86 / 93.30 = 1.2203  (22.0% cumulative, ~1.5%/yr)
  # Note: 7 additional transfer programmes in 2025 (Pensión MMA, PAM, PTUV, PDD, etc.)
  # did not exist in 2012 — that difference is real policy change, not a measurement artefact.
  mutate(ing_mon_cor = ing_mon_cor * (113.8566 / 93.3027))

# ==============================================================================
# Shared helpers
# ==============================================================================

wtd_quantile <- function(x, w, p) {
  ord  <- order(x)
  x    <- x[ord]; w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which.max(cumw >= p)]
}

make_breaks <- function(cutoff, n_bins = 60) seq(0, cutoff, length.out = n_bins + 1)

# ==============================================================================
# Individual histograms
# ==============================================================================

hist_plot <- function(data, anio, fill_colour) {
  cutoff <- ceiling(wtd_quantile(data$ing_mon_cor, data$fexp, 0.99) / 500) * 500
  brks   <- make_breaks(cutoff)

  data |>
    filter(ing_mon_cor <= cutoff) |>
    ggplot(aes(x = ing_mon_cor, weight = fexp)) +
    geom_histogram(breaks = brks, fill = fill_colour, colour = "white", linewidth = 0.2) +
    scale_x_continuous(
      labels = scales::label_dollar(prefix = "$", big.mark = ","),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste("Distribución del ingreso monetario corriente — ENIGHUR", anio),
      x     = "Ingreso monetario corriente del hogar (USD mensuales)",
      y     = "Número de hogares"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# ==============================================================================
# Overlay
# ==============================================================================

combined <- bind_rows(
  ingresos_2025 |> mutate(anio = "2025"),
  ingresos_2012 |> mutate(anio = "2012")
)

cutoff <- ceiling(wtd_quantile(combined$ing_mon_cor, combined$fexp, 0.95) / 500) * 500
brks   <- make_breaks(cutoff)

medians <- combined |>
  group_by(anio) |>
  summarise(mediana = wtd_quantile(ing_mon_cor, fexp, 0.5), .groups = "drop")

med_2012 <- medians$mediana[medians$anio == "2012"]
med_2025 <- medians$mediana[medians$anio == "2025"]

median_lines <- data.frame(
  xintercept = c(med_2012, med_2025),
  lbl        = c("Mediana 2012", "Mediana 2025")
)

overlay_plot <- combined |>
  filter(ing_mon_cor <= cutoff) |>
  ggplot(aes(x = ing_mon_cor, weight = fexp)) +
  geom_density(
    data      = \(d) filter(d, anio == "2025"),
    aes(colour = "ENIGHUR 2024-2025"),
    fill = "#2D6A9F", alpha = 0.55, linewidth = 0.7
  ) +
  geom_density(
    data      = \(d) filter(d, anio == "2012"),
    aes(colour = "ENIGHUR 2011-2012"),
    fill = "#D4691E", alpha = 0.35, linewidth = 0.7
  ) +
  geom_vline(
    data        = median_lines,
    aes(xintercept = xintercept, colour = lbl),
    linetype    = "dashed", linewidth = 0.7, inherit.aes = FALSE,
    key_glyph   = "path"
  ) +
  annotate("text",
           x = med_2012, y = Inf,
           label = paste0("Mediana 2012\n$", formatC(round(med_2012), format = "d", big.mark = ",")),
           hjust = 1.1, vjust = 1.4, size = 2.8, colour = "#A8400A") +
  annotate("text",
           x = med_2025, y = Inf,
           label = paste0("Mediana 2025\n$", formatC(round(med_2025), format = "d", big.mark = ",")),
           hjust = -0.1, vjust = 1.4, size = 2.8, colour = "#1A3A5C") +
  scale_colour_manual(
    name   = NULL,
    values = c(
      "ENIGHUR 2011-2012" = "#A8400A",
      "ENIGHUR 2024-2025" = "#1A3A5C",
      "Mediana 2012"      = "#A8400A",
      "Mediana 2025"      = "#1A3A5C"
    ),
    breaks = c("ENIGHUR 2011-2012", "ENIGHUR 2024-2025", "Mediana 2012", "Mediana 2025")
  ) +
  guides(
    colour = guide_legend(override.aes = list(
      fill      = c("#D4691E", "#2D6A9F", NA,       NA),
      alpha     = c(0.35,      0.55,      1,        1),
      colour    = c("#A8400A", "#1A3A5C", "#A8400A","#1A3A5C"),
      linetype  = c("solid",   "solid",   "dashed", "dashed"),
      linewidth = c(0.7,       0.7,       0.7,      0.7),
      size      = c(5,         5,         0,        0)
    ))
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = NULL) +
  labs(
    title    = "En 12 años, la distribución de ingresos de los ecuatorianos es casi la misma",
    subtitle = "Distribución del ingreso monetario de los hogares, 2012 vs. 2025",
    x        = "Ingreso monetario corriente del hogar (USD mensuales)",
    y        = "Densidad (número de hogares)",
    caption  = paste0(
      "Fuente: Instituto Nacional de Estadística y Censos (INEC) — ENIGHUR 2011-2012 y 2024-2025; IPC nacional (base 2014=100).\n",
      "Nota: Ingreso de 2012 deflactado a precios de 2025 usando el IPC empalmado INEC (factor: 1.220). ",
      "Se muestra hasta el percentil 95 del ingreso combinado (corte: $", formatC(cutoff, format = "d", big.mark = ","), ").\n",
      "El ingreso monetario corriente incluye remuneraciones netas, trabajo independiente,",
      " rentas de capital, transferencias y otros ingresos corrientes.\n",
      "Ingreso agropecuario 2012 calculado desde las variables de venta agrícola y pecuaria del hogar (i1408097–i1436097).",
      " Cifras ponderadas con el factor de expansión del hogar (Fexp)."
    )
  ) +
  theme_quantificador_legend() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 10, hjust = 0.5),
    axis.title.y = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, colour = "grey30", hjust = 0, lineheight = 1.2,
                                margin = margin(t = 6))
  )

# ==============================================================================
# Log-scale test chart (uncut)
# ==============================================================================

log_plot <- combined |>
  ggplot(aes(x = ing_mon_cor, weight = fexp)) +
  geom_density(
    data      = \(d) filter(d, anio == "2025"),
    aes(fill  = "ENIGHUR 2024-2025"),
    colour    = "#1A3A5C", alpha = 0.55, linewidth = 0.7
  ) +
  geom_density(
    data      = \(d) filter(d, anio == "2012"),
    aes(fill  = "ENIGHUR 2011-2012"),
    colour    = "#A8400A", alpha = 0.35, linewidth = 0.7
  ) +
  scale_x_log10(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = NULL) +
  scale_fill_manual(
    name   = NULL,
    values = c("ENIGHUR 2011-2012" = "#D4691E", "ENIGHUR 2024-2025" = "#2D6A9F")
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = c(0.35, 0.55), linewidth = 0.7))) +
  labs(
    title    = "En 12 años, la distribución de ingresos de los ecuatorianos es casi la misma",
    subtitle = "Escala logarítmica — distribución completa sin corte",
    x        = "Ingreso monetario corriente del hogar (USD mensuales, escala log)",
    y        = "Densidad (número de hogares)",
    caption  = "Fuente: INEC — ENIGHUR 2011-2012 y 2024-2025."
  ) +
  theme_quantificador_legend()

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
save_figure("distribucion_ingreso_2012_2025.png", overlay_plot)
save_figure("distribucion_ingreso_2012_2025_log.png", log_plot)
cat("Guardado: output/figures/\n")

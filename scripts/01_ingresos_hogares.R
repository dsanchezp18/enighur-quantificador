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

# Use INEC's pre-computed ing_mon_cor directly (includes net agricultural income).
# 2012 is now also computed with net agricultural income — both years comparable.
ingresos_2025 <- ENIGHUR2025_HOGARES_AGREGADOS |>
  select(fexp = Fexp, ing_mon_cor) |>
  filter(!is.na(ing_mon_cor), ing_mon_cor > 0)

# ==============================================================================
# 2012
# ==============================================================================

data_path_2012 <- file.path(
  "data", "enighur", "2012", "required",
  "ENIGHUR11_INGRESOS_H.sav"
)

if (!file.exists(data_path_2012)) stop("Dataset 2012 not found: ", data_path_2012, call. = FALSE)

# gas_ag for 2012: from GASTOS_HMO (c1703097-c1706097), exactly as INEC SPSS syntax.
gastos_hmo_path <- file.path(
  "data", "enighur", "2012", "required",
  "ENIGHUR11_GASTOS_HMO.sav"
)
gas_ag_2012 <- read_sav(gastos_hmo_path) |>
  mutate(gas_ag = rowSums(cbind(c1703097, c1704097, c1705097, c1706097), na.rm = TRUE)) |>
  select(Identif_hog, gas_ag)

# Reconstruct ing_mon_cor following INEC 2012 SPSS syntax exactly.
# Key differences from 2025: 18 wage items, 5 bonus items (no i1404004),
# 7 transfer items (no i1444008), i1407099 only for self-employment (not i1406099),
# i1709002 for independent deductions, 0→NA for capital income before summing.
ingresos_2012 <- read_sav(data_path_2012) |>
  left_join(gas_ag_2012, by = "Identif_hog") |>
  mutate(across(where(is.numeric), ~ ifelse(!is.na(.) & . < 0, 0, .))) |>
  mutate(
    # Wages: 18 items (i1401001-i1401018)
    suel_sal_bruto   = rowSums(cbind(i1401001,i1401002,i1401003,i1401004,i1401005,i1401006,
                                     i1401007,i1401008,i1401009,i1401010,i1401011,i1401012,
                                     i1401013,i1401014,i1401015,i1401016,i1401017,i1401018), na.rm=TRUE),
    ded_asal         = rowSums(cbind(i1701001, i1701002), na.rm = TRUE),
    # Bonuses: 5 items — i1404004 excluded per SPSS syntax
    ing_otro_neto    = rowSums(cbind(i1404001,i1404002,i1404003,i1404005,i1404006), na.rm=TRUE),
    ing_asal_mon_net = pmax(suel_sal_bruto - ded_asal + ing_otro_neto, 0),
    # Non-ag self-employment: only i1407099 per SPSS syntax (not i1406099)
    ing_cuent_prop_na = coalesce(as.numeric(i1407099), 0),
    # Net agricultural income: gross revenues − gas_ag (exactly as SPSS syntax)
    ag_rev           = rowSums(cbind(i1408097,i1409097,i1416097,i1421097,
                                     i1424097,i1428097,i1431097,i1436097), na.rm=TRUE),
    gas_ag           = coalesce(gas_ag, 0),
    i1432097         = ifelse(ag_rev >= gas_ag, ag_rev, gas_ag),  # SPSS: if revenues<costs, cost floor
    ing_ag_mon_neto  = i1432097 - gas_ag,
    ded_ind          = coalesce(as.numeric(i1709002), 0),
    ing_ind_mon_net  = pmax(ing_cuent_prop_na + ing_ag_mon_neto - ded_ind, 0),
    ing_ter_ocu      = coalesce(as.numeric(a1443001), 0),
    ing_trab_mon     = ing_asal_mon_net + ing_ind_mon_net + ing_ter_ocu,
    # Capital income: recode 0→NA per SPSS syntax, then sum
    ing_ren_prop     = rowSums(cbind(na_if(i1445004,0), na_if(i1445006,0), na_if(i1445007,0)), na.rm=TRUE),
    ing_cap          = rowSums(cbind(na_if(i1445001,0), na_if(i1445002,0),
                                     na_if(i1445003,0), na_if(i1445005,0)), na.rm=TRUE),
    ing_ren_prop_cap = ing_ren_prop + ing_cap,
    # Transfers: 7 items (i1444001-i1444007 per SPSS; i1444008 excluded)
    tranf_cor        = rowSums(cbind(i1444001,i1444002,i1444003,i1444004,
                                     i1444005,i1444006,i1444007), na.rm=TRUE),
    otro_ing_cor     = coalesce(as.numeric(b1443001), 0),
    ing_mon_cor      = ing_trab_mon + ing_ren_prop_cap + tranf_cor + otro_ing_cor
  ) |>
  select(fexp = Fexp_cen2010, ing_mon_cor) |>
  filter(!is.na(ing_mon_cor), ing_mon_cor > 0) |>
  # Deflate to 2024-2025 survey prices using INEC official spliced IPC (base 2014=100).
  # Survey periods: ENIGHUR 2011-2012 = Apr 2011–Mar 2012; ENIGHUR 2024-2025 = Dec 2024–Nov 2025.
  # IPC_2012_survey = avg(abr-11:mar-12) = 90.00  |  IPC_2025_survey = avg(dic-24:nov-25) = 113.68
  # Deflator = 113.68 / 90.00 = 1.2631
  mutate(ing_mon_cor = ing_mon_cor * (113.6774 / 90.0032))

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
           hjust = -0.1, vjust = 4.5, size = 2.8, colour = "#1A3A5C") +
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
      "Nota: Ingreso de 2012 deflactado a precios de la encuesta 2024-2025 usando el IPC empalmado INEC (factor: 1.263). ",
      "Se muestra hasta el percentil 95 del ingreso combinado (corte: $", formatC(cutoff, format = "d", big.mark = ","), ").\n",
      "El ingreso monetario corriente incluye remuneraciones netas, trabajo independiente,",
      " rentas de capital, transferencias y otros ingresos corrientes.\n",
      "Ingreso 2012 reconstruido siguiendo la sintaxis SPSS oficial INEC: neto de gastos agropecuarios (GASTOS_HMO), sin i1404004, sin i1444008.",
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

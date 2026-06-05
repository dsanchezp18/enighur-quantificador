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

resolve_existing_path <- function(candidates, label) {
  hits <- candidates[file.exists(candidates)]
  if (length(hits) > 0) {
    return(hits[[1]])
  }

  stop(
    label, " not found. Checked:\n- ",
    paste(candidates, collapse = "\n- "),
    call. = FALSE
  )
}

rdata_path_2025 <- resolve_existing_path(
  file.path(
    "data", "enighur", "2025",
    "Enighur_Bases_de_datos_R",
    "Bases de trabajo",
    "Bases_trabajo_R",
    "Bases_trabajo.RData"
  ),
  "RData 2025"
)

load(rdata_path_2025)

gasolina_hmo_2025 <- ENIGHUR2025_GASTOS_HMO |>
  transmute(
    Identif_hog,
    gasolina_mon = rowSums(cbind(
      as.numeric(c7222001),
      as.numeric(c7222003),
      as.numeric(c7222005)
    ), na.rm = TRUE)
  )

# Use INEC's pre-computed ing_mon_cor directly (includes net agricultural income).
# 2012 is now also computed with net agricultural income — both years comparable.
ingresos_2025 <- ENIGHUR2025_HOGARES_AGREGADOS |>
  select(fexp = Fexp, ing_mon_cor) |>
  filter(!is.na(ing_mon_cor), ing_mon_cor > 0)

# ==============================================================================
# 2012
# ==============================================================================

data_path_2012 <- resolve_existing_path(
  c(
    file.path("data", "enighur", "2012", "required", "ENIGHUR11_INGRESOS_H.sav"),
    file.path(
      "data", "enighur", "2012",
      "bbd_ingresos_gastos_2011-2012", "2011-2012", "Ingresos_Gastos",
      "02 BASE DE DATOS", "02 TABLAS DE TRABAJO", "04 ENIGHUR11_INGRESOS_H.sav"
    )
  ),
  "Dataset 2012"
)

# gas_ag for 2012: from GASTOS_HMO (c1703097-c1706097), exactly as INEC SPSS syntax.
gastos_hmo_path <- resolve_existing_path(
  c(
    file.path("data", "enighur", "2012", "required", "ENIGHUR11_GASTOS_HMO.sav"),
    file.path(
      "data", "enighur", "2012",
      "bbd_ingresos_gastos_2011-2012", "2011-2012", "Ingresos_Gastos",
      "02 BASE DE DATOS", "02 TABLAS DE TRABAJO", "08 ENIGHUR11_GASTOS_HMO.sav"
    )
  ),
  "Dataset 2012 GASTOS_HMO"
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
      x     = "Ingreso monetario corriente mensual del hogar",
      y     = "Número de hogares"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# ==============================================================================
# 2025 income distribution
# ==============================================================================

cutoff_2025 <- ceiling(wtd_quantile(ingresos_2025$ing_mon_cor, ingresos_2025$fexp, 0.95) / 100) * 100
brks_2025 <- seq(0, cutoff_2025, by = 100)
med_2025 <- wtd_quantile(ingresos_2025$ing_mon_cor, ingresos_2025$fexp, 0.5)

income_distribution_2025_plot <- ingresos_2025 |>
  filter(ing_mon_cor <= cutoff_2025) |>
  ggplot(aes(x = ing_mon_cor, weight = fexp)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = brks_2025,
    fill = "#2D6A9F",
    colour = "white",
    linewidth = 0.2,
    closed = "left"
  ) +
  geom_vline(
    xintercept = med_2025,
    colour = "#A8400A",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = med_2025,
    y = Inf,
    label = paste0("Mediana 2025\n$", formatC(round(med_2025), format = "d", big.mark = ",")),
    hjust = -0.08,
    vjust = 3.8,
    size = 3.0,
    colour = "#A8400A"
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    breaks = scales::breaks_width(250),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Ingreso monetario corriente mensual del hogar",
    y = "Porcentaje de hogares"
  ) +
  theme_quantificador() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ==============================================================================
# 2025 income vs expenditure — urban vs rural weighted box summaries
# ==============================================================================

dist_2025_base <- ENIGHUR2025_HOGARES_AGREGADOS |>
  transmute(
    Identif_hog,
    fexp = as.numeric(Fexp),
    area = ifelse(AREA == 1, "Urbana", "Rural"),
    `Ingreso monetario` = as.numeric(ing_mon_cor),
    `Gasto monetario total` = as.numeric(gas_mon_cor)
  ) |>
  left_join(gasolina_hmo_2025, by = "Identif_hog") |>
  mutate(gasolina_mon = coalesce(as.numeric(gasolina_mon), 0))

dist_2025_chart <- bind_rows(
  dist_2025_base |>
    transmute(fexp, area, indicador = "Ingreso monetario", valor = .data$`Ingreso monetario`),
  dist_2025_base |>
    transmute(fexp, area, indicador = "Gasto monetario total", valor = .data$`Gasto monetario total`)
) |>
  filter(!is.na(valor), valor > 0)

weighted_box <- function(x, w) {
  c(
    ymin = wtd_quantile(x, w, 0.05),
    lower = wtd_quantile(x, w, 0.25),
    middle = wtd_quantile(x, w, 0.50),
    upper = wtd_quantile(x, w, 0.75),
    ymax = wtd_quantile(x, w, 0.95)
  )
}

box_summary_2025 <- dist_2025_chart |>
  group_by(indicador, area) |>
  reframe(
    ymin = wtd_quantile(valor, fexp, 0.10),
    lower = wtd_quantile(valor, fexp, 0.25),
    middle = wtd_quantile(valor, fexp, 0.50),
    upper = wtd_quantile(valor, fexp, 0.75),
    ymax = wtd_quantile(valor, fexp, 0.90)
  ) |>
  mutate(
    indicador = factor(indicador, levels = c("Ingreso monetario", "Gasto monetario total")),
    area = factor(area, levels = c("Urbana", "Rural")),
    mediana_lbl = paste0("$", formatC(round(middle), format = "d", big.mark = ","))
  )

dist_2025_plot <- ggplot(
  box_summary_2025,
  aes(x = indicador, fill = area, colour = area)
) +
  geom_boxplot(
    aes(
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    position = position_dodge2(width = 0.72, preserve = "single"),
    width = 0.56,
    alpha = 0.55,
    linewidth = 0.8
  ) +
  geom_text(
    aes(y = middle, label = mediana_lbl, group = area),
    position = position_dodge2(width = 0.72, preserve = "single"),
    vjust = -0.9,
    size = 4.2,
    colour = "grey20"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Urbana" = "#2D6A9F",
      "Rural" = "#D4691E"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Urbana" = "#1A3A5C",
      "Rural" = "#A8400A"
    )
  ) +
  guides(
    fill = guide_legend(override.aes = list(colour = NA, linewidth = 0)),
    colour = "none"
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0.03, 0.18))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Ingreso o gasto mensual por hogar"
  ) +
  theme_quantificador_legend(legend.position = c(0.83, 0.83)) +
  theme(
    plot.margin = margin(12, 18, 8, 12)
  )

income_quintile_breaks_2025 <- vapply(c(0.20, 0.40, 0.60, 0.80), function(p) {
  wtd_quantile(dist_2025_base$`Ingreso monetario`, dist_2025_base$fexp, p)
}, numeric(1))

dist_2025_base$quintil_ingreso <- cut(
  dist_2025_base$`Ingreso monetario`,
  breaks = c(-Inf, income_quintile_breaks_2025, Inf),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  right = TRUE,
  include.lowest = TRUE
)

gasto_quintiles_chart <- bind_rows(
  transmute(
    dist_2025_base,
    grupo = as.character(quintil_ingreso),
    valor = .data$`Gasto monetario total`,
    fexp
  )
) |>
  filter(!is.na(grupo), !is.na(valor), valor > 0)

gasto_quintiles_summary <- gasto_quintiles_chart |>
  group_by(grupo) |>
  reframe(
    ymin = wtd_quantile(valor, fexp, 0.10),
    lower = wtd_quantile(valor, fexp, 0.25),
    middle = wtd_quantile(valor, fexp, 0.50),
    upper = wtd_quantile(valor, fexp, 0.75),
    ymax = wtd_quantile(valor, fexp, 0.90)
  ) |>
  mutate(
    grupo = factor(grupo, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")),
    mediana_lbl = paste0("$", formatC(round(middle), format = "d", big.mark = ","))
  )

quintile_axis_labels <- c(
  "Q1" = "Q1 (más pobre)",
  "Q2" = "Q2",
  "Q3" = "Q3",
  "Q4" = "Q4",
  "Q5" = "Q5 (más rico)"
)

gasto_quintiles_plot <- ggplot(
  gasto_quintiles_summary,
  aes(x = grupo, fill = grupo, colour = grupo)
) +
  geom_boxplot(
    aes(
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    width = 0.62,
    alpha = 0.6,
    linewidth = 0.8
  ) +
  geom_text(
    aes(y = middle, label = mediana_lbl),
    vjust = -0.45,
    size = 4.2,
    colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "Nacional" = "#5C6B73",
      "Q1" = "#D9E6F2",
      "Q2" = "#B8D2E8",
      "Q3" = "#8CBBD9",
      "Q4" = "#5499C7",
      "Q5" = "#1F618D"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Nacional" = "#3B4348",
      "Q1" = "#A7BED3",
      "Q2" = "#7DA6C2",
      "Q3" = "#5B8FB4",
      "Q4" = "#2E6E98",
      "Q5" = "#154360"
    )
  ) +
  guides(fill = "none", colour = "none") +
  scale_x_discrete(labels = quintile_axis_labels) +
  scale_y_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    breaks = scales::breaks_extended(n = 8),
    expand = expansion(mult = c(0.03, 0.22))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Quintiles de ingreso",
    y = "Gasto monetario total mensual por hogar"
  ) +
  theme_quantificador() +
  theme(
    plot.margin = margin(14, 16, 8, 12)
  )

gasolina_quintiles_plot_data <- dist_2025_base |>
  mutate(
    grupo = factor(
      as.character(quintil_ingreso),
      levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
      labels = c("Q1 (más pobre)", "Q2", "Q3", "Q4", "Q5 (más rico)")
    )
  ) |>
  filter(!is.na(grupo)) |>
  group_by(grupo) |>
  summarise(
    gasolina_promedio = sum(gasolina_mon * fexp, na.rm = TRUE) / sum(fexp, na.rm = TRUE),
    share_gasto_monetario = sum(gasolina_mon * fexp, na.rm = TRUE) / sum(`Gasto monetario total` * fexp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  bind_rows(
    dist_2025_base |>
      filter(!is.na(`Ingreso monetario`), !is.na(`Gasto monetario total`)) |>
      summarise(
        grupo = factor("Nacional", levels = c("Nacional", "Q1 (más pobre)", "Q2", "Q3", "Q4", "Q5 (más rico)")),
        gasolina_promedio = sum(gasolina_mon * fexp, na.rm = TRUE) / sum(fexp, na.rm = TRUE),
        share_gasto_monetario = sum(gasolina_mon * fexp, na.rm = TRUE) / sum(`Gasto monetario total` * fexp, na.rm = TRUE)
      )
  ) |>
  mutate(
    grupo = factor(as.character(grupo), levels = c("Nacional", "Q1 (más pobre)", "Q2", "Q3", "Q4", "Q5 (más rico)"))
  ) |>
  arrange(grupo)

gasolina_quintiles_plot <- ggplot(
  gasolina_quintiles_plot_data,
  aes(x = gasolina_promedio, y = grupo, fill = grupo)
) +
  geom_col(width = 0.62, alpha = 0.9, colour = NA) +
  geom_text(
    aes(label = paste0("$", formatC(round(gasolina_promedio, 1), format = "f", digits = 1), " | ", scales::percent(share_gasto_monetario, accuracy = 0.1))),
    hjust = -0.1,
    size = 4.2,
    colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "Nacional" = "#5C6B73",
      "Q1 (más pobre)" = "#D9E6F2",
      "Q2" = "#B8D2E8",
      "Q3" = "#8CBBD9",
      "Q4" = "#5499C7",
      "Q5 (más rico)" = "#1F618D"
    )
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    breaks = scales::breaks_extended(n = 7),
    expand = expansion(mult = c(0, 0.32))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Gasto mensual promedio en gasolina por hogar",
    y = "Quintiles de ingreso"
  ) +
  theme_quantificador() +
  theme(
    plot.margin = margin(8, 42, 8, 12)
  )

province_labels <- attr(ENIGHUR2025_HOGARES_AGREGADOS$PROVINCIA, "labels")

province_spending_plot_data <- ENIGHUR2025_HOGARES_AGREGADOS |>
  transmute(
    provincia = names(province_labels)[match(as.numeric(PROVINCIA), unname(province_labels))],
    fexp = as.numeric(Fexp),
    gasto_corriente_total = as.numeric(gas_cor_tot)
  ) |>
  filter(!is.na(provincia), !is.na(gasto_corriente_total), gasto_corriente_total > 0) |>
  group_by(provincia) |>
  summarise(
    mediana_gasto = wtd_quantile(gasto_corriente_total, fexp, 0.5),
    .groups = "drop"
  ) |>
  arrange(desc(mediana_gasto)) |>
  slice_head(n = 5) |>
  mutate(
    provincia = factor(provincia, levels = rev(provincia)),
    destaque = case_when(
      as.character(provincia) == "Galápagos" ~ "Galápagos",
      TRUE ~ "Resto"
    )
  )

national_median_spending <- wtd_quantile(
  as.numeric(ENIGHUR2025_HOGARES_AGREGADOS$gas_cor_tot),
  as.numeric(ENIGHUR2025_HOGARES_AGREGADOS$Fexp),
  0.5
)

province_spending_plot <- ggplot(
  province_spending_plot_data,
  aes(x = mediana_gasto, y = provincia, fill = destaque)
) +
  geom_col(width = 0.68, colour = NA) +
  geom_vline(
    xintercept = national_median_spending,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "#495057"
  ) +
  annotate(
    "text",
    x = national_median_spending,
    y = 0.55,
    label = paste0("Mediana nacional: $", formatC(round(national_median_spending), format = "d", big.mark = ",")),
    hjust = -0.02,
    vjust = 1,
    size = 4.2,
    colour = "#495057"
  ) +
  geom_text(
    aes(label = paste0("$", formatC(round(mediana_gasto), format = "d", big.mark = ","))),
    hjust = -0.08,
    size = 4.2,
    colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "Galápagos" = "#C94040",
      "Resto" = "#6FA8DC"
    )
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    labels = scales::label_dollar(prefix = "$", big.mark = ","),
    breaks = scales::breaks_extended(n = 8),
    expand = expansion(mult = c(0, 0.32))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Gasto corriente total mensual por hogar",
    y = NULL
  ) +
  theme_quantificador() +
  theme(
    plot.margin = margin(8, 42, 8, 12)
  )

region_labels <- attr(ENIGHUR2025_HOGARES_AGREGADOS$REGION, "labels")

regional_food_share_data <- ENIGHUR2025_HOGARES_AGREGADOS |>
  transmute(
    region = names(region_labels)[match(as.numeric(REGION), unname(region_labels))],
    fexp = as.numeric(Fexp),
    gasto_alimentos = as.numeric(d1),
    gasto_monetario_total = as.numeric(gas_mon_cor)
  ) |>
  filter(region %in% c("Costa", "Sierra", "Amazonía/Oriente")) |>
  group_by(region) |>
  summarise(
    gasto_alimentos_promedio = sum(gasto_alimentos * fexp, na.rm = TRUE) / sum(fexp, na.rm = TRUE),
    share_gasto_alimentos = sum(gasto_alimentos * fexp, na.rm = TRUE) / sum(gasto_monetario_total * fexp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    region = factor(region, levels = c("Costa", "Amazonía/Oriente", "Sierra")),
    etiqueta = paste0(
      scales::percent(share_gasto_alimentos, accuracy = 0.1),
      " | $",
      formatC(round(gasto_alimentos_promedio), format = "d", big.mark = ",")
    )
  )

regional_food_share_plot <- ggplot(
  regional_food_share_data,
  aes(x = share_gasto_alimentos, y = region, fill = region)
) +
  geom_col(width = 0.62, alpha = 0.9, colour = NA) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.08,
    size = 4.2,
    colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "Costa" = "#D4691E",
      "Amazonía/Oriente" = "#4F772D",
      "Sierra" = "#2D6A9F"
    )
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = scales::breaks_extended(n = 6),
    expand = expansion(mult = c(0, 0.30))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Porcentaje del gasto monetario total",
    y = NULL
  ) +
  theme_quantificador() +
  theme(
    plot.margin = margin(8, 38, 8, 12)
  )

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
social_width <- 6.5
social_height <- 5.2

save_figure("distribucion_ingreso_2025.png", income_distribution_2025_plot, width = social_width, height = social_height)
save_figure("distribucion_ingreso_gasto_nacional_rural_2025.png", dist_2025_plot, width = social_width, height = social_height)
save_figure("gasto_total_por_quintil_ingreso_2025.png", gasto_quintiles_plot, width = social_width, height = social_height)
save_figure("gasolina_promedio_por_quintil_2025.png", gasolina_quintiles_plot, width = social_width, height = social_height)
save_figure("gasto_total_mediano_provincia_2025.png", province_spending_plot, width = social_width, height = social_height)
save_figure("share_gasto_alimentos_region_2025.png", regional_food_share_plot, width = social_width, height = social_height)
cat("Guardado: output/figures/\n")

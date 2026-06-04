source("scripts/packages.R")

ensure_packages(c("openxlsx", "haven", "dplyr", "readxl"))

options(scipen = 999)

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

weighted_quantile <- function(x, w, p) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which.max(cumw >= p)]
}

weighted_total <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  sum(x[keep] * w[keep], na.rm = TRUE)
}

compute_sum1 <- function(data, vars) {
  present <- vars[vars %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_real_, nrow(data)))
  }

  tmp <- as.data.frame(lapply(data[present], as.numeric))
  out <- rowSums(tmp, na.rm = TRUE)
  all_missing <- rowSums(!is.na(tmp)) == 0
  out[all_missing] <- NA_real_
  out
}

parse_spss_compute_lines <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = " ")
  lines <- gsub("\t", " ", lines)

  exprs <- character()
  current <- NULL

  for (ln in lines) {
    ln2 <- trimws(ln)
    if (grepl("^COMPUTE\\s+", ln2, ignore.case = TRUE)) {
      current <- ln2
      if (grepl("\\.$", current)) {
        exprs <- c(exprs, current)
        current <- NULL
      }
    } else if (!is.null(current)) {
      current <- paste(current, ln2)
      if (grepl("\\.$", ln2)) {
        exprs <- c(exprs, current)
        current <- NULL
      }
    }
  }

  exprs
}

apply_spss_sum_syntax <- function(data, exprs) {
  for (expr in exprs) {
    expr <- sub("^COMPUTE\\s+", "", expr, ignore.case = TRUE)
    expr <- sub("\\.$", "", expr)
    parts <- strsplit(expr, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      next
    }

    lhs <- trimws(parts[1])
    rhs <- trimws(paste(parts[-1], collapse = "="))
    rhs <- gsub("SUMA?\\.1", "SUM.1", rhs, ignore.case = TRUE)

    if (grepl("^SUM\\.1\\s*\\(", rhs, ignore.case = TRUE)) {
      inside <- sub("^SUM\\.1\\s*\\(", "", rhs, ignore.case = TRUE)
      inside <- sub("\\)$", "", inside)
      vars <- trimws(unlist(strsplit(inside, ",")))
      vars <- vars[nzchar(vars)]
      data[[lhs]] <- compute_sum1(data, vars)
    } else {
      rhs_var <- trimws(rhs)
      if (rhs_var %in% names(data)) {
        data[[lhs]] <- as.numeric(data[[rhs_var]])
      }
    }
  }

  data
}

build_share_table <- function(data, survey_label, component_map, gasto_map, include_zone_split = FALSE) {
  original_table <- data.frame(
    encuesta = survey_label,
    nivel = "Original ENIGHUR",
    codigo = component_map$variable[-1],
    componente = component_map$componente[-1],
    weighted_total = vapply(
      component_map$variable[-1],
      function(var_name) weighted_total(as.numeric(data[[var_name]]), data$fexp),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  total_original <- sum(original_table$weighted_total, na.rm = TRUE)
  original_table$share_total <- original_table$weighted_total / total_original
  original_table$share_urbana <- NA_real_
  original_table$share_rural <- NA_real_

  if (include_zone_split) {
    total_original_urbana <- sum(vapply(
      component_map$variable[-1],
      function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Urbana"]), data$fexp[data$zona == "Urbana"]),
      numeric(1)
    ), na.rm = TRUE)
    total_original_rural <- sum(vapply(
      component_map$variable[-1],
      function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Rural"]), data$fexp[data$zona == "Rural"]),
      numeric(1)
    ), na.rm = TRUE)

    original_table$share_urbana <- vapply(
      component_map$variable[-1],
      function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Urbana"]), data$fexp[data$zona == "Urbana"]) / total_original_urbana,
      numeric(1)
    )
    original_table$share_rural <- vapply(
      component_map$variable[-1],
      function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Rural"]), data$fexp[data$zona == "Rural"]) / total_original_rural,
      numeric(1)
    )
  }

  custom_table <- lapply(unique(gasto_map$categoria_codigo), function(cat_code) {
    vars <- gasto_map$codigo_enighur[gasto_map$categoria_codigo == cat_code]
    total_values <- rowSums(data[, vars, drop = FALSE], na.rm = TRUE)

    out <- data.frame(
      encuesta = survey_label,
      nivel = "Agrupacion propia",
      codigo = cat_code,
      componente = unique(gasto_map$categoria_nombre[gasto_map$categoria_codigo == cat_code])[[1]],
      weighted_total = weighted_total(total_values, data$fexp),
      share_total = weighted_total(total_values, data$fexp) / total_original,
      share_urbana = NA_real_,
      share_rural = NA_real_,
      stringsAsFactors = FALSE
    )

    if (include_zone_split) {
      total_original_urbana <- sum(vapply(
        component_map$variable[-1],
        function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Urbana"]), data$fexp[data$zona == "Urbana"]),
        numeric(1)
      ), na.rm = TRUE)
      total_original_rural <- sum(vapply(
        component_map$variable[-1],
        function(var_name) weighted_total(as.numeric(data[[var_name]][data$zona == "Rural"]), data$fexp[data$zona == "Rural"]),
        numeric(1)
      ), na.rm = TRUE)

      out$share_urbana <- weighted_total(total_values[data$zona == "Urbana"], data$fexp[data$zona == "Urbana"]) / total_original_urbana
      out$share_rural <- weighted_total(total_values[data$zona == "Rural"], data$fexp[data$zona == "Rural"]) / total_original_rural
    }

    out
  }) |>
    dplyr::bind_rows()

  dplyr::bind_rows(original_table, custom_table)
}

build_share_table_from_totals <- function(original_totals, survey_label, gasto_map) {
  original_table <- data.frame(
    encuesta = survey_label,
    nivel = "Original ENIGHUR",
    codigo = original_totals$codigo,
    componente = original_totals$componente,
    weighted_total = original_totals$weighted_total,
    share_total = original_totals$weighted_total / sum(original_totals$weighted_total, na.rm = TRUE),
    share_urbana = NA_real_,
    share_rural = NA_real_,
    stringsAsFactors = FALSE
  )

  custom_table <- lapply(unique(gasto_map$categoria_codigo), function(cat_code) {
    vars <- gasto_map$codigo_enighur[gasto_map$categoria_codigo == cat_code]
    total_value <- sum(original_totals$weighted_total[match(vars, original_totals$codigo)], na.rm = TRUE)
    data.frame(
      encuesta = survey_label,
      nivel = "Agrupacion propia",
      codigo = cat_code,
      componente = unique(gasto_map$categoria_nombre[gasto_map$categoria_codigo == cat_code])[[1]],
      weighted_total = total_value,
      share_total = total_value / sum(original_totals$weighted_total, na.rm = TRUE),
      share_urbana = NA_real_,
      share_rural = NA_real_,
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  dplyr::bind_rows(original_table, custom_table)
}

median_by_zone <- function(data, value_col) {
  data.frame(
    mediana_total = weighted_quantile(data[[value_col]], data$fexp, 0.5),
    mediana_urbana = weighted_quantile(data[[value_col]][data$zona == "Urbana"], data$fexp[data$zona == "Urbana"], 0.5),
    mediana_rural = weighted_quantile(data[[value_col]][data$zona == "Rural"], data$fexp[data$zona == "Rural"], 0.5)
  )
}

apply_sheet_style <- function(wb, sheet, data, currency_cols = NULL) {
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#DCE6F1",
    border = "Bottom"
  )
  currency_style <- openxlsx::createStyle(numFmt = "$#,##0")

  openxlsx::writeDataTable(wb, sheet, data, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::addStyle(wb, sheet, header_style, rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(data)), widths = "auto")

  if (!is.null(currency_cols)) {
    openxlsx::addStyle(
      wb, sheet, currency_style,
      rows = 2:(nrow(data) + 1),
      cols = currency_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
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

hogares <- ENIGHUR2025_HOGARES_AGREGADOS
hogares$zona <- ifelse(hogares$AREA == 1, "Urbana", "Rural")
hogares$fexp <- as.numeric(hogares$Fexp)
hogares$ahorro_mon <- as.numeric(hogares$ing_mon_cor) - as.numeric(hogares$gas_mon_cor)
province_labels <- attr(hogares$PROVINCIA, "labels")
hogares$provincia_nombre <- names(province_labels)[match(as.numeric(hogares$PROVINCIA), unname(province_labels))]

map_path <- resolve_existing_path(
  file.path("data", "intermediate", "mapeo_categorias_gasto.csv"),
  "Mapping gasto categorias"
)

gasto_map <- utils::read.csv(map_path, stringsAsFactors = FALSE)

component_map <- data.frame(
  variable = c("gas_cor_tot", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13"),
  componente = c(
    "Gasto corriente total del hogar",
    "Alimentos y bebidas no alcohólicas",
    "Bebidas alcohólicas, tabaco y estupefacientes",
    "Prendas de vestir y calzado",
    "Vivienda, agua, electricidad, gas y otros combustibles",
    "Muebles, artículos para el hogar y conservación ordinaria del hogar",
    "Salud",
    "Transporte",
    "Información y comunicación",
    "Recreación, deporte y cultura",
    "Servicios educativos",
    "Servicios de restaurantes y alojamientos",
    "Seguros y servicios financieros",
    "Cuidado personal, previsión social y bienes y servicios diversos"
  ),
  stringsAsFactors = FALSE
)

median_rows <- lapply(seq_len(nrow(component_map)), function(i) {
  var_name <- component_map$variable[[i]]
  medians <- median_by_zone(hogares, var_name)
  data.frame(
    componente = component_map$componente[[i]],
    variable = var_name,
    medians,
    stringsAsFactors = FALSE
  )
})

median_table <- dplyr::bind_rows(median_rows)

province_median_table <- {
  category_order <- gasto_map |>
    dplyr::distinct(.data$categoria_codigo, .data$categoria_nombre, .data$orden) |>
    dplyr::arrange(.data$orden)

  for (i in seq_len(nrow(category_order))) {
    cat_code <- category_order$categoria_codigo[[i]]
    vars <- gasto_map$codigo_enighur[gasto_map$categoria_codigo == cat_code]
    hogares[[paste0("cat_", cat_code)]] <- rowSums(hogares[, vars, drop = FALSE], na.rm = TRUE)
  }

  component_cols <- c("gas_cor_tot", paste0("cat_", category_order$categoria_codigo))
  output_names <- c("mediana_gasto_total", paste0("mediana_", category_order$categoria_codigo))

  make_province_row <- function(label, subset_data) {
    medians <- vapply(
      component_cols,
      function(var_name) weighted_quantile(as.numeric(subset_data[[var_name]]), subset_data$fexp, 0.5),
      numeric(1)
    )

    out <- data.frame(provincia = label, stringsAsFactors = FALSE)
    for (j in seq_along(output_names)) {
      out[[output_names[[j]]]] <- medians[[j]]
    }
    out
  }

  province_levels <- sort(unique(stats::na.omit(hogares$provincia_nombre)))

  dplyr::bind_rows(
    make_province_row("Nacional", hogares),
    dplyr::bind_rows(lapply(province_levels, function(prov) {
      make_province_row(prov, hogares[hogares$provincia_nombre == prov, , drop = FALSE])
    }))
  )
}

ingreso_gasto_total_zona <- {
  make_zone_summary <- function(label, subset_data) {
    data.frame(
      zona = label,
      indicador = c("Gasto corriente total del hogar", "Ingreso monetario total del hogar"),
      weighted_mean = c(
        weighted_total(as.numeric(subset_data$gas_cor_tot), subset_data$fexp) / sum(subset_data$fexp, na.rm = TRUE),
        weighted_total(as.numeric(subset_data$ing_mon_cor), subset_data$fexp) / sum(subset_data$fexp, na.rm = TRUE)
      ),
      weighted_median = c(
        weighted_quantile(as.numeric(subset_data$gas_cor_tot), subset_data$fexp, 0.5),
        weighted_quantile(as.numeric(subset_data$ing_mon_cor), subset_data$fexp, 0.5)
      ),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(
    make_zone_summary("Total", hogares),
    make_zone_summary("Urbana", hogares[hogares$zona == "Urbana", , drop = FALSE]),
    make_zone_summary("Rural", hogares[hogares$zona == "Rural", , drop = FALSE])
  )
}

ingreso_agricola_table <- {
  ingreso_data <- hogares |>
    dplyr::transmute(
      zona = .data$zona,
      fexp = .data$fexp,
      ingreso_agricola = dplyr::coalesce(as.numeric(.data$ing_ag_mon_neto), 0),
      ingreso_no_agricola = as.numeric(.data$ing_mon_cor) - dplyr::coalesce(as.numeric(.data$ing_ag_mon_neto), 0),
      ingreso_total_monetario = as.numeric(.data$ing_mon_cor)
    )

  make_income_row <- function(label, subset_data) {
    total_ag <- weighted_total(subset_data$ingreso_agricola, subset_data$fexp)
    total_non_ag <- weighted_total(subset_data$ingreso_no_agricola, subset_data$fexp)
    total_income <- weighted_total(subset_data$ingreso_total_monetario, subset_data$fexp)

    dplyr::bind_rows(
      data.frame(
        zona = label,
        componente_ingreso = "Ingreso agricola neto",
        weighted_mean = total_ag / sum(subset_data$fexp, na.rm = TRUE),
        weighted_median = weighted_quantile(subset_data$ingreso_agricola, subset_data$fexp, 0.5),
        share_of_monetary_income = total_ag / total_income,
        stringsAsFactors = FALSE
      ),
      data.frame(
        zona = label,
        componente_ingreso = "Ingreso no agricola",
        weighted_mean = total_non_ag / sum(subset_data$fexp, na.rm = TRUE),
        weighted_median = weighted_quantile(subset_data$ingreso_no_agricola, subset_data$fexp, 0.5),
        share_of_monetary_income = total_non_ag / total_income,
        stringsAsFactors = FALSE
      ),
      data.frame(
        zona = label,
        componente_ingreso = "Ingreso monetario total",
        weighted_mean = total_income / sum(subset_data$fexp, na.rm = TRUE),
        weighted_median = weighted_quantile(subset_data$ingreso_total_monetario, subset_data$fexp, 0.5),
        share_of_monetary_income = 1,
        stringsAsFactors = FALSE
      )
    )
  }

  dplyr::bind_rows(
    make_income_row("Total", ingreso_data),
    make_income_row("Urbana", ingreso_data[ingreso_data$zona == "Urbana", , drop = FALSE]),
    make_income_row("Rural", ingreso_data[ingreso_data$zona == "Rural", , drop = FALSE])
  )
}

# ENIGHUR income percentiles
income_percentile_probs <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
income_percentile_labels <- c("p10", "p25", "p50", "p75", "p90", "p95", "p99")

enighur_2025_income <- hogares |>
  dplyr::transmute(
    fexp = as.numeric(.data$Fexp),
    ing_mon_cor = as.numeric(.data$ing_mon_cor)
  ) |>
  dplyr::filter(!is.na(.data$ing_mon_cor), .data$ing_mon_cor > 0)

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

gasto_syntax_path_2012 <- resolve_existing_path(
  file.path("data", "enighur", "2012", "04_SINTAXIS", "04 SINTAXIS", "AGREGADO DEL GASTO.sps"),
  "Sintaxis 2012 AGREGADO DEL GASTO"
)

gas_ag_2012 <- haven::read_sav(gastos_hmo_path) |>
  dplyr::mutate(gas_ag = rowSums(cbind(c1703097, c1704097, c1705097, c1706097), na.rm = TRUE)) |>
  dplyr::select(Identif_hog, gas_ag)

enighur_2012_income <- haven::read_sav(data_path_2012) |>
  dplyr::left_join(gas_ag_2012, by = "Identif_hog") |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(!is.na(.) & . < 0, 0, .))) |>
  dplyr::mutate(
    suel_sal_bruto = rowSums(cbind(i1401001, i1401002, i1401003, i1401004, i1401005, i1401006,
                                   i1401007, i1401008, i1401009, i1401010, i1401011, i1401012,
                                   i1401013, i1401014, i1401015, i1401016, i1401017, i1401018), na.rm = TRUE),
    ded_asal = rowSums(cbind(i1701001, i1701002), na.rm = TRUE),
    ing_otro_neto = rowSums(cbind(i1404001, i1404002, i1404003, i1404005, i1404006), na.rm = TRUE),
    ing_asal_mon_net = pmax(suel_sal_bruto - ded_asal + ing_otro_neto, 0),
    ing_cuent_prop_na = dplyr::coalesce(as.numeric(i1407099), 0),
    ag_rev = rowSums(cbind(i1408097, i1409097, i1416097, i1421097,
                           i1424097, i1428097, i1431097, i1436097), na.rm = TRUE),
    gas_ag = dplyr::coalesce(gas_ag, 0),
    i1432097 = ifelse(ag_rev >= gas_ag, ag_rev, gas_ag),
    ing_ag_mon_neto = i1432097 - gas_ag,
    ded_ind = dplyr::coalesce(as.numeric(i1709002), 0),
    ing_ind_mon_net = pmax(ing_cuent_prop_na + ing_ag_mon_neto - ded_ind, 0),
    ing_ter_ocu = dplyr::coalesce(as.numeric(a1443001), 0),
    ing_trab_mon = ing_asal_mon_net + ing_ind_mon_net + ing_ter_ocu,
    ing_ren_prop = rowSums(cbind(dplyr::na_if(i1445004, 0), dplyr::na_if(i1445006, 0), dplyr::na_if(i1445007, 0)), na.rm = TRUE),
    ing_cap = rowSums(cbind(dplyr::na_if(i1445001, 0), dplyr::na_if(i1445002, 0),
                            dplyr::na_if(i1445003, 0), dplyr::na_if(i1445005, 0)), na.rm = TRUE),
    ing_ren_prop_cap = ing_ren_prop + ing_cap,
    tranf_cor = rowSums(cbind(i1444001, i1444002, i1444003, i1444004,
                              i1444005, i1444006, i1444007), na.rm = TRUE),
    otro_ing_cor = dplyr::coalesce(as.numeric(b1443001), 0),
    ing_mon_cor = ing_trab_mon + ing_ren_prop_cap + tranf_cor + otro_ing_cor
  ) |>
  dplyr::transmute(
    fexp = as.numeric(.data$Fexp_cen2010),
    ing_mon_cor = as.numeric(.data$ing_mon_cor) * (113.6774 / 90.0032)
  ) |>
  dplyr::filter(!is.na(.data$ing_mon_cor), .data$ing_mon_cor > 0)

tabulados_2012_path <- resolve_existing_path(
  file.path("data", "enighur", "2012", "TABULADOS ENIGHUR 2011-2012.xlsx"),
  "Tabulados ENIGHUR 2011-2012"
)

tabulados_2012 <- readxl::read_excel(tabulados_2012_path, sheet = "Estructura del gasto (37)", col_names = FALSE)
original_totals_2012 <- data.frame(
  codigo = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12"),
  componente = c(
    "Alimentos y bebidas no alcohólicas",
    "Bebidas alcohólicas, tabaco y estupefacientes",
    "Prendas de vestir y calzado",
    "Alojamiento, agua, electricidad, gas y otros combustibles",
    "Muebles, artículos para el hogar y para la conservación ordinaria del hogar",
    "Salud",
    "Transporte",
    "Comunicaciones",
    "Recreación y cultura",
    "Educación",
    "Restaurantes y hoteles",
    "Bienes y servicios diversos"
  ),
  weighted_total = as.numeric(unlist(tabulados_2012[11:22, 3])),
  stringsAsFactors = FALSE
)

share_table <- dplyr::bind_rows(
  build_share_table(hogares, "ENIGHUR 2024-2025", component_map, gasto_map, include_zone_split = TRUE),
  build_share_table_from_totals(original_totals_2012, "ENIGHUR 2011-2012", gasto_map)
) |>
  dplyr::arrange(.data$encuesta, .data$nivel, dplyr::desc(.data$share_total))

# ============================================================
# Ahorro (savings) by zone — household level for BOTH surveys
# Consistent with the sankey definition:
# ahorro monetario = ingreso monetario corriente - gasto monetario corriente
# 2025: uses pre-computed gas_mon_cor from the ENIGHUR household base
# 2012: reconstructs gas_mon_cor following the official INEC SPSS syntax
#        gas_mon_cor = gas_gru_cor + ot_gas_mon
# Both survey rounds are expressed at 2024-2025 prices.
# ============================================================

deflator_2012 <- 113.6774 / 90.0032

make_ahorro_row <- function(encuesta_label, zona_label, subset_data) {
  data.frame(
    encuesta      = encuesta_label,
    zona          = zona_label,
    media_ahorro  = weighted_total(subset_data$ahorro_mon, subset_data$fexp) /
      sum(subset_data$fexp, na.rm = TRUE),
    mediana_ahorro = weighted_quantile(subset_data$ahorro_mon, subset_data$fexp, 0.5),
    stringsAsFactors = FALSE
  )
}

ahorro_2025 <- dplyr::bind_rows(
  make_ahorro_row("ENIGHUR 2024-2025", "Total",  hogares),
  make_ahorro_row("ENIGHUR 2024-2025", "Urbana", hogares[hogares$zona == "Urbana", , drop = FALSE]),
  make_ahorro_row("ENIGHUR 2024-2025", "Rural",  hogares[hogares$zona == "Rural",  , drop = FALSE])
)

# Re-compute 2012 income keeping Identif_hog and zone variable (Área).
ingresos_2012_zona <- haven::read_sav(data_path_2012) |>
  dplyr::left_join(gas_ag_2012, by = "Identif_hog") |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(!is.na(.) & . < 0, 0, .))) |>
  dplyr::mutate(
    suel_sal_bruto   = rowSums(cbind(i1401001,i1401002,i1401003,i1401004,i1401005,i1401006,
                                     i1401007,i1401008,i1401009,i1401010,i1401011,i1401012,
                                     i1401013,i1401014,i1401015,i1401016,i1401017,i1401018), na.rm=TRUE),
    ded_asal         = rowSums(cbind(i1701001, i1701002), na.rm=TRUE),
    ing_otro_neto    = rowSums(cbind(i1404001,i1404002,i1404003,i1404005,i1404006), na.rm=TRUE),
    ing_asal_mon_net = pmax(suel_sal_bruto - ded_asal + ing_otro_neto, 0),
    ing_cuent_prop_na = dplyr::coalesce(as.numeric(i1407099), 0),
    ag_rev           = rowSums(cbind(i1408097,i1409097,i1416097,i1421097,
                                     i1424097,i1428097,i1431097,i1436097), na.rm=TRUE),
    gas_ag           = dplyr::coalesce(gas_ag, 0),
    i1432097         = ifelse(ag_rev >= gas_ag, ag_rev, gas_ag),
    ing_ag_mon_neto  = i1432097 - gas_ag,
    ded_ind          = dplyr::coalesce(as.numeric(i1709002), 0),
    ing_ind_mon_net  = pmax(ing_cuent_prop_na + ing_ag_mon_neto - ded_ind, 0),
    ing_ter_ocu      = dplyr::coalesce(as.numeric(a1443001), 0),
    ing_trab_mon     = ing_asal_mon_net + ing_ind_mon_net + ing_ter_ocu,
    ing_ren_prop     = rowSums(cbind(dplyr::na_if(i1445004,0), dplyr::na_if(i1445006,0),
                                     dplyr::na_if(i1445007,0)), na.rm=TRUE),
    ing_cap          = rowSums(cbind(dplyr::na_if(i1445001,0), dplyr::na_if(i1445002,0),
                                     dplyr::na_if(i1445003,0), dplyr::na_if(i1445005,0)), na.rm=TRUE),
    ing_ren_prop_cap = ing_ren_prop + ing_cap,
    tranf_cor        = rowSums(cbind(i1444001,i1444002,i1444003,i1444004,
                                     i1444005,i1444006,i1444007), na.rm=TRUE),
    otro_ing_cor     = dplyr::coalesce(as.numeric(b1443001), 0),
    ing_mon_cor      = ing_trab_mon + ing_ren_prop_cap + tranf_cor + otro_ing_cor,
    ot_gas_mon       = rowSums(cbind(i1709001, i1709003, i1709004, i1709005,
                                     i1709006, i1709007, i1709008), na.rm = TRUE)
  ) |>
  dplyr::transmute(
    Identif_hog = .data$Identif_hog,
    fexp        = as.numeric(.data$Fexp_cen2010),
    zona        = ifelse(as.numeric(.data$`Área`) == 1, "Urbana", "Rural"),
    ing_mon_cor = as.numeric(.data$ing_mon_cor) * deflator_2012,
    ot_gas_mon  = as.numeric(.data$ot_gas_mon)
  )

gastos_2012_monetario <- haven::read_sav(gastos_hmo_path) |>
  as.data.frame()

gasto_exprs_2012 <- parse_spss_compute_lines(gasto_syntax_path_2012)
gasto_exprs_2012 <- gasto_exprs_2012[grepl("^(COMPUTE\\s+)?(c|g|d|gas_gru_cor)", gasto_exprs_2012, ignore.case = TRUE)]
gastos_2012_monetario <- apply_spss_sum_syntax(gastos_2012_monetario, gasto_exprs_2012) |>
  dplyr::transmute(
    Identif_hog = .data$Identif_hog,
    gas_gru_cor = as.numeric(.data$gas_gru_cor)
  )

ahorro_2012_hh <- ingresos_2012_zona |>
  dplyr::left_join(gastos_2012_monetario, by = "Identif_hog") |>
  dplyr::mutate(
    gas_mon_cor = (as.numeric(.data$gas_gru_cor) + .data$ot_gas_mon) * deflator_2012,
    ahorro_mon = .data$ing_mon_cor - .data$gas_mon_cor
  ) |>
  dplyr::filter(!is.na(.data$ahorro_mon))

ahorro_2012 <- dplyr::bind_rows(
  make_ahorro_row("ENIGHUR 2011-2012 (precios 2024-2025)", "Total",  ahorro_2012_hh),
  make_ahorro_row("ENIGHUR 2011-2012 (precios 2024-2025)", "Urbana", ahorro_2012_hh[ahorro_2012_hh$zona == "Urbana", , drop = FALSE]),
  make_ahorro_row("ENIGHUR 2011-2012 (precios 2024-2025)", "Rural",  ahorro_2012_hh[ahorro_2012_hh$zona == "Rural",  , drop = FALSE])
)

ahorro_table <- dplyr::bind_rows(ahorro_2025, ahorro_2012)

income_percentiles <- dplyr::bind_rows(
  data.frame(
    encuesta = "ENIGHUR 2011-2012 ajustada a precios 2024-2025",
    percentil = income_percentile_labels,
    valor = vapply(income_percentile_probs, function(p) weighted_quantile(enighur_2012_income$ing_mon_cor, enighur_2012_income$fexp, p), numeric(1)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    encuesta = "ENIGHUR 2024-2025",
    percentil = income_percentile_labels,
    valor = vapply(income_percentile_probs, function(p) weighted_quantile(enighur_2025_income$ing_mon_cor, enighur_2025_income$fexp, p), numeric(1)),
    stringsAsFactors = FALSE
  )
)

notes_table <- data.frame(
  item = c(
    "source",
    "zone_definition",
    "statistic",
    "coverage",
    "share_definition",
    "agricultural_income_definition",
    "ahorro_definition_2025",
    "ahorro_definition_2012"
  ),
  detail = c(
    "ENIGHUR 2024-2025 household working base.",
    "AREA = 1 treated as Urbana; AREA = 2 treated as Rural.",
    "All values are weighted medians using the household expansion factor.",
    "Includes total current expenditure and the 13 main expenditure components.",
    "Expenditure shares are weighted totals divided by the sum of the 13 original ENIGHUR consumption components. For 2024-2025, urban and rural shares are also shown.",
    "Ingreso no agricola is defined here as ingreso monetario total minus ingreso agricola neto.",
    "ENIGHUR 2024-2025 savings: monetary savings = ing_mon_cor - gas_mon_cor (monetary current income minus monetary current spending). Weighted mean and median at household level.",
    "ENIGHUR 2011-2012 savings: monetary savings = reconstructed ing_mon_cor minus reconstructed gas_mon_cor, following the official INEC SPSS syntax. Monetary spending is gas_gru_cor + ot_gas_mon, using d1-d12 from GASTOS_HMO plus non-consumption monetary spending from INGRESOS_H. Both deflated to 2024-2025 prices (IPC factor 1.2631). Weighted mean and median at household level."
  ),
  stringsAsFactors = FALSE
)

output_path <- file.path("output", "tables", "enighur_2025_mediana_gasto_componentes.xlsx")
output_path <- file.path("output", "tables", "enighur_ingresos_y_gastos_resumen.xlsx")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "medianas")
apply_sheet_style(wb, "medianas", median_table, currency_cols = 3:5)

openxlsx::addWorksheet(wb, "gasto_provincia")
apply_sheet_style(
  wb, "gasto_provincia", province_median_table,
  currency_cols = 2:ncol(province_median_table)
)

openxlsx::addWorksheet(wb, "ingreso_gasto_total_zona")
apply_sheet_style(wb, "ingreso_gasto_total_zona", ingreso_gasto_total_zona, currency_cols = 3:4)

openxlsx::addWorksheet(wb, "percentiles_ingreso")
apply_sheet_style(wb, "percentiles_ingreso", income_percentiles, currency_cols = 3)

openxlsx::addWorksheet(wb, "shares_comparison")
apply_sheet_style(wb, "shares_comparison", share_table, currency_cols = 4)
openxlsx::addStyle(
  wb, "shares_comparison", openxlsx::createStyle(numFmt = "0.0%"),
  rows = 2:(nrow(share_table) + 1), cols = 5:7, gridExpand = TRUE, stack = TRUE
)

openxlsx::addWorksheet(wb, "ingreso_agricola")
apply_sheet_style(wb, "ingreso_agricola", ingreso_agricola_table, currency_cols = 3:4)
openxlsx::addStyle(
  wb, "ingreso_agricola", openxlsx::createStyle(numFmt = "0.0%"),
  rows = 2:(nrow(ingreso_agricola_table) + 1), cols = 5, gridExpand = TRUE, stack = TRUE
)

openxlsx::addWorksheet(wb, "ahorro")
apply_sheet_style(wb, "ahorro", ahorro_table, currency_cols = 3:4)

openxlsx::addWorksheet(wb, "notes")
apply_sheet_style(wb, "notes", notes_table)

openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

cat("Guardado:", output_path, "\n")
print(median_table)

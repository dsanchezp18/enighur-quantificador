source("scripts/packages.R")

ensure_packages(c("haven", "dplyr", "openxlsx"))

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
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which.max(cumw >= p)]
}

weighted_quantile_table <- function(data, value_col, weight_col, probs, label_prefix) {
  values <- vapply(
    probs,
    function(p) weighted_quantile(data[[value_col]], data[[weight_col]], p),
    numeric(1)
  )

  data.frame(
    quantile_group = label_prefix,
    probability = probs,
    threshold = values,
    stringsAsFactors = FALSE
  )
}

five_number_summary <- function(data, survey_name, measure_name, value_col, weight_col) {
  probs <- c(0, 0.25, 0.5, 0.75, 1)
  labels <- c("min", "q1", "median", "q3", "max")
  values <- vapply(
    probs,
    function(p) weighted_quantile(data[[value_col]], data[[weight_col]], p),
    numeric(1)
  )

  data.frame(
    survey = survey_name,
    income_measure = measure_name,
    statistic = labels,
    probability = probs,
    value = values,
    stringsAsFactors = FALSE
  )
}

overview_row <- function(data, survey_name, measure_name, value_col, weight_col) {
  data.frame(
    survey = survey_name,
    income_measure = measure_name,
    households = nrow(data),
    weighted_mean = weighted.mean(data[[value_col]], data[[weight_col]]),
    weighted_median = weighted_quantile(data[[value_col]], data[[weight_col]], 0.5),
    weighted_share_1500_plus = weighted.mean(data[[value_col]] >= 1500, data[[weight_col]]),
    stringsAsFactors = FALSE
  )
}

apply_sheet_style <- function(wb, sheet, data, currency_cols = NULL, share_cols = NULL) {
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#DCE6F1",
    border = "Bottom"
  )
  currency_style <- openxlsx::createStyle(numFmt = "$#,##0")
  share_style <- openxlsx::createStyle(numFmt = "0.0%")

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

  if (!is.null(share_cols)) {
    openxlsx::addStyle(
      wb, sheet, share_style,
      rows = 2:(nrow(data) + 1),
      cols = share_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

# ENIGHUR 2025
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

enighur_2025 <- ENIGHUR2025_HOGARES_AGREGADOS |>
  dplyr::transmute(
    fexp = as.numeric(.data$Fexp),
    hh_income = as.numeric(.data$ing_mon_cor)
  ) |>
  dplyr::filter(!is.na(.data$hh_income), .data$hh_income > 0)

# ENEMDU 2025
enemdu_personas_path <- resolve_existing_path(
  file.path("data", "enemdu", "1_BDD_ENEMDU_2025_SPSS", "BDDenemdu_personas_2025_anual.sav"),
  "ENEMDU 2025 personas"
)

enemdu_personas <- haven::read_sav(
  enemdu_personas_path,
  col_select = c(id_hogar, fexp, ingpc, ingrl)
)

enemdu_hogares <- enemdu_personas |>
  dplyr::transmute(
    id_hogar = as.character(.data$id_hogar),
    fexp = as.numeric(.data$fexp),
    ingpc = as.numeric(.data$ingpc),
    ingrl_raw = as.numeric(.data$ingrl),
    ingrl_clean = dplyr::case_when(
      is.na(.data$ingrl_raw) ~ 0,
      .data$ingrl_raw == 999999 ~ NA_real_,
      TRUE ~ .data$ingrl_raw
    )
  ) |>
  dplyr::group_by(.data$id_hogar) |>
  dplyr::summarise(
    fexp = dplyr::first(.data$fexp),
    hh_size = dplyr::n(),
    ingpc = dplyr::first(.data$ingpc),
    has_complete_income = !is.na(dplyr::first(.data$ingpc)),
    has_complete_labor_income = !any(.data$ingrl_raw == 999999, na.rm = TRUE),
    hh_income_sum_ingrl = sum(.data$ingrl_clean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    hh_income_total = .data$ingpc * .data$hh_size
  )

enemdu_total <- enemdu_hogares |>
  dplyr::filter(.data$has_complete_income) |>
  dplyr::transmute(
    fexp = .data$fexp,
    hh_income = .data$hh_income_total
  )

enemdu_labor <- enemdu_hogares |>
  dplyr::filter(.data$has_complete_labor_income) |>
  dplyr::transmute(
    fexp = .data$fexp,
    hh_income = .data$hh_income_sum_ingrl
  )

overview_table <- dplyr::bind_rows(
  overview_row(enighur_2025, "ENIGHUR 2025", "Ingreso monetario del hogar", "hh_income", "fexp"),
  overview_row(enemdu_total, "ENEMDU 2025 anual", "Ingreso total del hogar derivado de ingpc", "hh_income", "fexp"),
  overview_row(enemdu_labor, "ENEMDU 2025 anual", "Suma del ingreso laboral dentro del hogar", "hh_income", "fexp")
)

five_number_table <- dplyr::bind_rows(
  five_number_summary(enighur_2025, "ENIGHUR 2025", "Ingreso monetario del hogar", "hh_income", "fexp"),
  five_number_summary(enemdu_total, "ENEMDU 2025 anual", "Ingreso total del hogar derivado de ingpc", "hh_income", "fexp"),
  five_number_summary(enemdu_labor, "ENEMDU 2025 anual", "Suma del ingreso laboral dentro del hogar", "hh_income", "fexp")
)

quintiles_table <- dplyr::bind_rows(
  cbind(
    survey = "ENIGHUR 2025",
    income_measure = "Ingreso monetario del hogar",
    weighted_quantile_table(enighur_2025, "hh_income", "fexp", seq(0.2, 1, by = 0.2), "quintile")
  ),
  cbind(
    survey = "ENEMDU 2025 anual",
    income_measure = "Ingreso total del hogar derivado de ingpc",
    weighted_quantile_table(enemdu_total, "hh_income", "fexp", seq(0.2, 1, by = 0.2), "quintile")
  ),
  cbind(
    survey = "ENEMDU 2025 anual",
    income_measure = "Suma del ingreso laboral dentro del hogar",
    weighted_quantile_table(enemdu_labor, "hh_income", "fexp", seq(0.2, 1, by = 0.2), "quintile")
  )
)

deciles_table <- dplyr::bind_rows(
  cbind(
    survey = "ENIGHUR 2025",
    income_measure = "Ingreso monetario del hogar",
    weighted_quantile_table(enighur_2025, "hh_income", "fexp", seq(0.1, 1, by = 0.1), "decile")
  ),
  cbind(
    survey = "ENEMDU 2025 anual",
    income_measure = "Ingreso total del hogar derivado de ingpc",
    weighted_quantile_table(enemdu_total, "hh_income", "fexp", seq(0.1, 1, by = 0.1), "decile")
  ),
  cbind(
    survey = "ENEMDU 2025 anual",
    income_measure = "Suma del ingreso laboral dentro del hogar",
    weighted_quantile_table(enemdu_labor, "hh_income", "fexp", seq(0.1, 1, by = 0.1), "decile")
  )
)

notes_table <- data.frame(
  item = c(
    "enighur_measure",
    "enemdu_total_measure",
    "enemdu_labor_measure",
    "weighting",
    "cutoff_share"
  ),
  detail = c(
    "ENIGHUR 2025 uses household monetary income from the working household base.",
    "ENEMDU total household income is derived as ingpc multiplied by household size.",
    "ENEMDU labor household income sums person-level ingrl within each household; households with ingrl = 999999 are excluded.",
    "All summaries, quintiles, and deciles are weighted using the household expansion factor.",
    "Overview includes the weighted share of households with income >= $1,500."
  ),
  stringsAsFactors = FALSE
)

output_path <- file.path("output", "tables", "resumen_ingresos_hogares_2025.xlsx")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "overview")
apply_sheet_style(wb, "overview", overview_table, currency_cols = 4:5, share_cols = 6)

openxlsx::addWorksheet(wb, "five_number")
apply_sheet_style(wb, "five_number", five_number_table, currency_cols = 5)

openxlsx::addWorksheet(wb, "quintiles")
apply_sheet_style(wb, "quintiles", quintiles_table, currency_cols = 5)

openxlsx::addWorksheet(wb, "deciles")
apply_sheet_style(wb, "deciles", deciles_table, currency_cols = 5)

openxlsx::addWorksheet(wb, "notes")
apply_sheet_style(wb, "notes", notes_table)

openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

cat("Resumen guardado en:", output_path, "\n")
print(overview_table)

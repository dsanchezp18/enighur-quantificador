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
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumw <- cumsum(w) / sum(w)
  x[which.max(cumw >= p)]
}

enemdu_personas_path <- resolve_existing_path(
  file.path("data", "enemdu", "1_BDD_ENEMDU_2025_SPSS", "BDDenemdu_personas_2025_anual.sav"),
  "ENEMDU 2025 personas"
)

enemdu_personas <- haven::read_sav(
  enemdu_personas_path,
  col_select = c(id_hogar, fexp, ingpc)
)

# Consistency rule:
# - ENIGHUR uses household income concepts directly at the household level.
# - ENEMDU annual official syntax for income stratification uses ingpc, not ingrl.
# - Therefore, the consistent household income measure here is total household income
#   derived as ingpc * household size.
# - Complete-income households are those with non-missing ingpc.
hogares <- enemdu_personas |>
  dplyr::transmute(
    id_hogar = as.character(.data$id_hogar),
    fexp = as.numeric(.data$fexp),
    ingpc = as.numeric(.data$ingpc)
  ) |>
  dplyr::group_by(.data$id_hogar) |>
  dplyr::summarise(
    fexp = dplyr::first(.data$fexp),
    hh_size = dplyr::n(),
    ingpc = dplyr::first(.data$ingpc),
    has_complete_income = !is.na(dplyr::first(.data$ingpc)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    hh_income_total = .data$ingpc * .data$hh_size
  )

hogares_completos <- hogares |>
  dplyr::filter(.data$has_complete_income)

summary_table <- data.frame(
  scope = "complete_income_households",
  income_measure = "hh_income_total_from_ingpc",
  households = nrow(hogares_completos),
  weighted_mean_income = weighted.mean(hogares_completos$hh_income_total, hogares_completos$fexp),
  weighted_median_income = weighted_quantile(hogares_completos$hh_income_total, hogares_completos$fexp, 0.5),
  unweighted_mean_income = mean(hogares_completos$hh_income_total),
  unweighted_median_income = median(hogares_completos$hh_income_total),
  households_with_missing_ingpc = sum(!hogares$has_complete_income),
  weighted_share_complete = weighted.mean(hogares$has_complete_income, hogares$fexp),
  stringsAsFactors = FALSE
)

notes_table <- data.frame(
  item = c(
    "source_file",
    "official_enemdu_income_variable",
    "official_enemdu_syntax_rule",
    "household_income_definition_used",
    "complete_income_rule",
    "enighur_consistency_note"
  ),
  detail = c(
    enemdu_personas_path,
    "ingpc",
    "The official ENEMDU annual syntax ranks income using ingpc.",
    "Household total income = ingpc * household size.",
    "Complete-income households require non-missing ingpc.",
    "This is the ENEMDU measure most consistent with ENIGHUR household income analysis."
  ),
  stringsAsFactors = FALSE
)

output_path <- file.path("output", "tables", "enemdu_2025_ingreso_hogar_summary.xlsx")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

openxlsx::write.xlsx(
  list(
    summary = summary_table,
    notes = notes_table
  ),
  output_path,
  overwrite = TRUE
)

cat("ENEMDU 2025 ingresos del hogar\n")
cat("Archivo:", enemdu_personas_path, "\n")
cat("Resumen guardado en:", output_path, "\n\n")

print(summary_table)

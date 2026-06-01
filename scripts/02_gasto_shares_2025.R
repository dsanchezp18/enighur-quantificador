suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
})

options(scipen = 999)

tabulados_2025_path <- file.path(
  "data", "enighur", "2025",
  "Tabulados_ENIGHUR_2024-2025.xlsx"
)

tabulados_2012_path <- file.path(
  "data", "enighur", "2012",
  "TABULADOS ENIGHUR 2011-2012.xlsx"
)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

stop_if_missing(tabulados_2025_path, "Tabulados 2025")
stop_if_missing(tabulados_2012_path, "Tabulados 2012")

clean_label <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

component_rows <- function(year, level, code, label, total, denominator,
                           parent_code = NA_character_,
                           mutually_exclusive = FALSE,
                           source_sheet = NA_character_) {
  data.frame(
    year = year,
    level = level,
    parent_code = parent_code,
    code = code,
    component = label,
    weighted_total_usd = as.numeric(total),
    share_gasto_corriente_monetario = as.numeric(total / denominator),
    share_gasto_corriente_monetario_pct = as.numeric(100 * total / denominator),
    denominator_gasto_corriente_monetario_usd = as.numeric(denominator),
    mutually_exclusive = mutually_exclusive,
    source_sheet = source_sheet,
    stringsAsFactors = FALSE
  )
}

build_2025_main <- function(path) {
  cuadro_214 <- read_excel(path, sheet = "CUADRO 2.1.4", col_names = FALSE, n_max = 26)
  structure_labels <- clean_label(cuadro_214[[1]])
  national_values <- as_number(cuadro_214[[2]])

  value_by_label <- function(label) {
    idx <- match(label, structure_labels)
    if (is.na(idx)) stop("Label not found in CUADRO 2.1.4: ", label, call. = FALSE)
    national_values[[idx]]
  }

  gasto_corriente_monetario_total <- value_by_label("Gasto corriente monetario")
  gasto_consumo_total <- value_by_label("Gasto corriente de consumo")
  gasto_no_consumo_total <- value_by_label("Gasto de no consumo")

  division_labels <- c(
    "Alimentos y bebidas no alcohólicas",
    "Bebidas alcohólicas, tabaco y estupefacientes",
    "Prendas de vestir y calzado",
    "Vivienda, agua, electricidad, gas y otros combustibles",
    "Muebles, artículos para el hogar y para la conservación ordinaria del hogar",
    "Salud",
    "Transporte",
    "Información y comunicación",
    "Recreación, deporte y cultura",
    "Servicios Educativos",
    "Servicios de restaurantes y alojamientos",
    "Seguros y servicios financieros",
    "Cuidado personal, previsión social y bienes y servicios diversos"
  )

  division_codes <- paste0("d", seq_along(division_labels))
  division_totals <- vapply(division_labels, value_by_label, numeric(1))

  rows <- c(
    list(component_rows("2025", "total", "Gasto_corriente_monetario", "Gasto corriente monetario",
                        gasto_corriente_monetario_total, gasto_corriente_monetario_total,
                        source_sheet = "CUADRO 2.1.4")),
    list(component_rows("2025", "component", "Gasto_consumo", "Gasto corriente de consumo",
                        gasto_consumo_total, gasto_corriente_monetario_total,
                        parent_code = "Gasto_corriente_monetario",
                        source_sheet = "CUADRO 2.1.4")),
    lapply(seq_along(division_codes), function(i) {
      component_rows("2025", "division", division_codes[[i]], division_labels[[i]],
                     division_totals[[i]], gasto_corriente_monetario_total,
                     parent_code = "Gasto_consumo", mutually_exclusive = TRUE,
                     source_sheet = "CUADRO 2.1.4")
    }),
    list(component_rows("2025", "component", "Gasto_no_consumo", "Gasto de no consumo",
                        gasto_no_consumo_total, gasto_corriente_monetario_total,
                        parent_code = "Gasto_corriente_monetario", mutually_exclusive = TRUE,
                        source_sheet = "CUADRO 2.1.4"))
  )

  do.call(rbind, rows)
}

build_2025_groups <- function(path, denominator) {
  cuadro_31 <- read_excel(path, sheet = "CUADRO 3.1", col_names = FALSE, range = "B14:C90")
  labels <- clean_label(cuadro_31[[1]])
  totals <- as_number(cuadro_31[[2]])

  keep <- !is.na(labels) &
    labels != "Nota:" &
    !grepl("^gEl tamaño|^\\(-\\)g|^Población|^\\*CCIF|^Fuente:|^Para mayor|^https://", labels)

  labels <- labels[keep]
  totals <- totals[keep]

  codes <- c(
    "Gasto_consumo",
    "d1", "g11", "g12", "g13",
    "d2", "g21", "g22", "g23", "g24",
    "d3", "g31", "g32",
    "d4", "g41", "g42", "g43", "g44", "g45",
    "d5", "g51", "g52", "g53", "g54", "g55", "g56",
    "d6", "g61", "g62", "g63", "g64",
    "d7", "g71", "g72", "g73", "g74",
    "d8", "g81", "g82", "g83",
    "d9", "g91", "g92", "g93", "g94", "g95", "g96", "g97", "g98",
    "d10", "g101", "g102", "g103", "g104", "g105",
    "d11", "g111", "g112",
    "d12", "g121", "g122",
    "d13", "g131", "g132", "g133", "g139"
  )

  if (length(labels) != length(codes)) {
    stop("Unexpected number of 2025 CUADRO 3.1 rows", call. = FALSE)
  }

  current_division <- NA_character_
  out <- vector("list", length(codes))

  for (i in seq_along(codes)) {
    code <- codes[[i]]
    level <- if (code == "Gasto_consumo") "component" else if (startsWith(code, "d")) "division" else "group"
    if (level == "division") current_division <- code

    parent_code <- switch(
      level,
      component = "Gasto_corriente_monetario",
      division = "Gasto_consumo",
      group = current_division
    )

    out[[i]] <- component_rows(
      "2025", level, code, labels[[i]], totals[[i]], denominator,
      parent_code = parent_code, source_sheet = "CUADRO 3.1"
    )
  }

  do.call(rbind, out)
}

build_2012_main <- function(path) {
  cuadro_37 <- read_excel(path, sheet = "Estructura del gasto (37)", col_names = FALSE, n_max = 24)
  labels <- clean_label(cuadro_37[[2]])
  totals <- as_number(cuadro_37[[3]])

  value_by_label <- function(label) {
    idx <- match(label, labels)
    if (is.na(idx)) stop("Label not found in Estructura del gasto (37): ", label, call. = FALSE)
    totals[[idx]]
  }

  gasto_corriente_monetario_total <- value_by_label("Gasto corriente monetario")
  gasto_consumo_total <- value_by_label("Gasto corriente de consumo")
  gasto_no_consumo_total <- value_by_label("Gasto de no consumo")

  division_labels <- c(
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
  )

  division_codes <- paste0("d", seq_along(division_labels))
  division_totals <- vapply(division_labels, value_by_label, numeric(1))

  rows <- c(
    list(component_rows("2012", "total", "Gasto_corriente_monetario", "Gasto corriente monetario",
                        gasto_corriente_monetario_total, gasto_corriente_monetario_total,
                        source_sheet = "Estructura del gasto (37)")),
    list(component_rows("2012", "component", "Gasto_consumo", "Gasto corriente de consumo",
                        gasto_consumo_total, gasto_corriente_monetario_total,
                        parent_code = "Gasto_corriente_monetario",
                        source_sheet = "Estructura del gasto (37)")),
    lapply(seq_along(division_codes), function(i) {
      component_rows("2012", "division", division_codes[[i]], division_labels[[i]],
                     division_totals[[i]], gasto_corriente_monetario_total,
                     parent_code = "Gasto_consumo", mutually_exclusive = TRUE,
                     source_sheet = "Estructura del gasto (37)")
    }),
    list(component_rows("2012", "component", "Gasto_no_consumo", "Gasto de no consumo",
                        gasto_no_consumo_total, gasto_corriente_monetario_total,
                        parent_code = "Gasto_corriente_monetario", mutually_exclusive = TRUE,
                        source_sheet = "Estructura del gasto (37)"))
  )

  do.call(rbind, rows)
}

build_2012_published <- function(path, denominator) {
  x <- read_excel(path, sheet = 46, col_names = FALSE)
  labels <- clean_label(x[[3]])
  totals <- as_number(x[[4]])

  keep <- !is.na(labels) &
    labels != "" &
    labels != "Regresar" &
    !grepl("^Cuadro No\\.|^Gasto corriente de consumo mensual|^Área geográfica|^Total$|^Deciles de Ingreso|^Fuente:", labels)

  out <- data.frame(
    year = "2012",
    level = "published_row",
    parent_code = NA_character_,
    code = paste0("r", seq_len(sum(keep))),
    component = labels[keep],
    weighted_total_usd = totals[keep],
    share_gasto_corriente_monetario = totals[keep] / denominator,
    share_gasto_corriente_monetario_pct = 100 * totals[keep] / denominator,
    denominator_gasto_corriente_monetario_usd = denominator,
    mutually_exclusive = FALSE,
    source_sheet = "Gas_cons_prom_div_ grup(40)",
    stringsAsFactors = FALSE
  )

  out[!is.na(out$weighted_total_usd), ]
}

main_2025 <- build_2025_main(tabulados_2025_path)
groups_2025 <- build_2025_groups(tabulados_2025_path, main_2025$denominator_gasto_corriente_monetario_usd[[1]])
main_2012 <- build_2012_main(tabulados_2012_path)
published_2012 <- build_2012_published(tabulados_2012_path, main_2012$denominator_gasto_corriente_monetario_usd[[1]])

summary_table <- rbind(
  main_2012[, c("year", "level", "code", "component", "weighted_total_usd", "share_gasto_corriente_monetario_pct", "source_sheet")],
  main_2025[, c("year", "level", "code", "component", "weighted_total_usd", "share_gasto_corriente_monetario_pct", "source_sheet")]
)

output_dir <- file.path("output", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

workbook_path <- file.path(output_dir, "enighur_spending_shares_2012_2025_national.xlsx")

write_xlsx(
  list(
    summary_main = summary_table,
    shares_2012_main = main_2012,
    shares_2012_published = published_2012,
    shares_2025_main = main_2025,
    shares_2025_groups = groups_2025
  ),
  path = workbook_path
)

cat("Wrote:", normalizePath(workbook_path, winslash = "/"), "\n")
cat("2012 mutually exclusive main share sum:", sum(main_2012$share_gasto_corriente_monetario[main_2012$mutually_exclusive]), "\n")
cat("2025 mutually exclusive main share sum:", sum(main_2025$share_gasto_corriente_monetario[main_2025$mutually_exclusive]), "\n")

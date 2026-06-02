source("scripts/packages.R")
ensure_packages(c("readxl", "readr", "dplyr", "networkD3"))

options(scipen = 999)

tab_path <- file.path("data", "enighur", "2025", "Tabulados_ENIGHUR_2024-2025.xlsx")
map_path <- file.path("data", "intermediate", "mapeo_categorias_gasto.csv")

if (!file.exists(tab_path)) stop("Tabulados not found: ", tab_path, call. = FALSE)
if (!file.exists(map_path)) stop("Mapping not found: ", map_path, call. = FALSE)

normalize_label <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

fmt_usd <- function(x) sprintf("$%.0f", round(x))
fmt_pct <- function(x) paste0(sprintf("%.1f", 100 * x), "%")

get_nacional_total <- function(sheet, pattern, exclude = NULL) {
  rows <- which(grepl(pattern, sheet[[1]], ignore.case = TRUE))
  if (!is.null(exclude)) {
    rows <- rows[!grepl(exclude, sheet[[1]][rows], ignore.case = TRUE)]
  }
  if (length(rows) == 0) {
    stop("No row matched pattern: ", pattern, call. = FALSE)
  }
  as.numeric(sheet[[2]][rows[1]])
}

match_gasto_row <- function(map_name_norm, gas_data) {
  exact_idx <- which(gas_data$nombre_norm == map_name_norm)
  if (length(exact_idx) > 0) {
    return(gas_data[exact_idx[1], , drop = FALSE])
  }

  map_tokens <- strsplit(map_name_norm, " +")[[1]]
  subset_idx <- which(vapply(
    gas_data$nombre_norm,
    function(lbl) all(map_tokens %in% strsplit(lbl, " +")[[1]]),
    logical(1)
  ))
  if (length(subset_idx) > 0) {
    return(gas_data[subset_idx[1], , drop = FALSE])
  }

  gas_data[0, , drop = FALSE]
}

ing_sheet <- readxl::read_excel(tab_path, sheet = "CUADRO 2.1.1", col_names = FALSE)
gas_sheet <- readxl::read_excel(tab_path, sheet = "CUADRO 2.1.3", col_names = FALSE)
avg_sheet <- readxl::read_excel(tab_path, sheet = "CUADRO 2.2.1", col_names = FALSE)
gasto_map <- readr::read_csv(map_path, show_col_types = FALSE)

ing_cor_tot_exp <- get_nacional_total(ing_sheet, "Ingreso corriente total del hogar")
ing_mon_cor_exp <- get_nacional_total(ing_sheet, "Ingreso corriente monetario del hogar")
gas_corr_exp <- get_nacional_total(gas_sheet, "Gasto corriente de consumo")
gas_no_con_exp <- get_nacional_total(gas_sheet, "Gasto de no consumo")

nac_row <- which(avg_sheet[[1]] == "Nacional" & is.na(avg_sheet[[2]]))
if (length(nac_row) == 0) {
  stop("Could not find Nacional row in CUADRO 2.2.1", call. = FALSE)
}
avg_ing_cor_tot <- as.numeric(avg_sheet[[3]][nac_row[1]])

avg_ing_mon_cor <- (ing_mon_cor_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_corr <- (gas_corr_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_no_con <- (gas_no_con_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_ahorro <- avg_ing_mon_cor - avg_gas_corr - avg_gas_no_con

if (avg_ahorro < 0) {
  stop("Savings bucket is negative; selected totals do not close.", call. = FALSE)
}

gas_rows <- gas_sheet |>
  dplyr::transmute(
    nombre_enighur = as.character(.data$...1),
    valor = suppressWarnings(as.numeric(.data$...2)),
    nombre_norm = normalize_label(as.character(.data$...1))
  ) |>
  dplyr::filter(!is.na(valor))

map_rows <- gasto_map |>
  dplyr::mutate(nombre_norm = normalize_label(.data$nombre_enighur))

matched_rows <- lapply(seq_len(nrow(map_rows)), function(i) {
  hit <- match_gasto_row(map_rows$nombre_norm[i], gas_rows)
  data.frame(
    categoria_codigo = map_rows$categoria_codigo[i],
    categoria_nombre = map_rows$categoria_nombre[i],
    orden = map_rows$orden[i],
    valor = if (nrow(hit) == 0) NA_real_ else hit$valor[1]
  )
})

cat_rows <- dplyr::bind_rows(matched_rows)

if (any(is.na(cat_rows$valor))) {
  missing_names <- unique(map_rows$nombre_enighur[is.na(cat_rows$valor)])
  stop("Missing gasto category matches: ", paste(missing_names, collapse = ", "), call. = FALSE)
}

cat_avg <- cat_rows |>
  dplyr::group_by(categoria_codigo, categoria_nombre, orden) |>
  dplyr::summarise(
    valor_exp = sum(valor),
    valor_avg = (sum(valor) / ing_cor_tot_exp) * avg_ing_cor_tot,
    .groups = "drop"
  ) |>
  dplyr::arrange(orden)

if (abs(sum(cat_avg$valor_avg) - avg_gas_corr) > 1e-6) {
  stop("Aggregated gasto corriente categories do not sum to promedio gasto corriente.", call. = FALSE)
}

root_label <- paste0("Ingreso monetario\n", fmt_usd(avg_ing_mon_cor))
gasto_corr_label <- paste0(
  "Gasto corriente\n", fmt_usd(avg_gas_corr), "\n", fmt_pct(avg_gas_corr / avg_ing_mon_cor), " del ingreso"
)
gasto_no_con_label <- paste0(
  "Gasto de no consumo\n", fmt_usd(avg_gas_no_con), "\n", fmt_pct(avg_gas_no_con / avg_ing_mon_cor), " del ingreso"
)
ahorro_label <- paste0(
  "Ahorro\n", fmt_usd(avg_ahorro), "\n", fmt_pct(avg_ahorro / avg_ing_mon_cor), " del ingreso"
)

category_labels <- paste0(
  cat_avg$categoria_nombre, "\n",
  fmt_usd(cat_avg$valor_avg), "\n",
  fmt_pct(cat_avg$valor_avg / avg_ing_mon_cor), " del ingreso"
)

node_names <- c(root_label, gasto_corr_label, gasto_no_con_label, ahorro_label, category_labels)
nodes <- data.frame(name = node_names, stringsAsFactors = FALSE)

links <- dplyr::bind_rows(
  data.frame(
    source = c(0L, 0L, 0L),
    target = c(1L, 2L, 3L),
    value = c(avg_gas_corr, avg_gas_no_con, avg_ahorro)
  ),
  data.frame(
    source = rep(1L, nrow(cat_avg)),
    target = seq.int(4L, 3L + nrow(cat_avg)),
    value = cat_avg$valor_avg
  )
)

cat(sprintf("Ingreso monetario promedio: $%.2f\n", avg_ing_mon_cor))
cat(sprintf("Gasto corriente promedio:   $%.2f\n", avg_gas_corr))
cat(sprintf("Gasto de no consumo:       $%.2f\n", avg_gas_no_con))
cat(sprintf("Ahorro promedio:           $%.2f\n", avg_ahorro))

sankey <- networkD3::sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "USD",
  fontSize = 13,
  nodeWidth = 30,
  nodePadding = 20,
  sinksRight = TRUE
)

print(sankey)

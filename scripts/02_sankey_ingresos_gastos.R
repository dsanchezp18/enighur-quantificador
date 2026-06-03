source("scripts/packages.R")
source("scripts/utils.R")

ensure_packages(c("readxl", "readr", "dplyr", "ggplot2", "scales"))

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

tab_path <- resolve_existing_path(
  file.path("data", "enighur", "2025", "Tabulados_ENIGHUR_2024-2025.xlsx"),
  "Tabulados ENIGHUR 2025"
)
map_path <- file.path("data", "intermediate", "mapeo_categorias_gasto.csv")

if (!file.exists(map_path)) {
  stop("Mapping not found: ", map_path, call. = FALSE)
}

normalize_label <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

fmt_usd <- function(x) scales::dollar(round(x), prefix = "$", big.mark = ",", accuracy = 1)
fmt_pct <- function(x) paste0(sprintf("%.1f", 100 * x), "%")

read_total_rows <- function(sheet_name) {
  sheet <- readxl::read_excel(tab_path, sheet = sheet_name, col_names = FALSE, .name_repair = "minimal")
  data.frame(
    label = as.character(sheet[[1]]),
    value = suppressWarnings(as.numeric(sheet[[2]])),
    stringsAsFactors = FALSE
  )
}

get_nacional_total <- function(rows, pattern, exclude = NULL) {
  matches <- grepl(pattern, rows$label, ignore.case = TRUE)
  if (!is.null(exclude)) {
    matches <- matches & !grepl(exclude, rows$label, ignore.case = TRUE)
  }

  hit <- rows[matches & !is.na(rows$value), , drop = FALSE]
  if (nrow(hit) == 0) {
    stop("No row matched pattern: ", pattern, call. = FALSE)
  }

  hit$value[[1]]
}

get_category_rows <- function(rows) {
  rows |>
    dplyr::mutate(
      nombre_enighur = .data$label,
      nombre_norm = normalize_label(.data$label)
    ) |>
    dplyr::filter(!is.na(.data$value))
}

match_gasto_row <- function(map_name_norm, gas_data) {
  exact_idx <- which(gas_data$nombre_norm == map_name_norm)
  if (length(exact_idx) > 0) {
    return(gas_data[exact_idx[[1]], , drop = FALSE])
  }

  map_tokens <- strsplit(map_name_norm, " +")[[1]]
  subset_idx <- which(vapply(
    gas_data$nombre_norm,
    function(lbl) all(map_tokens %in% strsplit(lbl, " +")[[1]]),
    logical(1)
  ))
  if (length(subset_idx) > 0) {
    return(gas_data[subset_idx[[1]], , drop = FALSE])
  }

  gas_data[0, , drop = FALSE]
}

read_average_total <- function() {
  avg_sheet <- readxl::read_excel(
    tab_path,
    sheet = "CUADRO 2.2.1",
    col_names = FALSE,
    .name_repair = "minimal"
  )
  nac_row <- which(avg_sheet[[1]] == "Nacional" & is.na(avg_sheet[[2]]))

  if (length(nac_row) == 0) {
    stop("Could not find Nacional row in CUADRO 2.2.1", call. = FALSE)
  }

  as.numeric(avg_sheet[[3]][nac_row[[1]]])
}

stack_stage <- function(labels, values, gap, total_height, node_type) {
  stage_height <- sum(values) + gap * max(length(values) - 1, 0)
  cursor <- total_height - (total_height - stage_height) / 2

  rows <- lapply(seq_along(values), function(i) {
    ymax <- cursor
    ymin <- ymax - values[[i]]
    cursor <<- ymin - gap

    data.frame(
      label = labels[[i]],
      value = values[[i]],
      ymin = ymin,
      ymax = ymax,
      ymid = (ymin + ymax) / 2,
      node_type = node_type,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

allocate_within <- function(ymin, ymax, values) {
  cursor <- ymax
  lapply(values, function(value) {
    upper <- cursor
    lower <- upper - value
    cursor <<- lower

    list(ymin = lower, ymax = upper)
  })
}

make_flow_polygon <- function(x0, x1, from_range, to_range, fill, flow_id, alpha = 0.75) {
  t <- seq(0, 1, length.out = 50)
  s <- t^2 * (3 - 2 * t)
  xs <- x0 + (x1 - x0) * t
  upper <- from_range$ymax + (to_range$ymax - from_range$ymax) * s
  lower <- from_range$ymin + (to_range$ymin - from_range$ymin) * s

  data.frame(
    x = c(xs, rev(xs)),
    y = c(upper, rev(lower)),
    fill = fill,
    alpha = alpha,
    flow_id = flow_id,
    stringsAsFactors = FALSE
  )
}

ing_rows <- read_total_rows("CUADRO 2.1.1")
gas_rows <- read_total_rows("CUADRO 2.1.3")

ing_cor_tot_exp <- get_nacional_total(ing_rows, "Ingreso corriente total del hogar")
ing_mon_cor_exp <- get_nacional_total(ing_rows, "Ingreso corriente monetario del hogar")
gas_corr_exp <- get_nacional_total(gas_rows, "Gasto corriente de consumo")
gas_no_con_exp <- get_nacional_total(gas_rows, "Gasto de no consumo")

avg_ing_cor_tot <- read_average_total()
avg_ing_mon_cor <- (ing_mon_cor_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_corr <- (gas_corr_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_no_con <- (gas_no_con_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_ahorro <- avg_ing_mon_cor - avg_gas_corr - avg_gas_no_con

if (avg_ahorro < 0) {
  stop("Savings bucket is negative; selected totals do not close.", call. = FALSE)
}

gas_categories <- get_category_rows(gas_rows)
gasto_map <- readr::read_csv(map_path, show_col_types = FALSE)
map_rows <- gasto_map |>
  dplyr::mutate(nombre_norm = normalize_label(.data$nombre_enighur))

matched_rows <- lapply(seq_len(nrow(map_rows)), function(i) {
  hit <- match_gasto_row(map_rows$nombre_norm[[i]], gas_categories)
  data.frame(
    categoria_codigo = map_rows$categoria_codigo[[i]],
    categoria_nombre = map_rows$categoria_nombre[[i]],
    orden = map_rows$orden[[i]],
    valor = if (nrow(hit) == 0) NA_real_ else hit$value[[1]],
    stringsAsFactors = FALSE
  )
})

category_rows <- dplyr::bind_rows(matched_rows)

if (any(is.na(category_rows$valor))) {
  missing_names <- unique(map_rows$nombre_enighur[is.na(category_rows$valor)])
  stop("Missing gasto category matches: ", paste(missing_names, collapse = ", "), call. = FALSE)
}

category_rows <- category_rows |>
  dplyr::group_by(.data$categoria_codigo, .data$categoria_nombre, .data$orden) |>
  dplyr::summarise(
    valor = sum(.data$valor),
    valor_avg = (sum(.data$valor) / ing_cor_tot_exp) * avg_ing_cor_tot,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$orden) |>
  dplyr::mutate(
    label = .data$categoria_nombre,
    share_ingreso = .data$valor_avg / avg_ing_mon_cor
  )

if (abs(sum(category_rows$valor_avg) - avg_gas_corr) > 1e-6) {
  stop("Gasto corriente categories do not sum to total gasto corriente.", call. = FALSE)
}

mid_labels <- c("Gasto corriente", "Gasto de no consumo", "Ahorro")
mid_values <- c(avg_gas_corr, avg_gas_no_con, avg_ahorro)

plot_height <- max(
  avg_ing_mon_cor,
  sum(mid_values) + avg_ing_mon_cor * 0.06,
  sum(category_rows$valor_avg) + avg_ing_mon_cor * 0.03 * (nrow(category_rows) - 1)
)

root_stage <- stack_stage("Ingreso monetario", avg_ing_mon_cor, gap = 0, total_height = plot_height, node_type = "root")
mid_stage <- stack_stage(mid_labels, mid_values, gap = avg_ing_mon_cor * 0.03, total_height = plot_height, node_type = "mid")
right_stage <- stack_stage(category_rows$label, category_rows$valor_avg, gap = avg_ing_mon_cor * 0.01, total_height = plot_height, node_type = "right")

root_allocations <- allocate_within(root_stage$ymin[[1]], root_stage$ymax[[1]], mid_values)
mid_targets <- lapply(seq_len(nrow(mid_stage)), function(i) list(ymin = mid_stage$ymin[[i]], ymax = mid_stage$ymax[[i]]))
right_targets <- lapply(seq_len(nrow(right_stage)), function(i) list(ymin = right_stage$ymin[[i]], ymax = right_stage$ymax[[i]]))
gasto_corr_allocations <- allocate_within(mid_stage$ymin[[1]], mid_stage$ymax[[1]], category_rows$valor_avg)

mid_palette <- c(
  "Gasto corriente" = "#2D6A9F",
  "Gasto de no consumo" = "#8D99AE",
  "Ahorro" = "#1B4332"
)

category_palette <- c(
  "#4F772D", "#90A955", "#EC9A29", "#BC4749", "#386641", "#6A994E",
  "#A7C957", "#F2C14E", "#D68C45", "#7F5539", "#5E548E", "#6C757D", "#8E9AAF"
)

root_flows <- dplyr::bind_rows(lapply(seq_along(root_allocations), function(i) {
  make_flow_polygon(
    x0 = 0.12,
    x1 = 0.88,
    from_range = root_allocations[[i]],
    to_range = mid_targets[[i]],
    fill = unname(mid_palette[[mid_labels[[i]]]]),
    flow_id = paste0("root-", i)
  )
}))

category_flows <- dplyr::bind_rows(lapply(seq_len(nrow(category_rows)), function(i) {
  make_flow_polygon(
    x0 = 1.12,
    x1 = 1.88,
    from_range = gasto_corr_allocations[[i]],
    to_range = right_targets[[i]],
    fill = category_palette[[i]],
    flow_id = paste0("cat-", i),
    alpha = 0.8
  )
}))

node_rects <- dplyr::bind_rows(
  dplyr::mutate(root_stage, xmin = -0.12, xmax = 0.12, fill = "#F8F9FA"),
  dplyr::mutate(mid_stage, xmin = 0.88, xmax = 1.12, fill = "#F8F9FA"),
  dplyr::mutate(right_stage, xmin = 1.88, xmax = 2.12, fill = "#F8F9FA")
)

mid_stage <- mid_stage |>
  dplyr::mutate(
    label_text = c(
      paste0("Gasto corriente\n", fmt_usd(avg_gas_corr), "\n", fmt_pct(avg_gas_corr / avg_ing_mon_cor), " del ingreso"),
      paste0("Gasto de no consumo\n", fmt_usd(avg_gas_no_con), "\n", fmt_pct(avg_gas_no_con / avg_ing_mon_cor), " del ingreso"),
      paste0("Ahorro\n", fmt_usd(avg_ahorro), "\n", fmt_pct(avg_ahorro / avg_ing_mon_cor), " del ingreso")
    )
  )

right_stage <- right_stage |>
  dplyr::mutate(
    label_text = paste0(
      .data$label, "\n",
      fmt_usd(.data$value), "\n",
      fmt_pct(.data$value / avg_ing_mon_cor), " del ingreso"
    )
  )

root_label <- paste0(
  "Ingreso monetario\n",
  fmt_usd(avg_ing_mon_cor), "\n100% del ingreso"
)

plot <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data = root_flows,
    ggplot2::aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  ggplot2::geom_polygon(
    data = category_flows,
    ggplot2::aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  ggplot2::geom_rect(
    data = node_rects,
    ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
    fill = "#F8F9FA",
    colour = "#495057",
    linewidth = 0.3
  ) +
  ggplot2::annotate(
    "text",
    x = 0,
    y = root_stage$ymid[[1]],
    label = root_label,
    size = 3.2,
    lineheight = 1.05,
    family = "",
    colour = "#212529"
  ) +
  ggplot2::geom_text(
    data = mid_stage,
    ggplot2::aes(x = 1, y = .data$ymid, label = .data$label_text),
    size = 3,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  ggplot2::geom_text(
    data = right_stage,
    ggplot2::aes(x = 2.16, y = .data$ymid, label = .data$label_text),
    hjust = 0,
    size = 2.7,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.2, 3.2), ylim = c(0, plot_height), clip = "off") +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::label_dollar(prefix = "$", big.mark = ",")) +
  ggplot2::labs(
    title = "En que gastan los hogares ecuatorianos su ingreso monetario",
    subtitle = "Descomposicion del ingreso monetario promedio del hogar ENIGHUR 2024-2025",
    x = NULL,
    y = "USD mensuales promedio",
    caption = paste0(
      "Fuente: INEC, ENIGHUR 2024-2025. Totales nacionales de CUADRO 2.1.1, 2.1.3 y promedio nacional de CUADRO 2.2.1.\n",
      "El ahorro se calcula como ingreso monetario menos gasto corriente de consumo y gasto de no consumo."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 120, 10, 10),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

save_figure("sankey_ingresos_gastos_2025.png", plot, width = 12, height = 8, dpi = 300)
cat("Guardado: output/figures/sankey_ingresos_gastos_2025.png\n")

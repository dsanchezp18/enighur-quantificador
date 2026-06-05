source("scripts/packages.R")
source("scripts/utils.R")

ensure_packages(c("readxl", "readr", "dplyr", "ggplot2", "scales", "plotly", "htmlwidgets"))

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
with_alpha <- function(colour, alpha = 1) grDevices::adjustcolor(colour, alpha.f = alpha)

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
  dplyr::left_join(
    gasto_map |>
      dplyr::distinct(.data$categoria_codigo, .data$color_hex),
    by = "categoria_codigo"
  ) |>
  dplyr::arrange(.data$orden) |>
  dplyr::mutate(
    label = .data$categoria_nombre,
    share_ingreso = .data$valor_avg / avg_ing_mon_cor
  )

if (abs(sum(category_rows$valor_avg) - avg_gas_corr) > 1e-6) {
  stop("Gasto corriente categories do not sum to total gasto corriente.", call. = FALSE)
}

message(
  "Alimentos y restaurantes = ",
  paste(
    gasto_map$nombre_enighur[gasto_map$categoria_codigo == "alimentacion"],
    collapse = " + "
  )
)

mid_labels <- c("Gasto corriente", "Gasto de no consumo", "Ahorro")
mid_values <- c(avg_gas_corr, avg_gas_no_con, avg_ahorro)

mid_gap <- avg_ing_mon_cor * 0.012
right_gap <- avg_ing_mon_cor * 0.004

plot_height <- max(
  avg_ing_mon_cor,
  sum(mid_values) + mid_gap * (length(mid_values) - 1),
  sum(category_rows$valor_avg) + right_gap * (nrow(category_rows) - 1)
) * 1.01

root_stage <- stack_stage("Ingreso monetario", avg_ing_mon_cor, gap = 0, total_height = plot_height, node_type = "root")
mid_stage <- stack_stage(mid_labels, mid_values, gap = mid_gap, total_height = plot_height, node_type = "mid")
right_stage <- stack_stage(category_rows$label, category_rows$valor_avg, gap = right_gap, total_height = plot_height, node_type = "right")

root_allocations <- allocate_within(root_stage$ymin[[1]], root_stage$ymax[[1]], mid_values)
mid_targets <- lapply(seq_len(nrow(mid_stage)), function(i) list(ymin = mid_stage$ymin[[i]], ymax = mid_stage$ymax[[i]]))
right_targets <- lapply(seq_len(nrow(right_stage)), function(i) list(ymin = right_stage$ymin[[i]], ymax = right_stage$ymax[[i]]))
gasto_corr_allocations <- allocate_within(mid_stage$ymin[[1]], mid_stage$ymax[[1]], category_rows$valor_avg)

root_xmin <- -0.18
root_xmax <- 0.18
mid_xmin <- 1.24
mid_xmax <- 1.54
right_xmin <- 2.34
right_xmax <- 2.64
mid_label_x <- (mid_xmin + mid_xmax) / 2
right_label_x <- 2.70
root_flow_x0 <- root_xmax
root_flow_x1 <- mid_xmin
cat_flow_x0 <- mid_xmax
cat_flow_x1 <- right_xmin

mid_palette <- c(
  "Gasto corriente" = "#2D6A9F",
  "Gasto de no consumo" = "#8D99AE",
  "Ahorro" = "#1B4332"
)

category_palette <- category_rows$color_hex

root_flows <- dplyr::bind_rows(lapply(seq_along(root_allocations), function(i) {
  make_flow_polygon(
    x0 = root_flow_x0,
    x1 = root_flow_x1,
    from_range = root_allocations[[i]],
    to_range = mid_targets[[i]],
    fill = unname(mid_palette[[mid_labels[[i]]]]),
    flow_id = paste0("root-", i)
  )
}))

category_flows <- dplyr::bind_rows(lapply(seq_len(nrow(category_rows)), function(i) {
  make_flow_polygon(
    x0 = cat_flow_x0,
    x1 = cat_flow_x1,
    from_range = gasto_corr_allocations[[i]],
    to_range = right_targets[[i]],
    fill = category_palette[[i]],
    flow_id = paste0("cat-", i),
    alpha = 0.8
  )
}))

node_rects <- dplyr::bind_rows(
  dplyr::mutate(root_stage, xmin = root_xmin, xmax = root_xmax, fill = "#F8F9FA"),
  dplyr::mutate(mid_stage, xmin = mid_xmin, xmax = mid_xmax, fill = "#F8F9FA"),
  dplyr::mutate(right_stage, xmin = right_xmin, xmax = right_xmax, fill = "#F8F9FA")
)

mid_stage <- mid_stage |>
  dplyr::mutate(
    label_text = c(
      paste0("Gasto corriente\n", fmt_usd(avg_gas_corr)),
      paste0("Gasto de no consumo\n", fmt_usd(avg_gas_no_con)),
      paste0("Ahorro\n", fmt_usd(avg_ahorro))
    )
  )

right_stage <- right_stage |>
  dplyr::mutate(
    label_text = paste0(
      .data$label, "\n",
      fmt_usd(.data$value)
    )
  )

root_label <- paste0(
  "Ingreso monetario\n",
  fmt_usd(avg_ing_mon_cor)
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
    size = 4.1,
    lineheight = 1.05,
    family = "",
    colour = "#212529"
  ) +
  ggplot2::geom_text(
    data = mid_stage,
    ggplot2::aes(x = mid_label_x, y = .data$ymid, label = .data$label_text),
    size = 4,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  ggplot2::geom_text(
    data = right_stage,
    ggplot2::aes(x = right_label_x, y = .data$ymid, label = .data$label_text),
    hjust = 0,
    size = 3.8,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.28, 3.55), ylim = c(0, plot_height), clip = "off") +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::label_dollar(prefix = "$", big.mark = ",")) +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_quantificador() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(8, 60, 8, 12)
  )

save_figure(
  "sankey_ingresos_gastos_2025.png",
  plot = plot,
  width = 12.4,
  height = 7.8,
  dpi = 300
)

interactive_nodes <- data.frame(
  label = c(
    root_label,
    mid_stage$label_text,
    right_stage$label_text
  ),
  color = c(
    "#F8F9FA",
    unname(mid_palette[mid_labels]),
    category_palette[seq_len(nrow(category_rows))]
  ),
  stringsAsFactors = FALSE
)

interactive_links <- dplyr::bind_rows(
  data.frame(
    source = 0,
    target = seq_along(mid_labels),
    value = mid_values,
    color = unname(vapply(mid_palette[mid_labels], with_alpha, character(1), alpha = 0.75)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    source = rep(1, nrow(category_rows)),
    target = seq_len(nrow(category_rows)) + length(mid_labels),
    value = category_rows$valor_avg,
    color = vapply(category_palette[seq_len(nrow(category_rows))], with_alpha, character(1), alpha = 0.8),
    stringsAsFactors = FALSE
  )
)

interactive_plot <- plotly::plot_ly(
  type = "sankey",
  arrangement = "fixed",
  node = list(
    label = interactive_nodes$label,
    color = interactive_nodes$color,
    pad = 14,
    thickness = 26,
    line = list(color = "#495057", width = 0.4),
    x = c(0.03, rep(0.38, length(mid_labels)), rep(0.70, nrow(category_rows))),
    y = c(
      0.17,
      seq(0.10, 0.80, length.out = length(mid_labels)),
      seq(0.05, 0.89, length.out = nrow(category_rows))
    ),
    hovertemplate = "%{label}<extra></extra>"
  ),
  link = list(
    source = interactive_links$source,
    target = interactive_links$target,
    value = interactive_links$value,
    color = interactive_links$color,
    hovertemplate = paste0("%{value:$,.0f} mensuales promedio<extra></extra>")
  )
) |>
  plotly::layout(
    font = list(size = 12, color = "#212529"),
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    margin = list(l = 20, r = 40, t = 30, b = 30)
  )

interactive_path <- file.path("output", "figures", "sankey_ingresos_gastos_interactivo.html")
htmlwidgets::saveWidget(
  plotly::as_widget(interactive_plot),
  interactive_path,
  selfcontained = TRUE
)

print(plot)

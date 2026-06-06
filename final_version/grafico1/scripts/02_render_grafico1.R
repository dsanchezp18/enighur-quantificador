options(scipen = 999)

required_pkgs <- c("dplyr", "ggplot2", "scales")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Faltan paquetes de R: ",
    paste(missing_pkgs, collapse = ", "),
    ". Instala esos paquetes antes de ejecutar este script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

fmt_usd <- function(x) scales::dollar(round(x), prefix = "$", big.mark = ",", accuracy = 1)

theme_grafico1 <- function() {
  theme_classic(base_size = 12) +
    theme(
      axis.text = element_text(colour = "grey20", size = 12),
      axis.title = element_text(size = 12),
      axis.line = element_line(colour = "grey60"),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(8, 60, 8, 12)
    )
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

  bind_rows(rows)
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

save_plot_files <- function(plot, output_dir, stem, width, height, dpi = 300) {
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  svg_path <- file.path(output_dir, paste0(stem, ".svg"))
  ggsave(png_path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(svg_path, plot = plot, width = width, height = height, device = grDevices::svg, bg = "white")
  invisible(c(png = png_path, svg = svg_path))
}

base_dir <- normalizePath(file.path(getwd(), "final_version", "grafico1"), winslash = "/", mustWork = FALSE)
if (!dir.exists(base_dir)) {
  base_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

processed_path <- file.path(base_dir, "data", "processed", "grafico1_processed.rds")
output_dir <- file.path(base_dir, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

processed <- readRDS(processed_path)
category_rows <- processed$category_rows

avg_ing_mon_cor <- processed$resumen$avg_ing_mon_cor
avg_gas_corr <- processed$resumen$avg_gas_corr
avg_gas_no_con <- processed$resumen$avg_gas_no_con
avg_ahorro <- processed$resumen$avg_ahorro

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

root_flows <- bind_rows(lapply(seq_along(root_allocations), function(i) {
  make_flow_polygon(
    x0 = root_flow_x0,
    x1 = root_flow_x1,
    from_range = root_allocations[[i]],
    to_range = mid_targets[[i]],
    fill = unname(mid_palette[[mid_labels[[i]]]]),
    flow_id = paste0("root-", i)
  )
}))

category_flows <- bind_rows(lapply(seq_len(nrow(category_rows)), function(i) {
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

node_rects <- bind_rows(
  mutate(root_stage, xmin = root_xmin, xmax = root_xmax, fill = "#F8F9FA"),
  mutate(mid_stage, xmin = mid_xmin, xmax = mid_xmax, fill = "#F8F9FA"),
  mutate(right_stage, xmin = right_xmin, xmax = right_xmax, fill = "#F8F9FA")
)

mid_stage <- mid_stage |>
  mutate(
    label_text = c(
      paste0("Gasto corriente\n", fmt_usd(avg_gas_corr)),
      paste0("Gasto de no consumo\n", fmt_usd(avg_gas_no_con)),
      paste0("Ahorro\n", fmt_usd(avg_ahorro))
    )
  )

right_stage <- right_stage |>
  mutate(label_text = paste0(.data$label, "\n", fmt_usd(.data$value)))

root_label <- paste0("Ingreso\nmonetario\n", fmt_usd(avg_ing_mon_cor))

plot <- ggplot() +
  geom_polygon(
    data = root_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_polygon(
    data = category_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_rect(
    data = node_rects,
    aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
    fill = "#F8F9FA",
    colour = "#495057",
    linewidth = 0.3
  ) +
  annotate(
    "text",
    x = 0.06,
    y = root_stage$ymid[[1]],
    label = root_label,
    size = 4.4,
    lineheight = 1.05,
    colour = "#212529"
  ) +
  geom_text(
    data = mid_stage,
    aes(x = mid_label_x, y = .data$ymid, label = .data$label_text),
    size = 4.3,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  geom_text(
    data = right_stage,
    aes(x = right_label_x, y = .data$ymid, label = .data$label_text),
    hjust = 0,
    size = 4.2,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  coord_cartesian(xlim = c(-0.28, 3.55), ylim = c(0, plot_height), clip = "off") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scales::label_dollar(prefix = "$", big.mark = ",")) +
  labs(x = NULL, y = NULL) +
  theme_grafico1() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank()
  )

save_plot_files(plot, output_dir = output_dir, stem = "grafico1", width = 6.5, height = 6.8, dpi = 300)
print(plot)

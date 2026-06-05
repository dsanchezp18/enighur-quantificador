source("scripts/utils.R")
ensure_packages(c("dplyr", "scales"))

bases_primarias_path <- file.path("data", "enighur", "2025", "Bases_primarias", "Bases_primarias.RData")

if (!file.exists(bases_primarias_path)) {
  stop("No se encontro la base primaria: ", bases_primarias_path, call. = FALSE)
}

load(bases_primarias_path)

if (!exists("ENIGHUR_F2_GDIARIOS_SECCION2")) {
  stop("La tabla ENIGHUR_F2_GDIARIOS_SECCION2 no existe en Bases_primarias.RData.", call. = FALSE)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  scale_x_discrete(
    labels = function(x) gsub(paste0(sep, ".+$"), "", x),
    ...
  )
}

wrap_product <- function(x, width = 34) {
  vapply(x, function(value) paste(strwrap(value, width = width), collapse = "\n"), character(1))
}

normalize_product <- function(x) {
  dplyr::case_when(
    startsWith(x, "Pan común blanco") ~ "Pan común blanco",
    startsWith(x, "Tomate riñón") ~ "Tomate riñón",
    startsWith(x, "Cebolla paiteña colorada") ~ "Cebolla paiteña",
    startsWith(x, "Cebolla paiteña perla") ~ "Cebolla paiteña",
    startsWith(x, "Papa chola") ~ "Papa chola",
    startsWith(x, "Pimiento común (verde)") ~ "Pimiento verde",
    startsWith(x, "Huevos de gallina") ~ "Huevos",
    startsWith(x, "Leche entera pasteurizada") ~ "Leche entera en funda",
    startsWith(x, "Queso fresco") ~ "Queso fresco",
    startsWith(x, "Pollo entero") ~ "Pollo entero",
    startsWith(x, "Banano (guineo)") ~ "Banano",
    startsWith(x, "Manzana fresca") ~ "Manzana",
    startsWith(x, "Plátano verde ") ~ "Plátano verde",
    startsWith(x, "Plátano maduro ") ~ "Plátano maduro",
    startsWith(x, "Yuca / Yuca encerada") ~ "Yuca",
    TRUE ~ x
  )
}

target_regions <- c("Costa", "Sierra", "Amazonía/Oriente")

food_daily <- ENIGHUR_F2_GDIARIOS_SECCION2

food_daily <- food_daily |>
  dplyr::transmute(
    hogar = .data$Identif_hog,
    fexp = as.numeric(.data$Fexp),
    region = dplyr::case_when(
      as.character(.data$REGION) == "Oriente" ~ "Amazonía/Oriente",
      TRUE ~ as.character(.data$REGION)
    ),
    producto = normalize_product(trimws(as.character(.data$GD1201E)))
  ) |>
  dplyr::filter(
    !is.na(.data$hogar),
    !is.na(.data$fexp),
    !is.na(.data$producto),
    nzchar(.data$producto),
    .data$region %in% target_regions
  )

households_by_region <- food_daily |>
  dplyr::distinct(.data$region, .data$hogar, .keep_all = TRUE) |>
  dplyr::group_by(.data$region) |>
  dplyr::summarise(total_hogares = sum(.data$fexp, na.rm = TRUE), .groups = "drop")

households_national <- food_daily |>
  dplyr::distinct(.data$hogar, .keep_all = TRUE) |>
  dplyr::summarise(total_hogares = sum(.data$fexp, na.rm = TRUE)) |>
  dplyr::mutate(region = "Nacional")

product_presence_region <- food_daily |>
  dplyr::distinct(.data$region, .data$hogar, .data$producto, .keep_all = TRUE) |>
  dplyr::group_by(.data$region, .data$producto) |>
  dplyr::summarise(hogares_producto = sum(.data$fexp, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(households_by_region, by = "region") |>
  dplyr::mutate(share_hogares = .data$hogares_producto / .data$total_hogares)

product_presence_national <- food_daily |>
  dplyr::distinct(.data$hogar, .data$producto, .keep_all = TRUE) |>
  dplyr::group_by(.data$producto) |>
  dplyr::summarise(hogares_producto = sum(.data$fexp, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(region = "Nacional") |>
  dplyr::left_join(households_national, by = "region") |>
  dplyr::mutate(share_hogares = .data$hogares_producto / .data$total_hogares)

national_top <- product_presence_national |>
  dplyr::arrange(dplyr::desc(.data$share_hogares), .data$producto) |>
  dplyr::slice_head(n = 6) |>
  dplyr::pull(.data$producto)

regional_standouts <- product_presence_region |>
  dplyr::left_join(
    product_presence_national |>
      dplyr::select(producto, national_share = share_hogares),
    by = "producto"
  ) |>
  dplyr::mutate(relative_lift = .data$share_hogares - .data$national_share) |>
  dplyr::group_by(.data$region) |>
  dplyr::slice_max(.data$relative_lift, n = 2, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::pull(.data$producto)

selected_products <- unique(c(national_top, regional_standouts))

plot_data <- dplyr::bind_rows(product_presence_national, product_presence_region) |>
  dplyr::filter(.data$producto %in% selected_products) |>
  dplyr::mutate(region = factor(.data$region, levels = c("Nacional", "Costa", "Sierra", "Amazonía/Oriente")))

product_order <- plot_data |>
  dplyr::group_by(.data$producto) |>
  dplyr::summarise(
    national_share = .data$share_hogares[.data$region == "Nacional"][1],
    max_share = max(.data$share_hogares),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(.data$national_share), dplyr::desc(.data$max_share), .data$producto) |>
  dplyr::pull(.data$producto)

label_map <- c(
  "Tomate riñón" = "Tomate riñón",
  "Pan común blanco" = "Pan común blanco",
  "Cebolla paiteña" = "Cebolla paiteña",
  "Huevos" = "Huevos",
  "Leche entera en funda" = "Leche entera",
  "Pimiento verde" = "Pimiento verde",
  "Papa chola" = "Papa chola",
  "Queso fresco" = "Queso fresco",
  "Plátano verde" = "Plátano verde",
  "Plátano maduro" = "Plátano maduro",
  "Plátano maduro barraganete fresco o refrigerado" = "Plátano maduro",
  "Pollo entero" = "Pollo entero",
  "Banano" = "Banano",
  "Manzana" = "Manzana",
  "Yuca" = "Yuca",
  "Yuca / Yuca encerada, fresca o refrigerada" = "Yuca"
)

heatmap_data <- plot_data |>
  dplyr::mutate(
    producto = factor(.data$producto, levels = rev(product_order)),
    producto_label = dplyr::coalesce(unname(label_map[as.character(.data$producto)]), as.character(.data$producto)),
    share_label = scales::percent(.data$share_hogares, accuracy = 0.1),
    text_colour = ifelse(.data$share_hogares >= 0.42, "white", "#163a59")
  ) |>
  dplyr::mutate(
    producto_label = factor(
      .data$producto_label,
      levels = rev(dplyr::coalesce(unname(label_map[product_order]), product_order))
    )
  )

table_data <- plot_data |>
  dplyr::transmute(
    ambito = as.character(.data$region),
    producto = .data$producto,
    hogares_expandidos = round(.data$hogares_producto, 0),
    hogares_totales_expandidos = round(.data$total_hogares, 0),
    share_hogares = round(.data$share_hogares, 4)
  ) |>
  dplyr::arrange(.data$ambito, dplyr::desc(.data$share_hogares))

output_table <- file.path("output", "tables", "productos_alimentos_mas_presentes_region_2025.csv")
dir.create(dirname(output_table), recursive = TRUE, showWarnings = FALSE)
write.csv(table_data, output_table, row.names = FALSE, fileEncoding = "UTF-8")

presence_plot <- ggplot(
  heatmap_data,
  aes(x = .data$region, y = .data$producto_label, fill = .data$share_hogares)
) +
  geom_tile(width = 0.92, height = 0.92, colour = "white", linewidth = 0.8) +
  geom_text(
    aes(label = .data$share_label, colour = .data$text_colour),
    size = 3.1,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#cbd5e1",
    high = "#163a59",
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_colour_identity() +
  labs(x = NULL, y = NULL) +
  theme_quantificador() +
  theme(
    axis.text.y = element_text(size = 12, lineheight = 0.95),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(8, 18, 8, 18)
  )

save_figure(
  "productos_alimentos_mas_presentes_region_2025.png",
  presence_plot,
  width = 11.25,
  height = 7.1
)

message("Saved figure: output/figures/productos_alimentos_mas_presentes_region_2025.png")
message("Saved table: output/tables/productos_alimentos_mas_presentes_region_2025.csv")

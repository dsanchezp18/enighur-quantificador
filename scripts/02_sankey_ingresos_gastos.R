suppressPackageStartupMessages({
  library(readxl)
  library(networkD3)
  library(dplyr)
})

options(scipen = 999)

tab_path <- file.path("data", "enighur", "2025", "Tabulados_ENIGHUR_2024-2025.xlsx")

if (!file.exists(tab_path)) stop("Tabulados not found: ", tab_path, call. = FALSE)

# ==============================================================================
# 1. READ NATIONAL TOTALS (expanded pesos) FROM CUADROS 2.1.1 AND 2.1.3
# ==============================================================================

ing_sheet <- read_excel(tab_path, sheet = "CUADRO 2.1.1", col_names = FALSE)
gas_sheet <- read_excel(tab_path, sheet = "CUADRO 2.1.3", col_names = FALSE)

# Col 2 = variable label, Col 3 = National expanded total

get_nacional_total <- function(sheet, pattern, exclude = NULL) {
  rows <- which(grepl(pattern, sheet[[2]], ignore.case = TRUE))
  if (!is.null(exclude)) {
    rows <- rows[!grepl(exclude, sheet[[2]][rows], ignore.case = TRUE)]
  }
  as.numeric(sheet[[3]][rows[1]])
}

ing_cor_tot_exp  <- get_nacional_total(ing_sheet, "Ingreso corriente total del hogar")
ing_mon_cor_exp  <- get_nacional_total(ing_sheet, "Ingreso corriente monetario del hogar")

gas_cor_mon_exp  <- get_nacional_total(gas_sheet, "Gasto corriente monetario", exclude = "no monetario")
gas_no_con_exp   <- get_nacional_total(gas_sheet, "Gasto de no consumo")

# ==============================================================================
# 2. READ NATIONAL AVERAGE TOTAL INCOME FROM CUADRO 2.2.1
#    (used to derive implied number of households)
# ==============================================================================

avg_sheet <- read_excel(tab_path, sheet = "CUADRO 2.2.1", col_names = FALSE)

# Nacional row: col 2 == "Nacional" and col 3 is NA, col 4 = average
nac_row <- which(avg_sheet[[2]] == "Nacional" & is.na(avg_sheet[[3]]))
avg_ing_cor_tot  <- as.numeric(avg_sheet[[4]][nac_row[1]])

# ==============================================================================
# 3. DERIVE AVERAGES
#    avg_X = (X_expanded_total / ing_cor_tot_expanded) * avg_ing_cor_tot
# ==============================================================================

avg_ing_mon_cor  <- (ing_mon_cor_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_cor_mon  <- (gas_cor_mon_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_no_con   <- (gas_no_con_exp  / ing_cor_tot_exp) * avg_ing_cor_tot

cat(sprintf("Ingreso monetario promedio:          $%.2f\n", avg_ing_mon_cor))
cat(sprintf("Gasto corriente monetario promedio:  $%.2f\n", avg_gas_cor_mon))
cat(sprintf("Gasto de no consumo promedio:        $%.2f\n", avg_gas_no_con))

# ==============================================================================
# 4. SANKEY
# ==============================================================================

fmt_usd <- function(x) sprintf("$%.0f", round(x))

nodes <- data.frame(
  name = c(
    paste0("Ingreso monetario\npromedio ", fmt_usd(avg_ing_mon_cor)),
    paste0("Gasto corriente\nmonetario promedio ", fmt_usd(avg_gas_cor_mon)),
    paste0("Gasto de no\nconsumo promedio ", fmt_usd(avg_gas_no_con))
  ),
  stringsAsFactors = FALSE
)

links <- data.frame(
  source = c(0L, 0L),
  target = c(1L, 2L),
  value  = c(avg_gas_cor_mon, avg_gas_no_con)
)

sankey <- sankeyNetwork(
  Links       = links,
  Nodes       = nodes,
  Source      = "source",
  Target      = "target",
  Value       = "value",
  NodeID      = "name",
  units       = "USD",
  fontSize    = 13,
  nodeWidth   = 30,
  nodePadding = 20,
  sinksRight  = TRUE
)

print(sankey)

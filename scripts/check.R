source("scripts/packages.R")

ensure_packages(c("tidyverse", "haven", "janitor", "ggplot2", "cowplot"))

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

ingresos_2012_candidates <- c(
  file.path("data", "enighur", "2012", "required", "ENIGHUR11_INGRESOS_H.sav"),
  file.path(
    "data", "enighur", "2012",
    "bbd_ingresos_gastos_2011-2012", "2011-2012", "Ingresos_Gastos",
    "02 BASE DE DATOS", "02 TABLAS DE TRABAJO", "04 ENIGHUR11_INGRESOS_H.sav"
  )
)

gastos_hmo_2012_candidates <- c(
  file.path("data", "enighur", "2012", "required", "ENIGHUR11_GASTOS_HMO.sav"),
  file.path(
    "data", "enighur", "2012",
    "bbd_ingresos_gastos_2011-2012", "2011-2012", "Ingresos_Gastos",
    "02 BASE DE DATOS", "02 TABLAS DE TRABAJO", "08 ENIGHUR11_GASTOS_HMO.sav"
  )
)

rdata_2025_path <- file.path(
  "data", "enighur", "2025",
  "Enighur_Bases_de_datos_R",
  "Bases de trabajo",
  "Bases_trabajo_R",
  "Bases_trabajo.RData"
)

required_files <- c(
  "scripts/01_ingresos_hogares.R",
  "scripts/packages.R",
  "scripts/utils.R",
  file.path("report", "report.qmd"),
  rdata_2025_path
)

missing_files <- required_files[!file.exists(required_files)]
missing_groups <- character(0)

invisible(tryCatch(
  resolve_existing_path(ingresos_2012_candidates, "2012 ingresos dataset"),
  error = function(e) {
    missing_groups <<- c(missing_groups, conditionMessage(e))
  }
))

invisible(tryCatch(
  resolve_existing_path(gastos_hmo_2012_candidates, "2012 gastos dataset"),
  error = function(e) {
    missing_groups <<- c(missing_groups, conditionMessage(e))
  }
))

if (length(missing_files) > 0 || length(missing_groups) > 0) {
  stop(
    "Missing required project files:\n- ",
    paste(c(missing_files, missing_groups), collapse = "\n- "),
    call. = FALSE
  )
}

cat("CHECK_OK\n")

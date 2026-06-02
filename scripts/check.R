source("scripts/packages.R")

ensure_packages(c("tidyverse", "haven", "janitor", "ggplot2", "cowplot"))

required_files <- c(
  "scripts/01_ingresos_hogares.R",
  "scripts/packages.R",
  "scripts/utils.R",
  "reporte.qmd",
  file.path("data", "enighur", "2025", "Enighur_Bases_de_datos_R", "Bases de trabajo", "Bases_trabajo_R", "Bases_trabajo.RData"),
  file.path("data", "enighur", "2012", "required", "ENIGHUR11_INGRESOS_H.sav"),
  file.path("data", "enighur", "2012", "required", "ENIGHUR11_GASTOS_HMO.sav")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required project files:\n- ",
    paste(missing_files, collapse = "\n- "),
    call. = FALSE
  )
}

cat("CHECK_OK\n")

# ============================================================
# packages.R
# Helper para instalar (si hace falta) y cargar paquetes de R.
# ============================================================

project_library <- function() {
  lib <- file.path(getwd(), ".Rlib")
  if (!dir.exists(lib)) {
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  }
  lib
}

ensure_packages <- function(pkgs, repos = "https://cloud.r-project.org") {
  lib <- project_library()
  .libPaths(c(lib, .libPaths()))

  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    message("Instalando paquetes faltantes: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, repos = repos, lib = lib)
  }

  still_missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop(
      "No se pudieron cargar estos paquetes: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(lapply(
    pkgs,
    function(pkg) suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  ))
}

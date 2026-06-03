# ============================================================
# packages.R
# Helper para validar y cargar paquetes de R.
# ============================================================

ensure_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      paste0(
        "Faltan paquetes de R: ",
        paste(missing_pkgs, collapse = ", "),
        ". Instala esos paquetes antes de ejecutar scripts."
      ),
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

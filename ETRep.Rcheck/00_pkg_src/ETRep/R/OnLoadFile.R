#' Package startup hook
#'
#' This special function is called automatically when the ETRep package is loaded.
#' It ensures that `rgl` uses the off-screen WebGL device (`useNULL = TRUE`)
#' on macOS and other headless environments, so that package installation
#' and examples do not fail with an OpenGL error.
#'
#' @param libname Character string; the path to the package library.
#' @param pkgname Character string; the name of the package.
#'
#' If the user has not already set `options(rgl.useNULL)`, this function sets it
#' to `TRUE` to suppress the typical warning:
#' \emph{"rgl.init failed, will use the null device"}.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("rgl.useNULL"))) {
    options(rgl.useNULL = TRUE)
  }
}
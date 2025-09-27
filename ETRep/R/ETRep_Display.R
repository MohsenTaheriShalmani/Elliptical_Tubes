#' Open an rgl device with fallback to WebGL
#'
#' This function tries to open a native rgl window. If that fails
#' (e.g. on macOS without OpenGL) it falls back to an off-screen
#' device suitable for rendering with rglwidget().
#'
#' @param show_widget logical; if TRUE and native OpenGL is not available,
#'        a message is displayed suggesting to use etrep_show3d().
#' @param ... additional arguments passed to [rgl::open3d()].
#' @return Device ID returned by [rgl::open3d()].
#' @keywords internal
.etrep_open3d <- function(show_widget = TRUE, ...) {
  # make sure rgl.useNULL is defined
  if (is.null(getOption("rgl.useNULL")))
    options(rgl.useNULL = TRUE)
  
  # try native device first
  dev_id <- try(rgl::open3d(useNULL = FALSE, ...), silent = TRUE)
  
  use_null <- inherits(dev_id, "try-error")
  
  if (use_null) {
    # fallback
    dev_id <- rgl::open3d(useNULL = TRUE, ...)
    if (show_widget) {
      packageStartupMessage(
        "Using WebGL via rglwidget() as Native OpenGL device not available."
      )
    }
  }
  
  invisible(dev_id)
}

#' Display current rgl scene in the browser
#'
#' Saves the current rgl scene to a temporary HTML file and opens it
#' in the system's default browser.
#'
#' @param width,height size of the viewer in pixels.
#' @return The path to the HTML file (invisible).
#' @keywords internal
.etrep_show3d <- function(width = 800, height = 600) {
  f <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(
    rgl::rglwidget(width = width, height = height),
    file = f, selfcontained = TRUE
  )
  utils::browseURL(f)
  invisible(f)
}



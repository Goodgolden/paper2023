
#' Title
#'
#' @return
#' @export
#'
#' @examples

run_shiny <- function() {
  appDir <- system.file("shiny-examples",
                        "myapp",
                        package = "plmlmm")


  if (appDir == "") {
    stop("Could not find example directory.
         Try re-installing `plmlmm`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


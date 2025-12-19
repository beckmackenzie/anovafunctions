#' Run the One-way ANOVA Shiny App
#' @importFrom shiny runApp
#' @export
run_oneway_app <- function() {
  app_dir <- system.file("shiny", "oneway_app", package = "anovafunctions")
  if (app_dir == "") stop("Could not find the Shiny app directory. Try reinstalling.")
  shiny::runApp(app_dir, display.mode = "normal")
}
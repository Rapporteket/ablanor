#' Run the Ablanor Shiny Application
#'
#' @return An object representing the Ablanor app
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}

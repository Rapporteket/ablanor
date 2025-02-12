#' Run the Ablanor Shiny Application
#'
#' @return An object representing the Ablanor app
#' @export

run_app <- function(browser = FALSE, logAsJson = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }

  shiny::shinyApp(ui = app_ui, server = app_server, options = list(launch.browser = browser))
}

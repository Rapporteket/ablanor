#' Run the Ablanor Shiny Application
#'
#' @return An object representing the Ablanor app
#' @export

run_app <- function(browser = FALSE, logAsJson = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  if (browser) {
    options(shiny.launch.browser = TRUE)
  }

  shiny::shinyApp(ui = app_ui, server = app_server)
}

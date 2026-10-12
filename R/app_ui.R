#' Client (ui) for the Ablanor app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {
  reg_title <- "AblaNor"

  ui <- shiny::tagList(
    shiny::navbarPage(
      title = rapbase::regTitle(reg_title),
      windowTitle = reg_title,
      theme = rapbase::rapTheme(),
      id = "tabs",

      shiny::tabPanel(
        "Start",
          shiny::mainPanel(
            shiny::htmlOutput("veiledning", inline = TRUE),
            rapbase::navbarWidgetInput("ablanorWidget", selectOrganization = TRUE)
          )
      ),
      shiny::navbarMenu(
        title = "Månedsrapporter",

        shiny::tabPanel(
          title = "Sykehus vs Nasjonal",
          shiny::uiOutput("hospitalReport")
        # ),
        #
        # shiny::tabPanel(
        #   title = "Operatør vs Sykehus",
        #   shiny::uiOutput("operatorReport")
        )
      ),

      shiny::tabPanel(
        "Abonnement",
        shiny::sidebarPanel(
          rapbase::autoReportFormatInput("ablanorSubscription"),
          rapbase::autoReportInput("ablanorSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("ablanorSubscription")
        )
      )
    )
  )
}

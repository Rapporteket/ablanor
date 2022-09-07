#' Client (ui) for the Ablanor app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  reg_title <- "AblaNor"

  ui <- shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        reg_title),
      windowTitle = reg_title,
      theme = "rap/bootstrap.css",
      id = "tabs",

      shiny::tabPanel(
        "Start",
        shinyalert::useShinyalert(),
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput("veiledning", inline = TRUE),
          rapbase::appNavbarUserWidget(
            user = shiny::uiOutput("appUserName"),
            organization = shiny::uiOutput("appOrgName"),
            addUserInfo = TRUE),
          shiny::tags$head(
            shiny::tags$link(rel = "shortcut icon", href = "rap/favicon.ico")
          )
        )
      ),

      shiny::tabPanel(
        "Utforsker",
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("selectDataSet")),
          shiny::column(6, shiny::uiOutput("selectVars"))
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput("togglePivotSurvey"))
        ),
        shiny::fluidRow(
          shiny::column(12, rpivotTable::rpivotTableOutput("pivotSurvey"))
        )
      ),

      shiny::tabPanel(
        "Kodebok",
        shiny::sidebarLayout(
          shiny::sidebarPanel(shiny::uiOutput("kbControl")),
          shiny::mainPanel(shiny::htmlOutput("kbdData"))
        )
      ),

      shiny::tabPanel(
        "Datadump",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 4,
            shiny::selectInput("dumpDataSet", "Velg datasett:",
                               c("basereg",
                                 "friendlycentre",
                                 "mce",
                                 "patientlist",
                                 "pros",
                                 "rand12",
                                 "followup",
                                 "pros_patient_followup",
                                 "kodeboken")),
            shiny::dateRangeInput(
              "dumpDateRange", "Velg periode:",
              start = lubridate::ymd(Sys.Date()) - lubridate::years(1),
              end = Sys.Date(), separator = "-",
              weekstart = 1
            ),
            shiny::radioButtons("dumpFormat", "Velg filformat:",
                         choices = c("csv", "xlsx-csv")),
            shiny::downloadButton("dumpDownload", "Hent!")
          ),
          shiny::mainPanel(
            shiny::htmlOutput("dataDumpInfo")
          )
        )
      ),

      shiny::tabPanel(
        "Månedsrapporter",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::radioButtons("formatReport",
                                "Format for nedlasting",
                                list(PDF = "pdf", HTML = "html"),
                                inline = FALSE),
            shiny::downloadButton("downloadReport", "Last ned!"),
            width = 2
          ),
          shiny::mainPanel(
            shiny::htmlOutput("maanedligRapport", inline = TRUE)
          )
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
      ),

      shiny::navbarMenu(
        "Verktøy",

        shiny::tabPanel(
          "Utsending",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::autoReportFormatInput("ablanorDispatchment"),
              rapbase::autoReportOrgInput("ablanorDispatchment"),
              rapbase::autoReportInput("ablanorDispatchment")
            ),
            shiny::mainPanel(
              rapbase::autoReportUI("ablanorDispatchment")
            )
          )
        ),

        shiny::tabPanel(
          "Eksport",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::exportUCInput("ablanorExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("ablanorExportGuide")
            )
          )
        ),

        shiny::tabPanel(
          "Bruksstatistikk",
          shiny::sidebarLayout(
            shiny::sidebarPanel(rapbase::statsInput("ablanorStats")),
            shiny::mainPanel(rapbase::statsUI("ablanorStats"))
          )
        )
      )
    )
  )
}

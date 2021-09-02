# shiny web app ui

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
      "Datadump",
      # ADD SOME CONTENT, Should we downoad an already controlled
      # datadump using les_data_ablanor()?
      # Eksempelkode basert på NORIC:
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput("dumpDataSet", "Velg datasett:",
                      c("basereg",
                        "friendlycentre",
                        "mce",
                        "patientlist",
                        "pros")),
          dateRangeInput("dumpDateRange", "Velg periode:",
                         start = lubridate::ymd(Sys.Date()) -
                           lubridate::years(1),
                         end = Sys.Date(), separator = "-",
                         weekstart = 1),
          radioButtons("dumpFormat", "Velg filformat:",
                       choices = c("csv", "xlsx-csv")),
          downloadButton("dumpDownload", "Hent!")
        ),
        mainPanel(
          htmlOutput("dataDumpInfo")
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
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 3,
          shiny::uiOutput("subscriptionRepList"),
          shiny::selectInput("subscriptionFreq",
                             "Frekvens:",
                             list(Årlig = "Årlig-year",
                                   Kvartalsvis = "Kvartalsvis-quarter",
                                   Månedlig = "Månedlig-month",
                                   Ukentlig = "Ukentlig-week",
                                   Daglig = "Daglig-DSTday"),
                             selected = "Månedlig-month"),
          shiny::selectInput("subscriptionFileFormat",
                             "Format:",
                             c("html", "pdf")),
          shiny::actionButton("subscribe", "Bestill!")
        ),
        shiny::mainPanel(
          shiny::uiOutput("subscriptionContent")
        )
      )
    ),

    shiny::tabPanel(
      "Utsending",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportOrgInput("ablanorDispatchment"),
          rapbase::autoReportFormatInput("ablanorDispatchment"),
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
    )
  )
)

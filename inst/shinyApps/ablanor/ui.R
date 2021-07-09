# shiny web app ui

shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
reg_title <- "AblaNor"


ui <- shiny::tagList(
  shiny::navbarPage(
    title = shiny::div(shiny::a(shiny::includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                reg_title),
    windowTitle = reg_title,
    theme = "rap/bootstrap.css",
    id = "tabs",

    shiny::tabPanel("Start",
             shinyalert::useShinyalert(),
             mainPanel(width = 12,
                       shiny::htmlOutput("veiledning", inline = TRUE),
                       rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                           organization =
                                             uiOutput("appOrgName"),
                                           addUserInfo = TRUE),
                       shiny::tags$head(shiny::tags$link(rel = "shortcut icon",
                                           href = "rap/favicon.ico"))
             )

    ),



    shiny::tabPanel("Utforsker",
             shiny::fluidRow(
               column(6, uiOutput("selectDataSet")),
               column(6, uiOutput("selectVars"))
             ),
             shiny::fluidRow(
               column(12, uiOutput("togglePivotSurvey"))
             ),
             shiny::fluidRow(
               column(12,
                      rpivotTableOutput("pivotSurvey"))
             )
    ),




    shiny::tabPanel("Datadump",
             # ADD SOME CONTENT, Should we downoad an already controlled
             # datadump using les_data_ablanor()?
    ),

    shiny::tabPanel("Månedsrapporter",
             shiny::sidebarLayout(
               shiny::sidebarPanel(
                 shiny::radioButtons("formatReport",
                              "Format for nedlasting",
                              c("PDF", "HTML"),
                              inline = FALSE),
                 shiny::downloadButton("downloadReport", "Last ned!"),
                 width = 2
               ),
               shiny::mainPanel(
                 shiny::htmlOutput("maanedligRapport", inline = TRUE)
               )

             )
    ),


    shiny::tabPanel("Abonnement",
             shiny::sidebarLayout(
               shiny::sidebarPanel(width = 3,
                            shiny::uiOutput("subscriptionRepList"),
                            shiny::selectInput("subscriptionFreq", "Frekvens:",
                                        list(Årlig = "Årlig-year",
                                              Kvartalsvis = "Kvartalsvis-quarter",
                                              Månedlig = "Månedlig-month",
                                              Ukentlig = "Ukentlig-week",
                                              Daglig = "Daglig-DSTday"),
                                        selected = "Månedlig-month"),
                            shiny::selectInput("subscriptionFileFormat", "Format:",
                                        c("html", "pdf")),
                            shiny::actionButton("subscribe", "Bestill!")
               ),
               shiny::mainPanel(
                 shiny::uiOutput("subscriptionContent")
               )
             )
    )
  )
)

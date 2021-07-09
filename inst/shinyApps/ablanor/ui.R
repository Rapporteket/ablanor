#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ablanor)
library(rapbase)
library(raplog)
library(magrittr)
library(rpivotTable)

addResourcePath("rap", system.file("www", package = "rapbase"))
reg_title <- "AblaNor"



ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                reg_title),
    windowTitle = reg_title,
    theme = "rap/bootstrap.css",
    id = "tabs",

    tabPanel("Start",
             shinyalert::useShinyalert(),
             mainPanel(width = 12,
                       htmlOutput("veiledning", inline = TRUE),
                       appNavbarUserWidget(user = uiOutput("appUserName"),
                                           organization =
                                             uiOutput("appOrgName"),
                                           addUserInfo = TRUE),
                       tags$head(tags$link(rel = "shortcut icon",
                                           href = "rap/favicon.ico"))
             )

    ),



    tabPanel("Utforsker",
             fluidRow(
               column(6, uiOutput("selectDataSet")),
               column(6, uiOutput("selectVars"))
             ),
             fluidRow(
               column(12, uiOutput("togglePivotSurvey"))
             ),
             fluidRow(
               column(12,
                      rpivotTableOutput("pivotSurvey"))
             )
    ),




    tabPanel("Datadump",
             # ADD SOME CONTENT, Should we downoad an already controlled
             # datadump using les_data_ablanor()?
    ),

    tabPanel("Månedsrapporter",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("formatReport",
                              "Format for nedlasting",
                              c("PDF", "HTML"),
                              inline = FALSE),
                 downloadButton("downloadReport", "Last ned!"),
                 width = 2
               ),
               mainPanel(
                 htmlOutput("maanedligRapport", inline = TRUE)
               )

             )
    ),


    tabPanel("Abonnement",
             sidebarLayout(
               sidebarPanel(width = 3,
                            uiOutput("subscriptionRepList"),
                            selectInput("subscriptionFreq", "Frekvens:",
                                        list(Årlig = "Årlig-year",
                                              Kvartalsvis = "Kvartalsvis-quarter",
                                              Månedlig = "Månedlig-month",
                                              Ukentlig = "Ukentlig-week",
                                              Daglig = "Daglig-DSTday"),
                                        selected = "Månedlig-month"),
                            selectInput("subscriptionFileFormat", "Format:",
                                        c("html", "pdf")),
                            actionButton("subscribe", "Bestill!")
               ),
               mainPanel(
                 uiOutput("subscriptionContent")
               )
             )
    )
  )
)

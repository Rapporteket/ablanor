#' Server logic for the Ablanor app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting AblaNor application")


  # Parameters that will remain throughout the session
  registryName <- "ablanor"
  userFullName <- Sys.getenv("FALK_USER_FULLNAME")
  mapOrgId <- ablanor::getNameReshId()
  userOperator <- "Test Operatoresen"

  mapOrgIdMod <- mapOrgId %>% dplyr::rename(UnitId = id, orgname = name)
  user <- rapbase::navbarWidgetServer2(
    "ablanorWidget", "Ablanor", map_orgname = mapOrgIdMod, caller = packageName()
  )

  shiny::observeEvent(user$role(), {
    shiny::removeTab(inputId = "tabs", target = "Utforsker")
    shiny::removeTab(inputId = "tabs", target = "Datadump")
    shiny::removeTab(inputId = "tabs", target = "Kodebok")
    shiny::removeTab(inputId = "tabs", target = "Verktøy")
    if (user$role() == "LC" || user$role() == "SC") {
      # Legg til faner for Utforsker, Kodebok og Datadump for LC og SC brukere
      shiny::insertTab(inputId = "tabs",
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
        target = "Start",
        position = "after"
      )
      shiny::insertTab(inputId = "tabs",
        shiny::tabPanel(
          "Kodebok",
          shiny::sidebarLayout(
            shiny::sidebarPanel(shiny::uiOutput("kbControl")),
            shiny::mainPanel(shiny::htmlOutput("kbdData"))
          )
        ),
        target = "Utforsker",
        position = "after"
      )
      shiny::insertTab(inputId = "tabs",
        shiny::tabPanel(
          title = "Datadump",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              width = 4,
              shiny::uiOutput(outputId = "selectDumpSet"),
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
        target = "Kodebok",
        position = "after")
    }
    if (user$role() %in% c("LU", "LC")) {
      shiny::showTab(inputId = "tabs", target = "Abonnement")
      shiny::showTab(inputId = "tabs", target = "Månedsrapporter")
    }

    if (user$role() == "LU") {
      shiny::hideTab(inputId = "tabs", target = "Sykehus vs Nasjonal")
    }
    if (user$role() == "SC") {
      # Legg til fane for verktøy for SC-brukere
      shiny::appendTab(inputId = "tabs",
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
      shiny::hideTab(inputId = "tabs", target = "Abonnement")
      shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
    }
  })

  dataSets <- list(
    `Bruk og valg av data` = "info",

    # SAMLETABELLER MED UTLEDETE VARIABLER
    `Pasient, prosedyre og kvalitetsindikatorer` = "basereg_pros_indik",
    `Pasient, prosedyre og hendelse` = "basereg_pros_hendelse",
    `Pasient, prosedyre og eprom basis` = "pros_pat_followup0",
    `Pasient, prosedyre og eProm 1 år` = "pros_pat_followup1",
    `Pasient, prosedyre og eProm 5 år` = "pros_pat_followup5",

    # RÅDATA:
    `Basisskjema rådata` = "basereg",
    `Prosedyreskjema rådata` = "pros",
    `Forløpsoversikt rådata` = "mce",
    `RAND-12: basis, 1 og 5 år. Rådata.` = "rand12",
    `eProm basis. Rådata` = "followupbasis",
    `eProm 1 år. Rådata` = "followup1",
    `eProm 5 år. Rådata` = "followup5",
    `GKV (pasienterfaring) basis. Rådata` = "gkv")

  contentDump <- function(file, type, userRole, reshId) {
    d <- ablanor::getDataDump(tableName = input$dumpDataSet,
                              fromDate = input$dumpDateRange[1],
                              toDate = input$dumpDateRange[2],
                              session = session,
                              userRole = userRole,
                              reshId = reshId)
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }


  # Start
  output$veiledning <- shiny::renderUI({
    if (is.null(user$org())) {
      NULL
    } else {
      rapbase::renderRmd(
        system.file("veiledning.Rmd", package = "ablanor"),
        outputType = "html_fragment",
        params = list(title = "empty title",
                      author = user$fullName(),
                      hospitalName = getHospitalName(user$org()),
                      tableFormat = "html",
                      reshId = user$org())
      )
    }
  })
  output$download_report <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "veiledning",
                        fileext = paste0(".", input$format_report)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("veiledning.Rmd", package = "ablanor"),
        outputType = input$format_report,
        params = list(
          author = user$fullName(),
          tableFormat = input$format_report,
          reshId = user$org()
        )
      )
      file.rename(fn, file)
    }
  )


  # Utforsker
  ## reactive values
  rvals <- shiny::reactiveValues()
  rvals$showPivotTable <- FALSE
  rvals$togglePivotingText <- "Last valgte data!"
  rvals$selectedDataSet <- "info"
  rvals$selectedVars <- ""


  ## observers
  shiny::observeEvent(input$togglePivoting, {
    if (rvals$showPivotTable) {
      rvals$showPivotTable <- FALSE
      rvals$togglePivotingText <- "Last valgte data!"
      # persist last choice
      rvals$selectedDataSet <- input$selectedDataSet
      rvals$selectedVars <- input$selectedVars
    } else {
      rvals$showPivotTable <- TRUE
      rvals$togglePivotingText <- "Endre valg av data!"
    }
  })

  shiny::observeEvent(input$selectedDataSet, {
    rvals$selectedVars <- ""
  })


  dat <- shiny::reactive({
    ablanor::getPivotDataSet(setId = input$selectedDataSet,
                             session = session,
                             reshId = user$org(),
                             userRole = user$role())
  })


  metaDat <- shiny::reactive({
    ablanor::getPivotDataSet(setId = input$selectedDataSet,
                             session = session,
                             reshId = user$org(),
                             userRole = user$role(),
                             singleRow = TRUE)
  })



  ##  OUTPUTS
  output$selectDataSet <- shiny::renderUI({
    if (rvals$showPivotTable) {
      NULL
    } else {
      shiny::tagList(
        shiny::selectInput(
          inputId = "selectedDataSet", label = "Velg datasett:",
          choices = dataSets, selected = rvals$selectedDataSet),
        shiny::checkboxInput("isSelectAllVars", "Velg alle variabler")
      )
    }
  })


  output$selectVars <- shiny::renderUI({
    if (length(input$isSelectAllVars) == 0) {
      NULL
    } else {
      if (length(rvals$showPivotTable) == 0 || rvals$showPivotTable) {
        shiny::h4(paste("Valgt datasett:",
                        names(dataSets)[dataSets == input$selectedDataSet]))
      } else {
        if (input$isSelectAllVars) {
          vars <- names(metaDat())
        } else {
          vars <- rvals$selectedVars
        }

        shiny::selectInput(inputId = "selectedVars", label = "Velg variabler:",
                           choices = names(metaDat()), multiple = TRUE,
                           selected = vars)
      }
    }
  })




  output$togglePivotSurvey <- shiny::renderUI({
    if (length(input$selectedVars) == 0) {
      NULL
    } else {
      shiny::actionButton(inputId = "togglePivoting",
                          label = rvals$togglePivotingText)
    }
  })



  output$pivotSurvey <- rpivotTable::renderRpivotTable({
    if (rvals$showPivotTable) {
      rpivotTable::rpivotTable(
        data = dat()[input$selectedVars],
        sorters = ablanor::make_sorters(df = dat()[input$selectedVars]))
    } else {
      rpivotTable::rpivotTable(data.frame())
    }
  })



  # Kodebok
  kodebok <- ablanor::getKodebokMedUtledetedVar()
  metaDatKb <- shiny::reactive({
    ablanor::getPivotDataSet(setId = input$kbdTab,
                             session = session,
                             reshId = user$org(),
                             userRole = user$role(),
                             singleRow = TRUE)
  })

  ## innhold kontrollpanel:
  output$kbControl <- renderUI({
    selectInput(inputId = "kbdTab",
                label = "Vis kodebok for tabellen:",
                choices =  dataSets)
  })

  # vektor med alle variabelnavn i valgt tabell
  selectedkbTabVars <- reactive({
    if (input$kbdTab %in% c("basereg",
                            "pros",
                            "mce",
                            "rand12",
                            "followupbasis",
                            "followup1",
                            "followup5",
                            "gkv",
                            "proms",
                            "basereg_pros_indik",
                            "basereg_pros_hendelse",
                            "pros_pat_followup0",
                            "pros_pat_followup1",
                            "pros_pat_followup5")) {
      metaDatKb() %>% names()
    }
    else {
      data.frame()
    }
  })

  output$kbdTable <- DT::renderDataTable(
    # kodebok ablanor, Kun variabelnavn som finnes den valgte tabellen
    kodebok[kodebok$fysisk_feltnavn %in% selectedkbTabVars(), ],
    options = list(
      lengthMenu = c(25, 50, 100, 200, 400),
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      )
    )
  )

  output$kbdData <- renderUI({
    DT::dataTableOutput("kbdTable")
  })




  # Datadump

  # Datasets avaliable for download

  output$selectDumpSet <- shiny::renderUI({
    dataSetsDump <- c("basereg",
                      "pros",
                      "mce",
                      "rand12",
                      "followupbasis",
                      "followup1",
                      "followup5",
                      "gkv",
                      "hendelse",
                      "kodeboken")
    if (user$role() == "SC") {
      dataSetsDump <- c("basereg_prosedyre",
                        dataSetsDump,
                        "proms",
                        "patientlist",
                        "friendlycentre",
                        "mce_patient_data")
    }
    htmltools::tagList(
      shiny::selectInput(inputId = "dumpDataSet",
                         label = "Velg datasett:",
                         choices = dataSetsDump))
  })



  output$dataDumpInfo <- shiny::renderUI({
    shiny::p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(
        file, input$dumpFormat, userRole = user$role(), reshId = user$org()
      )
    }
  )

  # Månedlige rapport

  output$hospitalReport <- shiny::renderUI({
    # Prosedyrer report not for LU or national user (SC).
    if (user$role() %in% c("LU", "SC")) {
      NULL
    } else {
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          style = "position:fixed;width:130px;",
          h5("Last ned rapporten (pdf)"),
          shiny::downloadButton("downloadReportSykehus", "Hent!"),
          width = 2
        ),
        shiny:: mainPanel(
          shiny:: htmlOutput("prosedyrerSykehus", inline = TRUE)
        )
      )
    }
  })

  output$operatorReport <- shiny::renderUI({
    # Prosedyrer report not for national user (SC).
    if (user$role() == "SC") {
      NULL
    } else {
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          style = "position:fixed;width:130px;",
          h5("Last ned rapporten (pdf)"),
          shiny::downloadButton("downloadReportOperator", "Hent!"),
          width = 2
        ),
        shiny:: mainPanel(
          shiny:: htmlOutput("prosedyrerOperator", inline = TRUE)
        )
      )
    }
  })


  output$prosedyrerSykehus <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("AblaNor_local_monthly.Rmd", package = "ablanor"),
      outputType = "html_fragment",
      params = list(
        author = user$fullName,
        hospitalName = ablanor::getHospitalName(user$org()),
        tableFormat = "html",
        reshId = user$org(),
        registryName = registryName,
        userRole = user$role(),
        userFullName = userFullName,
        userOperator = userOperator
      )
    )
  })

  output$prosedyrerOperator <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("AblaNor_operator_vs_local_monthly.Rmd",
                  package = "ablanor"),
      outputType = "html_fragment",
      params = list(
        author = user$fullName(),
        hospitalName = ablanor::getHospitalName(user$org()),
        tableFormat = "html",
        reshId = user$org(),
        registryName = registryName,
        userRole = user$role(),
        userFullName = userFullName,
        userOperator = userOperator
      )
    )
  })


  output$downloadReportSykehus <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "AblaNor_local_vs_national_monthly",
                        fileext = paste0(".", "pdf")))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("AblaNor_local_monthly.Rmd", package = "ablanor"),
        outputType = "pdf",
        params = list(
          author = user$fullName,
          hospitalName = getHospitalName(user$org()),
          tableFormat = "pdf",
          reshId = user$org(),
          registryName = registryName,
          userFullName = userFullName,
          userRole = user$role(),
          userOperator = userOperator
        )
      )
      file.rename(fn, file)
    }
  )


  output$downloadReportOperator <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "AblaNor_operator_vs_local_monthly",
                        fileext = paste0(".", "pdf")))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("AblaNor_operator_vs_local_monthly.Rmd",
                    package = "ablanor"),
        outputType = "pdf",
        params = list(
          author = user$fullName(),
          hospitalName = getHospitalName(user$org()),
          tableFormat = "pdf",
          reshId = user$org(),
          registryName = registryName,
          userFullName = userFullName,
          userRole = user$role(),
          userOperator = userOperator
        )
      )
      file.rename(fn, file)
    }
  )


  # Abonnement

  # Values shared among subscriptions and dispatchment
  orgs <- ablanor::getNameReshId(asNamedList = TRUE,
                                 shortName = FALSE,
                                 newNames = TRUE)


  subReports <- shiny::reactiveVal(
    list()
  )


  shiny::observeEvent(list(user$org(), user$role()), {
    subReports(list(
      "Månedlige resultater: Operatør vs. Sykehus" = list(
        synopsis = paste0("AblaNor - ",
                          "Egne registreringer opp mot ",
                          "sykehusets registreringer"),
        fun = "reportProcessor",
        paramNames = c("report",
                       "outputType",
                       "title",
                       "orgId",
                       "userFullName"),
        paramValues = c("AblaNor_operator_vs_local_monthly",
                        "pdf",
                        "Månedsresultater",
                        999999,
                        "userFullName")
      )
    ))

    if (user$role() == "LC") {
      subReportsSykehus <- list(
        "Månedlige resultater: Sykehus vs. Nasjonal" = list(
          synopsis = paste0("AblaNor - ",
                            "Eget sykehus mot nasjonale resultater"),
          fun = "reportProcessor",
          paramNames = c("report",
                         "outputType",
                         "title",
                         "orgId",
                         "userFullName"),
          paramValues = c("AblaNor_local_monthly",
                          "pdf",
                          "Månedsresultater",
                          999999,
                          "userFullName")
        )
      )
      subReports(c(subReports(), subReportsSykehus))
    }
  })

  subParamNames <- shiny::reactive(c(
    "orgId",
    "orgName",
    "userFullName",
    "userRole",
    "registryName",
    "tableFormat"
  ))
  subParamValues <- shiny::reactive(c(
    user$org(),
    user$orgName(),
    user$fullName(),
    user$role(),
    registryName,
    "latex"
  ))


  ## serve subscriptions (Abonnement)
  rapbase::autoReportServer(
    id = "ablanorSubscription",
    registryName = "ablanor",
    type = "subscription",
    org = org$value,
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = subReports,
    orgs = orgs,
    user = user,
    runAutoReportButton = (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC",
                                                               "DEV",
                                                               "TEST"))
  )

  # Utsendelse

  org <- rapbase::autoReportOrgServer("ablanorDispatchment", orgs)
  disFormat <- rapbase::autoReportFormatServer("ablanorDispatchment")

  disParamNames <- shiny::reactive(c(
    "orgId",
    "outputType",
    "userFullName"
  ))
  disParamValues <- shiny::reactive(c(
    org$value(),
    disFormat(),
    user$fullName()
  ))

  rapbase::autoReportServer(
    id = "ablanorDispatchment",
    registryName = "ablanor",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = list(
      "Månedlige resultater: Sykehus vs. Nasjonal" = list(
        synopsis = paste0("AblaNor - ",
                          "Eget sykehus mot nasjonale resultater"),
        fun = "reportProcessor",
        paramNames = c("report",
                       "outputType",
                       "title",
                       "orgId",
                       "userFullName"),
        paramValues = c("AblaNor_local_monthly",
                        "pdf",
                        "Månedsresultater",
                        999999,
                        "userFullName")
      )
    ),
    orgs = orgs,
    user = user,
    runAutoReportButton = (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC",
                                                               "DEV",
                                                               "TEST"))
  )


  # Eksport
  rapbase::exportUCServer("ablanorExport", registryName)
  rapbase::exportGuideServer("ablanorExportGuide", registryName)

  # Brukerstatistikk
  rapbase::statsServer(
    "ablanorStats",
    registryName,
    app_id = Sys.getenv("FALK_APP_ID")
  )
}

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
  dataSets <- list(
    `Bruk og valg av data` = "info",
    `Prosedyre, basisskjema og oppfølging` = "pros_patient",
    `RAND-12` = "rand12",
    `Basisskjema` = "basereg"

  )

  registryName <- "ablanor"
  mapOrgId <- ablanor::getNameReshId(registryName)
  reshId <- rapbase::getUserReshId(session)
  hospitalName <- ablanor::getHospitalName(registryName, reshId)
  userFullName <- rapbase::getUserFullName(session)
  userRole <- rapbase::getUserRole(session)
  userOperator <- "Test Operatoresen"
  author <- userFullName
  # userOperator <- ? #@fixme

  # Hide all tabs if LU -role
  if (userRole == "LU") {
    shiny::hideTab(inputId = "tabs", target = "Utforsker")
    shiny::hideTab(inputId = "tabs", target = "Datadump")
    shiny::hideTab(inputId = "tabs", target = "Kodebok")
    shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
    shiny::hideTab(inputId = "tabs", target = "Abonnement")
    shiny::hideTab(inputId = "tabs", target = "Verktøy")
  }


  # Hide tabs when not role 'SC'
  if (userRole != "SC") {
    shiny::hideTab(inputId = "tabs", target = "Verktøy")
  }

  # Hide tabs when role 'SC'
  if (userRole == "SC") {
    shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
  }


  contentDump <- function(file, type, userRole, reshId) {
    d <- ablanor::getDataDump(registryName, input$dumpDataSet,
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


  # widget
  output$appUserName <- shiny::renderText(userFullName)
  output$appOrgName <- shiny::renderText(
    paste(hospitalName, userRole, sep = ", "))

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session,
                                                 callerPkg = "ablanor")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert(
      "Dette vet Rapporteket om deg:", userInfo,
      type = "", imageUrl = "rap/logo.svg",
      closeOnEsc = TRUE, closeOnClickOutside = TRUE,
      html = TRUE, confirmButtonText = rapbase::noOptOutOk()
    )
  })




  # Start
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "ablanor"),
      outputType = "html_fragment",
      params = list(title = "empty title",
                    author = author,
                    hospitalName = hospitalName,
                    tableFormat = "html",
                    reshId = reshId)
    )
  })


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
                             registryName = registryName,
                             session = session,
                             reshId = reshId,
                             userRole = userRole)
  })


  metaDat <- shiny::reactive({
    ablanor::getPivotDataSet(setId = input$selectedDataSet,
                             registryName = registryName,
                             session = session,
                             reshId = reshId,
                             userRole = userRole,
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
      if (length(rvals$showPivotTable) == 0 | rvals$showPivotTable) {
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
                             registryName = registryName,
                             session = session,
                             reshId = reshId,
                             userRole = userRole,
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
    if (input$kbdTab %in% c("rand12", "pros_patient")) {
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
  output$dataDumpInfo <- shiny::renderUI({
    shiny::p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file, input$dumpFormat, userRole = userRole, reshId = reshId)
    }
  )

  # Månedlig rapport
  output$maanedligRapport <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("AblaNor_local_monthly.Rmd", package = "ablanor"),
      outputType = "html_fragment",
      params = list(author = author,
                    hospitalName = hospitalName,
                    tableFormat = "html",
                    reshId = reshId,
                    registryName = registryName,
                    userRole = userRole,
                    userOperator = userOperator)
    )
  })

  output$downloadReport <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "AblaNor_local_monthly",
                        fileext = paste0(".", input$formatReport)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("AblaNor_local_monthly.Rmd", package = "ablanor"),
        outputType = input$formatReport,
        params = list(author = author,
                      hospitalName = hospitalName,
                      tableFormat = input$formatReport,
                      reshId = reshId,
                      registryName = registryName,
                      userFullName = userFullName,
                      userRole = userRole,
                      userOperator = userOperator)
      )
      file.rename(fn, file)
    }
  )


  # Values shared among subscriptions and dispatchment
  orgs <- ablanor::getNameReshId(registryName = registryName,
                                 asNamedList = TRUE)

  # Abonnement
  subReports <- list(
    "Månedlige resultater" = list(
      synopsis = "Månedlige resultater sykehus/avdeling",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "orgId", "orgName"),
      paramValues = c("local_monthly", "pdf", "Månedsresultater", reshId,
                      hospitalName)
    )
  )

  rapbase::autoReportServer(
    id = "ablanorSubscription", registryName = registryName,
    type = "subscription", reports = subReports, orgs = orgs
  )

  # Utsendelse
  disReports <- list(
    "Månedlige resultater" = list(
      synopsis = "AblaNor månedlige resultater sykehus/avdeling",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "orgId"),
      paramValues = c("local_monthly", "pdf", "Månedsresultater", 999999)
    )
  )

  org <- rapbase::autoReportOrgServer("ablanorDispatchment", orgs)
  disFormat <- rapbase::autoReportFormatServer("ablanorDispatchment")

  disParamNames <- shiny::reactive(c("orgId", "outputType"))
  disParamValues <- shiny::reactive(c(org$value(), disFormat()))

  rapbase::autoReportServer(
    id = "ablanorDispatchment", registryName = registryName,
    type = "dispatchment", org = org$value, paramNames = disParamNames,
    paramValues = disParamValues, reports = disReports, orgs = orgs,
    eligible = (userRole == "SC")
  )

  # Eksport
  ## brukerkontroller
  rapbase::exportUCServer("ablanorExport", registryName,
                          eligible = (userRole == "SC"))
  ## veileding
  rapbase::exportGuideServer("ablanorExportGuide", registryName)

  # Brukerstatistikk
  rapbase::statsServer("ablanorStats", registryName,
                       eligible = (userRole == "SC"))
}

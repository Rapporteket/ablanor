# shiny app server logic
library(ablanor)

server <- function(input, output, session) {


  rapbase::appLogger(session = session, msg = "Starting AblaNor application")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    registryName <- "ablanor"
    mapOrgId <- ablanor::getNameReshId(registryName)
    reshId <- rapbase::getUserReshId(session)
    hospitalName <- ablanor::getHospitalName(registryName, reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    userOperator <- "Test Operatoresen"
    author <- userFullName
    # userOperator <- ? #@fixme
  } else {
    ### if need be, define your (local) values here

    readRenviron("H:/data/.Renviron")
    reshId <- Sys.getenv("Test_reshId")
    hospitalName <- Sys.getenv("Test_hospitalName")
    userFullName <- "Test Testersen"  # tester rapport per bruker
    userOperator <- Sys.getenv("Test_operator")
    userRole <- "LC"
    registryName <- "test_ablanor_lokalt"
    author <- userFullName

  }


  # Hide tabs when not role 'SC'
  if (userRole != "SC") {
    shiny::hideTab(inputId = "tabs", target = "Datadump")
    shiny::hideTab(inputId = "tabs", target = "Eksport")
  }

  # Hide tabs when role 'SC'
  if (userRole == "SC") {
    shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
  }


  contentDump <- function(file, type) {
    d <- ablanor::getDataDump(registryName, input$dumpDataSet,
                              fromDate = input$dumpDateRange[1],
                              toDate = input$dumpDateRange[2],
                              session = session)
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
      outputType = "html_fragment"
    )
  })


  # Utforsker
  ## Data sets available
  dataSets <- list(`Bruk og valg av data` = "info",
                   `Prosedyre og basisskjema` = "pros_patient",
                   `RAND-12` = "rand12"
  )


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
    getPivotDataSet(setId = input$selectedDataSet,
                    registryName = registryName,
                    session = session,
                    reshId = reshId)
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
    if (length(rvals$showPivotTable) == 0 | rvals$showPivotTable) {
      shiny::h4(paste("Valgt datasett:",
                      names(dataSets)[dataSets == input$selectedDataSet]))
    } else {
      if (input$isSelectAllVars) {
        vars <- names(dat())
      } else {
        vars <- rvals$selectedVars
      }

      shiny::selectInput(inputId = "selectedVars", label = "Velg variabler:",
                         choices = names(dat()), multiple = TRUE,
                         selected = vars)
    }
    # @ note : Har tatt dat() og ikke metaDat som i NORIC
    # selectInput(inputId = "selectedVars", label = "Velg variabler:",
    #        choices = names(dat()), multiple = TRUE,
    #        selected = vars)
    # }
  })




  output$togglePivotSurvey <- shiny::renderUI({
    if (length(input$selectedVars) == 0) {
      NULL
    } else {
      shiny::actionButton(inputId = "togglePivoting",
                          label = rvals$togglePivotingText)
    }
  })



  # @fixme : Variablene blir sortert alfabetisk. Her er et eksempel på hvordan
  # man kan styre rekkefølgen på levels. Også gjøre for måned f.eks?
  output$pivotSurvey <- rpivotTable::renderRpivotTable({
    if (rvals$showPivotTable) {
      rpivotTable::rpivotTable(dat()[input$selectedVars],
                               sorters = "
                         function(attr) {
                    var sortAs = $.pivotUtilities.sortAs;
                    if (attr == \"bmi_category\") { return sortAs([
                         \"Alvorlig undervekt\",
                         \"Moderat undervekt\",
                         \"Normal\",
                         \"Moderat fedme, klasse I\",
                         \"Fedme, klasse II\",
                         \"Fedme, klasse III\",
                         \"Overvekt\"]); }}"
      )
    } else {
      rpivotTable::rpivotTable(data.frame())
    }
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
      contentDump(file, input$dumpFormat)
    }
  )

  # Månedlig rapport
  # If LU-role, get report on own practice
  # If LC-role, get report on hospital practice
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




  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- shiny::reactiveValues(
    subscriptionTab = rapbase::makeAutoReportTab(session,
                                                 type = "subscription",
                                                 mapOrgId = mapOrgId))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = "none",
    rownames = FALSE, options = list(dom = "t")
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- shiny::renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(rv$subscriptionTab) == 0) {
      shiny::p(paste("Ingen aktive abonnement for", fullName))
    } else {
      shiny::tagList(
        shiny::p(paste("Aktive abonnement for",
                       fullName, "som sendes per epost til ",
                       rapbase::getUserEmail(session), ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement

  ### lag liste over mulige valg av rapporter
  output$subscriptionRepList <- shiny::renderUI({
    shiny::selectInput("subscriptionRep", "Rapport:",
                       c("Prosedyrer, månedlig"))
  })

  ### aktiver abonnement, men kun når et aktuelt valg er gjort
  shiny::observeEvent(input$subscribe, {
    package <- "ablanor"
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval
    )
    email <- rapbase::getUserEmail(session)
    if (input$subscriptionRep == "Prosedyrer, månedlig") {
      synopsis <- "ablanor/Rapporteket: prosedyrer, månedlig"
      baseName <- "Ablanor_local_monthly"
    }


    if (nchar(input$subscriptionRep) > 0) {
      fun <- "subscriptionLocalMonthlyReps"
      paramNames <- c("baseName", "reshId", "registryName", "author",
                      "hospitalName", "type")
      paramValues <- c(baseName, reshId, registryName, author, hospitalName,
                       input$subscriptionFileFormat)
      rapbase::createAutoReport(synopsis = synopsis, package = package,
                                fun = fun, paramNames = paramNames,
                                paramValues = paramValues, owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear,
                                interval = interval,
                                intervalName = intervalName)
    }
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  shiny::observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })


  # Utsendelse
  reports <- list(
    Veiledning = list(
      synopsis = "Startside til AblaNor på Rapporteket som testrapport",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title"),
      paramValues = c("veiledning", "html", "En testrapport")
    ),
    "Månedlige resultater" = list(
      synopsis = "Månedlige resultater sykehus/avdeling",
      fun = "reportProcessor",
      paramNames = c("report", "outputType", "title", "orgId"),
      paramValues = c("local_montly", "pdf", "Månedsresultater", 999999)
    )
  )

  orgs <- getNameReshId(registryName = registryName, asNamedList = TRUE)
  org <- rapbase::autoReportOrgServer("ablanorDispatchment", orgs)
  format <- rapbase::autoReportFormatServer("ablanorDispatchment")

  paramNames <- shiny::reactive(c("orgId", "outputType"))
  paramValues <- shiny::reactive(c(org$value(), format()))

  rapbase::autoReportServer(
    id = "ablanorDispatchment", registryName = registryName,
    type = "dispatchment", paramNames = paramNames, paramValues = paramValues,
    reports = reports, orgs = orgs
  )

  # Eksport
  ## brukerkontroller
  rapbase::exportUCServer("ablanorExport", registryName)

  ## veileding
  rapbase::exportGuideServer("ablanorExportGuide", registryName)
}

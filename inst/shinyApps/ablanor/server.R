# shiny app server logic
library(ablanor)

server <- function(input, output, session) {


  rapbase::appLogger(session = session, msg = "Starting AblaNor application")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    registryName <- "AblanorRapporteket"
    mapOrgId <- ablanor::getNameReshId(registryName)
    reshId <- rapbase::getUserReshId(session)
    hospitalName <- ablanor::getHospitalName(registryName, reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    #registryName <- noric::NORICmakeRegistryName("noricStaging", reshId)
    userOperator <- "Test Operatoresen"
    author <- userFullName
    # userOperator <- ? #@fixme
  } else {
    ### if need be, define your (local) values here

    readRenviron("H:/data/.Renviron")
    reshId <- Sys.getenv("Test_reshId")
    hospitalName <- Sys.getenv("Test_hospitalName")
    userFullName <- "Test Testersen"  # tester rapport per bruker
    userOperator<- Sys.getenv("Test_operator")
    userRole <- "LC"
    registryName <- "test_ablanor_lokalt"
    author <- userFullName

  }


  # Hide tabs when not role 'SC'
  if (userRole != "SC") {
    shiny::hideTab(inputId = "tabs", target = "Datadump")
  }

  # Hide tabs when not role 'SC'
  if (userRole == "SC") {
    shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
  }


  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile) {
    # set param needed for report meta processing
    params <- list(author = author,
                   hospitalName = hospitalName,
                   tableFormat = "html",
                   reshId = reshId,
                   registryName = registryName,
                   userRole = userRole,
                   userOperator = userOperator)
    # do all kniting and rendering from temporary directory/file
    sourceFile <- tempfile(fileext = ".Rmd")
    file.copy(from = system.file(srcFile, package="ablanor"),
              to = sourceFile,
              overwrite = TRUE)
    owd <- setwd(dirname(sourceFile))
    on.exit(setwd(owd))
    sourceFile %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c("fragment_only",
                                           "base64_images")) %>%
      shiny::HTML()
  }


  # filename function for re-use
  downloadFilename <- function(fileBaseName, type) {
    paste(paste0(fileBaseName,
                 as.character(as.integer(as.POSIXct(Sys.time())))),
          sep = ".", switch(
            type,
            PDF = "pdf", HTML = "html")
    )
  }



  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type) {
    src <- normalizePath(system.file(srcFile, package = "ablanor"))
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    out <- rmarkdown::render(
      tmpFile,
      output_format = switch(
        type,
        PDF = rmarkdown::pdf_document(),
        HTML = rmarkdown::html_document()),
      params = list(
        tableFormat = switch(
          type,
          PDF = "latex",
          HTML = "html"),
        hospitalName = hospitalName,
        author = author,
        reshId = reshId,
        registryName = registryName,
        userRole = userRole,
        userOperator = userOperator),
      output_dir = tempdir())
    file.rename(out, file)
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
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })




  # Start
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
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
        # vars <- names(metaDat())
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



  # @fixme : Variablene blir sortert alfabetisk. Her er et eksempel på hvordan man kan
  # styre rekkefølgen på levels. Også gjøre for måned f.eks?
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
    htmlRenderRmd("AblaNor_local_monthly.Rmd")
  })

  output$downloadReport <- shiny::downloadHandler(
    filename = function() {
      downloadFilename("AblaNor_local_monthly",
                       input$formatReport)
    },
    content = function(file) {
      contentFile(file, "AblaNor_local_monthly.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatReport)
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
      p(paste("Ingen aktive abonnement for", fullName))
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
    selectInput("subscriptionRep", "Rapport:", c("Prosedyrer, månedlig"))
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


  # Eksport
  ## reaktive funksjoner
  pubkey <- shiny::reactive({
    shiny::req(input$exportPid)
    getUserPubkey(input$exportPid)
  })

  ## observers
  # shiny::observeEvent(input$exportDownload, {
  #   rv$filename <- ablanor::dumpFile("ablanor")
  #   print(rv$filename)
  # })

  ## nedlasting
  output$exportDownload <- shiny::downloadHandler(
    filename = rv$exportFile,
    content = function(file) {
      rv$exportFile <- ablanor::dumpFile("AblanorRapporteket")
      print(rv$exportfile)
      file.copy(rv$exportFile, file)
    }
  )


  ## brukerkontroller
  output$exportPidUI <- shiny::renderUI({
    shiny::selectInput("exportPid", "Velg mottaker:",
                       getRepoContributors("ablanor"))
  })
  output$exportKeyUI <- shiny::renderUI({
    if (length(pubkey()) == 0) {
      shiny::p("No keys found!")
    } else {
      shiny::selectInput("exportKey", "Velg nøkkel:",
                         selectListPubkey(pubkey()))
    }
  })
  output$exportDownloadUI <- shiny::renderUI({
    shiny::req(input$exportKey)
    shiny::downloadButton("exportDownload", "Last ned!")
  })


  ## veileding
  output$exportGuide <- shiny::renderUI({
    rapbase::renderRmd(system.file("exportGuide.Rmd", package = "ablanor"),
                       outputType = "html_fragment")
  })
}

#' Hent merget datasett
#' Hent datasett fra prosedyreskjema,og rand12. Merge
#' sammen, filtrere på forløp der prosedyre er utført. Legge til evt utledete
#' variabler og nye variabler.
#' @param registryName Dersom verdien "test_ablanor_lokalt" leser vi inn
#' lokal RData-fil. Ellers er det SQL spørring
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param tekstVars legge til tekstvariabler hentet fra kodebok for kategoriske
#' variabler TO DO
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#' @export
#'
#' @examples
getRand12Data <- function(registryName,
                          singleRow = FALSE,
                          tekstVars = FALSE) {
  . <- ""

  if (registryName == "test_ablanor_lokalt") {
    # LASTE INN DATA LOKALT
    load(file = Sys.getenv("filbane_ablanor_test"))

  } else {
    ## SQL SPØRRING :

    # SPM ARE : Hva heter databasen til Prosedyrene?  BLIR DETTE RIKTIG ?
    query_procedure <- "
    SELECT *
    FROM  PROSEDYRE (?)
    "
    # SPM ARE : Hva heter databasen til rand12?  BLIR DETTE RIKTIG ?
    query_rand12 <- "
    SELECT *
    FROM  rand12 (?)
    "

    if(singleRow) {
      msg_procedure <- "Query metadata for merged dataset, procedure"
      msg_rand12<- "Query metadata for merged dataset, rand12"
      query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
      query_rand12 <- paste0(query_rand12, "\nLIMIT\n  1;")
    } else {
      msg_procedure <- "Query data for merged dataset, procedure"
      msg_rand12 <- "Query data for merged dataset, rand12"
      query_procedure <- paste0(query_procedure, ";")
      query_rand12 <- paste0(query_rand12, ";")
    }

    #   ARE : Hva er denne koden til? Tilpasse til 2 spørringer ?
    if ("session" %in% names(list(...))) {
      raplog::repLogger(session = list(...)[["session"]], msg = msg)
    }

    d_pros <- rapbase::loadRegData(registryName, query_procedure, dbType)
    d_rand12 <- rapbase::loadRegData(registryName, query_rand12, dbType)
  }




  # MERGE DATASETTENE :
  # NB: I Ablanor skal berre skjema som høyrer til forløp som har resultert i ein
  # prosedyre (eventuelt ein avbroten ein) analyserast. Oppføringar for andre
  # forløp vert filtrerte vekk. Viss ein person for eksempel berre har eit
  # basisskjema men ikkje (enno) eit prosedyreskjema, vil personen også vera
  # filtrert vekk frå basisskjema-datsettet (og forløpsdatasettet,
  # pasientdatasettet og andre datasett).
  # Her brukar me left_join, for å sikre at berre forløpsid der prosedyre
  # finst vert tekne med.

  d_rand12_ut <- d_pros %>% select(mceid, centreid) %>%
    dplyr::left_join(., d_rand12, by = c("mceid", "centreid"))


  d_rand12_ut
}
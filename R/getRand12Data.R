#' Hent merget datasett
#' Hent datasett fra prosedyreskjema,og rand12. Merge
#' sammen, filtrere på forløp der prosedyre er utført. Legge til evt utledete
#' variabler og nye variabler.
#' @param registryName Dersom verdien "test_ablanor_lokalt" leser vi inn
#' lokal RData-fil. Ellers er det SQL spørring
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param reshId Integer organization id
#' @param allData Logical if global data set is to be returned. When FALSE
#' (default) data will be filtered by \code{reshId}

#' @param ... Optional arguments to be passed to the function
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#' @export

getRand12Data <- function(registryName,
                          singleRow = FALSE,
                          reshID = NULL, allData = FALSE, ...) {

  . <- ""

  d <- ablanor::getRand12(registryName, singleRow,
                          reshID = reshID, allData = allData...)
  d_pros <- d$pros
  d_rand12 <- d$rand12



  # MERGE DATASETTENE :
  # NB: I Ablanor skal berre skjema som høyrer til forløp som har resultert i
  # ein
  # prosedyre (eventuelt ein avbroten ein) analyserast. Oppføringar for andre
  # forløp vert filtrerte vekk. Viss ein person for eksempel berre har eit
  # basisskjema men ikkje (enno) eit prosedyreskjema, vil personen også vera
  # filtrert vekk frå basisskjema-datsettet (og forløpsdatasettet,
  # pasientdatasettet og andre datasett).
  # Her brukar me left_join, for å sikre at berre forløpsid der prosedyre
  # finst vert tekne med.

  d_rand12_ut <- d_pros %>%
    dplyr::select(.data$MCEID, .data$CENTREID) %>%
    dplyr::left_join(., d_rand12, by = c("MCEID", "CENTREID"))

  names(d_rand12_ut) <- tolower(names(d_rand12_ut))



  d_rand12_ut
}

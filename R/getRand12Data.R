#' Hent merget datasett
#' Hent datasett fra prosedyreskjema,og rand12. Merge
#' sammen, filtrere på forløp der prosedyre er utført. Legge til evt utledete
#' variabler og nye variabler.
#' @param registryName Dersom verdien "test_ablanor_lokalt" leser vi inn
#' lokal RData-fil. Ellers er det SQL spørring
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param reshId Integer organization id
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by reshId), "SC" has access to national data.

#' @param ... Optional arguments to be passed to the function
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#' @export

getRand12Data <- function(registryName,
                          singleRow = FALSE,
                          reshId = NULL, userRole, ...) {

  . <- ""

  d <- ablanor::getRand12(registryName, singleRow,
                          reshId = reshId, userRole = userRole, ...)
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
    dplyr::left_join(., d_rand12, by = c("MCEID", "CENTREID")) %>%
    dplyr::rename("RAND_COMPLETE" = .data$COMPLETE,
                  "RAND_INCOMPLETE_REASON"= .data$INCOMPLETE_REASON)

  names(d_rand12_ut) <- tolower(names(d_rand12_ut))



  d_rand12_ut %>% dplyr::arrange(.data$mceid)
}

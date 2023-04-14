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



#' Hent merget datasett
#' Hent datasett fra basisskje,a,og rand12. Merge
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
getBaseregData <- function(registryName,
                           singleRow = FALSE,
                           reshId = NULL,
                           userRole, ...) {

  . <- ""

  d <- ablanor::getBasereg(registryName = registryName,
                           singleRow = singleRow,
                           reshId = reshId,
                           userRole = userRole, ...)
  d_basereg <- d$basereg
  d_pros <- d$pros


  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TO TABELLER (status for skjema etc)
  # Vi angir en prefix for å få med variablene fra begge tabellene
  d_basereg %<>%
    dplyr::rename_at(dplyr::vars(.data$USERCOMMENT:.data$CREATEDBY),
                     function(x) {
                       paste0("basereg_", x)
                     })
  d_pros %<>%
    dplyr::select(MCEID, CENTREID)


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

  d_basereg_ut <-dplyr::right_join(d_basereg,
                                   d_pros,
                                   by = c("MCEID", "CENTREID"))

  names(d_basereg_ut) <- tolower(names(d_basereg_ut))
  d_basereg_ut %>% dplyr::arrange(.data$mceid)
}





#' Hent merget datasett
#' Hent datasett fra basisskje,a,og rand12. Merge
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
getFollowupData <- function(registryName,
                            singleRow = FALSE,
                            reshId = NULL,
                            userRole, ...) {

  . <- ""

  d <- ablanor::getFollowup(registryName = registryName,
                            singleRow = singleRow,
                            reshId = reshId,
                            userRole = userRole, ...)
  d_followup <- d$followup
  d_pros <- d$pros
  d_mce <- d$mce

  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TO TABELLER (status for skjema etc)
  # Vi angir en prefix for å få med variablene fra begge tabellene
  # Forberede Followup-data
  followup_data <- d_followup %>%
    dplyr::rename("MCEID_FOLLOWUP" = .data$MCEID) %>%
    dplyr::rename_at(dplyr::vars(.data$USERCOMMENT:.data$CREATEDBY,
                                 .data$COMPLETE, .data$INCOMPLETE_REASON),
                     function(x) {
                       paste0("followup_", x)
                     }) %>%
    # Kobler med alle forløp av type = 9, for å legge til parentmceid.
    # (Parentmceid er forløpet som ligger til grunn for utsending av oppf.)
    dplyr::left_join(.,
                     d_mce %>%
                       dplyr::filter(.data$MCETYPE == 9) %>%
                       dplyr::select(.data$MCEID, .data$PARENTMCEID) %>%
                       dplyr::rename("MCEID_FOLLOWUP" = .data$MCEID,
                                     "MCEID" = .data$PARENTMCEID),
                     by = "MCEID_FOLLOWUP", "CENTREID") %>%
    dplyr::relocate(.data$MCEID, .before = "MCEID_FOLLOWUP")





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


  d_ablanor <- dplyr::left_join(d_pros,
                               followup_data,
                               by = c("MCEID", "CENTREID")) %>%
    dplyr::relocate(.data$followup_STATUS, .before = "followup_COMPLETE")



  names(d_ablanor) <- tolower(names(d_ablanor))

  d_ablanor

}

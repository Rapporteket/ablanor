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
                          reshId = NULL,
                          userRole,
                          fromDate = NULL,
                          toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getRand12(registryName = registryName,
                          singleRow = singleRow,
                          reshId = reshId,
                          userRole = userRole,
                          fromDate = fromDate,
                          toDate = toDate)
  d_rand12 <- d$d_rand12

  names(d_rand12) <- tolower(names(d_rand12))

  d_rand12 %>%
    dplyr::arrange(mceid) %>%
    dplyr::relocate(dato_pros, .after = "centreid") %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    ablanor::utlede_tidsvariabler(.) %>%
    dplyr::mutate(
      aar_rand12 = as.ordered(lubridate::year(.data$dato_rand12)),
      maaned_nr_rand12 = as.ordered(sprintf(fmt = "%02d",
                                            lubridate::month(.data$dato_rand12))),
      maaned_rand12 = ifelse(
        test = is.na(.data$aar_rand12) | is.na(.data$maaned_nr_rand12),
        yes = NA,
        no = paste0(.data$aar_rand12, "-", .data$maaned_nr_rand12)))



}



#' Hent berabeidet datasett
#'
#' Inneholder kun basisskjema for forløp der prosedyre er utført (har dato).
#' Legge til evt utledete variabler her.
#' @param registryName "ablanor"
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param reshId Integer organization id
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by reshId), "SC" has access to national data.
#' @param fromDate first date (dato_pros)
#' @param toDate laste date (dato_pros)
#' @param ... Optional arguments to be passed to the function
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#' @export
getBaseregData <- function(registryName,
                           singleRow = FALSE,
                           reshId = NULL,
                           userRole,
                           fromDate = NULL,
                           toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getBasereg(registryName = registryName,
                           singleRow = singleRow,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = fromDate,
                           toDate = toDate)
  d_basereg <- d$d_basereg

  names(d_basereg) <- tolower(names(d_basereg))

  d_basereg %>%
    dplyr::arrange(mceid) %>%
    dplyr::relocate(dato_pros, .after = "centreid") %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    ablanor::utlede_tidsvariabler(.)
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

  d_ablanor %>%
    dplyr::mutate(

      # Tidsvariabler for prosedyre
      aar_prosedyre = as.ordered(lubridate::year(.data$dato_pros)),
      maaned_nr_prosedyre = as.ordered(sprintf(fmt = "%02d",
                                               lubridate::month(.data$dato_pros))),
      maaned_prosedyre = ifelse(test = is.na(.data$aar_prosedyre) | is.na(.data$maaned_nr_prosedyre),
                                yes = NA,
                                no = paste0(.data$aar_prosedyre, "-", .data$maaned_nr_prosedyre)),

      # Tidsvariabler for oppfolging
      aar_followup = as.ordered(lubridate::year(.data$dato_followup)),
      maaned_nr_followup = as.ordered(sprintf(fmt = "%02d",
                                              lubridate::month(.data$dato_followup))),
      maaned_followup = ifelse(test = is.na(.data$aar_followup) | is.na(.data$maaned_nr_followup),
                               yes = NA,
                               no = paste0(.data$aar_followup, "-", .data$maaned_nr_followup)))

}

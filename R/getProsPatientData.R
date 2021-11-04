#' Hent merget datasett
#' Hent datasett fra basis-skjema, prosedyreskjema, mce, og pasientskjema. Merge
#' sammen, filtrere på forløp der prosedyre er utført. Legge til utledete
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

getProsPatientData <- function(registryName,
                               singleRow = FALSE,
                               reshID = NULL, allData = FALSE, ...) {

  . <- ""

  d <- ablanor::getProsPatient(registryName, singleRow, reshID = reshID,
                               allData = allData, ...)

  d_basereg <- d$basereg
  d_pros <- d$pros
  d_mce <- d$mce
  d_patientlist <- d$patientlist
  d_followup <- d$followup

  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TO TABELLER (status for skjema etc)
  # intersect(names(d_pros), names(d_basereg)) # samme variabel-navn.
  # Vi angir en prefix for å få med variablene fra begge tabellene
  d_basereg %<>%
    dplyr::rename_at(dplyr::vars(.data$USERCOMMENT:.data$CREATEDBY),
                     function(x) {
                       paste0("basereg_", x)
                     })
  d_pros %<>%
    dplyr::rename_at(dplyr::vars(.data$USERCOMMENT:.data$CREATEDBY),
                     function(x) {
                       paste0("pros_", x)
                     })

  d_mce %<>%
    dplyr::rename_at(dplyr::vars(.data$STATUS),
                     function(x) {
                       paste0("mce_", x)
                     })


  # MERGE DATASETTENE :
  # NB: I Ablanor skal berre skjema som høyrer til forløp som har resultert i
  # ein prosedyre (eventuelt ein avbroten ein) analyserast. Oppføringar for
  # andre forløp vert filtrerte vekk. Viss ein person for eksempel berre har
  # eit basisskjema men ikkje (enno) eit prosedyreskjema, vil personen også
  # vera filtrert vekk frå basisskjema-datsettet (og forløpsdatasettet,
  # pasientdatasettet og andre datasett).
  # Her brukar me left_join, for å sikre at berre forløpsid der prosedyre
  # finst vert tekne med.

  d_ablanor <- d_pros %>%
    dplyr::left_join(.,
                     d_mce %>% dplyr::select(.data$MCEID,
                                             .data$PATIENT_ID,
                                             .data$mce_STATUS),
                     by = "MCEID") %>%
    dplyr::left_join(.,
                     d_patientlist,
                     by = c("PATIENT_ID" = "ID")) %>%
    dplyr::left_join(.,
                     d_basereg,
                     by = c("MCEID", "CENTREID"))

  # Sjekk at ingen variabel-navn teller dobbelt.
  # TEST I getLocalProsedyrePasientData
  # d_ablanor %>% dplyr::select(ends_with(".x") | ends_with(".y"))


  # Forberede Followup-data
  followup_data <- d_followup %>%
    dplyr::rename("MCEID_FOLLOWUP" = .data$MCEID) %>%
    dplyr::rename_at(dplyr::vars(.data$USERCOMMENT:.data$CREATEDBY),
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
                     by = "MCEID_FOLLOWUP") %>%
    dplyr::relocate(.data$MCEID, .before = "MCEID_FOLLOWUP")

  # Sjekk at bare en oppfølging per forløp
  # (I starten ble flere skjema sendt ut da er det nyeste skjema som gjelder)
  followup_data %<>%
    dplyr::group_by(.data$MCEID) %>%
    dplyr::mutate(max_mceid_followup = max(.data$MCEID_FOLLOWUP)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$MCEID_FOLLOWUP == .data$max_mceid_followup) %>%
    dplyr::select(- .data$max_mceid_followup)


  # Legg til follow-up i pasient - prosedyre - data
  d_ablanor %<>%
    dplyr::left_join(.,
                     followup_data,
                     by = c("MCEID", "CENTREID"))


  names(d_ablanor) <- tolower(names(d_ablanor))


  # UTLEDETE VARIABLER

  # ALDER :
  d_ablanor %<>%
    ablanor::utlede_alder(.) %>%
    ablanor::utlede_alder_75(.) %>%
    ablanor::utlede_aldersklasse(.)

  # BMI klasse
  d_ablanor %<>%
    ablanor::utlede_bmi_klasse(.)



  # UKE, MÅNED, ÅR
  d_ablanor %<>% ablanor::utlede_tidsvariabler(.)


  # AFLI : ICD
  d_ablanor %<>% ablanor::utlede_kateg_afli_aryt_i48(.)


  # VT : KARDIOMYOPATI
  d_ablanor %<>% ablanor::utlede_kardiomyopati(.)


  # HJERTESVIKT OG REDUSERT EF
  d_ablanor %<>% ablanor::utlede_hjertesvikt_redusert_ef(.)



  d_ablanor
}

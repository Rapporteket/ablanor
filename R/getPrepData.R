#' Data managment on tables
#'
#' Load data and apply data-management operations. Tables can be used in
#' \emph{pivot-tables} or monthly reports. Notice, only tables when a
#' procedure-date exists!
#'
#'


#' @param registryName "ablanor"
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param reshId Integer organization id. From login settings.
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by reshId), "SC" has access to national data.
#' @param fromDate NULL default is 01-01-1900. If datadump or pivot table,
#' start date of calendar is used.
#' @param toDate NULL default is newest registration in Abalnor. If datadump or
#'  pivot table, end date of calendar is used.
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#'
#' @name getPrepDataAblanor
#' @aliases getBaseregData
#' getProsData
#' getMceData
#' getRand12Data
#' getFollowupBasisData
#' getFollowupOneYrData
#' getGkvData
#' getPromsData
#' getBaseregProsData
#' getBaseregProsHendelseData
NULL

#' @rdname getPrepDataAblanor
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
    ablanor::utlede_tidsvariabler(.) %>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby),
                     function(x) {
                       paste0("basereg_", x)
                     })

}

#' @rdname getPrepDataAblanor
#' @export
getProsData <- function(registryName,
                        singleRow = FALSE,
                        reshId = NULL,
                        userRole,
                        fromDate = NULL,
                        toDate = NULL, ...) {
  . <- ""

  d <- ablanor::getPros(registryName = registryName,
                        singleRow = singleRow,
                        reshId = reshId,
                        userRole = userRole,
                        fromDate = fromDate,
                        toDate = toDate)
  d_pros <- d$d_pros

  names(d_pros) <- tolower(names(d_pros))

  d_pros %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    ablanor::utlede_tidsvariabler(.) %>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby),
                     function(x) {
                       paste0("pros_", x)
                     })

}

#' @rdname getPrepDataAblanor
#' @export
getMceData <- function(registryName,
                       singleRow = FALSE,
                       reshId = NULL,
                       userRole,
                       fromDate = NULL,
                       toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getMce(registryName = registryName,
                       singleRow = singleRow,
                       reshId = reshId,
                       userRole = userRole,
                       fromDate = fromDate,
                       toDate = toDate)
  d_mce <- d$d_mce

  names(d_mce) <- tolower(names(d_mce))

  d_mce %>%
    dplyr::arrange(mceid) %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    dplyr::mutate(
      aar_mce = as.ordered(lubridate::year(.data$tscreated)),
      maaned_nr_mce = as.ordered(sprintf(fmt = "%02d",
                                         lubridate::month(.data$tscreated))),
      maaned_mce = ifelse(
        test = is.na(.data$aar_mce) | is.na(.data$maaned_nr_mce),
        yes = NA,
        no = paste0(.data$aar_mce, "-", .data$maaned_nr_mce))) %>%
    dplyr::rename_at(dplyr::vars(.data$status:.data$updatedby),
                     function(x) {
                       paste0("mce_", x)
                     })

}



#' @rdname getPrepDataAblanor
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

  # TO DO:
  # Legg til mcetype og prosedyredato til parentmceid for rand12
  # basis, 1år, 5år
  d_rand12 %>%
    dplyr::arrange(mceid) %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    dplyr::mutate(
      aar_rand12 = as.ordered(lubridate::year(.data$dato_rand12)),
      maaned_nr_rand12 = as.ordered(sprintf(fmt = "%02d",
                                            lubridate::month(.data$dato_rand12))),
      maaned_rand12 = ifelse(
        test = is.na(.data$aar_rand12) | is.na(.data$maaned_nr_rand12),
        yes = NA,
        no = paste0(.data$aar_rand12, "-", .data$maaned_nr_rand12))) %>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby,
                                 complete,
                                 incomplete_reason),
                     function(x) {
                       paste0("rand_", x)
                     })

}





#' @rdname getPrepDataAblanor
#' @export
getFollowupBasisData <- function(registryName,
                                 singleRow = FALSE,
                                 reshId = NULL,
                                 userRole, ...) {

  . <- ""

  d <- ablanor::getFollowupBasis(registryName = registryName,
                                 singleRow = singleRow,
                                 reshId = reshId,
                                 userRole = userRole, ...)
  d_followupBasis <- d$d_followupBasis


  names(d_followupBasis) <- tolower(names(d_followupBasis))

  d_followupBasis %>%
    dplyr::mutate(


      # Tidsvariabler for oppfolging
      aar_followup = as.ordered(lubridate::year(.data$dato_followup)),
      maaned_nr_followup = as.ordered(sprintf(fmt = "%02d",
                                              lubridate::month(.data$dato_followup))),
      maaned_followup = ifelse(test = is.na(.data$aar_followup) | is.na(.data$maaned_nr_followup),
                               yes = NA,
                               no = paste0(.data$aar_followup, "-", .data$maaned_nr_followup)))

}


#' @rdname getPrepDataAblanor
#' @export
getFollowupOneYrData <- function(registryName,
                                 singleRow = FALSE,
                                 reshId = NULL,
                                 userRole, ...) {

  . <- ""

  d <- ablanor::getFollowupOneYr(registryName = registryName,
                                 singleRow = singleRow,
                                 reshId = reshId,
                                 userRole = userRole, ...)
  d_followup <- d$d_followup1


  names(d_followup) <- tolower(names(d_followup))

  d_followup %>%
    dplyr::mutate(


      # Tidsvariabler for oppfolging
      aar_followup = as.ordered(lubridate::year(.data$dato_followup)),
      maaned_nr_followup = as.ordered(sprintf(fmt = "%02d",
                                              lubridate::month(.data$dato_followup))),
      maaned_followup = ifelse(test = is.na(.data$aar_followup) | is.na(.data$maaned_nr_followup),
                               yes = NA,
                               no = paste0(.data$aar_followup, "-", .data$maaned_nr_followup)))

}


#' @rdname getPrepDataAblanor
#' @export
getGkvData <- function(registryName,
                       singleRow = FALSE,
                       reshId = NULL,
                       userRole,
                       fromDate = NULL,
                       toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getGkv(registryName = registryName,
                       singleRow = singleRow,
                       reshId = reshId,
                       userRole = userRole,
                       fromDate = fromDate,
                       toDate = toDate, ...)
  d_gkv <- d$d_gkv

  names(d_gkv) <- tolower(names(d_gkv))

  # TO DO:
  # Legg til mcetype og prosedyredato til parentmceid for GKV???
  d_gkv %>%
    dplyr::arrange(mceid) %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    dplyr::mutate(
      aar_gkv = as.ordered(lubridate::year(.data$dato_gkv)),
      maaned_nr_gkv = as.ordered(sprintf(fmt = "%02d",
                                         lubridate::month(.data$dato_gkv))),
      maaned_gkv = ifelse(
        test = is.na(.data$aar_gkv) | is.na(.data$maaned_nr_gkv),
        yes = NA,
        no = paste0(.data$aar_gkv, "-", .data$maaned_nr_gkv))) %>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby,
                                 complete,
                                 incomplete_reason),
                     function(x) {
                       paste0("gkv_", x)
                     })

}

#' @rdname getPrepDataAblanor
#' @export
getPromsData <- function(registryName,
                         singleRow = FALSE,
                         reshId = NULL,
                         userRole,
                         fromDate = NULL,
                         toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getProms(registryName = registryName,
                         singleRow = singleRow,
                         reshId = reshId,
                         userRole = userRole,
                         fromDate = fromDate,
                         toDate = toDate, ...)
  d_proms <- d$d_proms

  names(d_proms) <- tolower(names(d_proms))

  # TO DO:
  # Legg til mcetype og prosedyredato til parentmceid for GKV???
  d_proms %>%
    dplyr::arrange(mceid) %>%
    ablanor::legg_til_sykehusnavn(., short = FALSE) %>%
    dplyr::mutate(
      aar_tssendt = as.ordered(lubridate::year(tssendt)),
      maaned_nr_tssendt = as.ordered(sprintf(fmt = "%02d",
                                         lubridate::month(tssendt))),
      maaned_tssendt = ifelse(
        test = is.na(aar_tssendt) | is.na(maaned_nr_tssendt),
        yes = NA,
        no = paste0(aar_tssendt, "-", maaned_nr_tssendt))) %>%
    dplyr::rename_at(dplyr::vars(status:tsupdated),
                     function(x) {
                       paste0("proms_", x)
                     })

}


#' @rdname getPrepDataAblanor
#' @export
getBaseregProsData <- function(registryName,
                               singleRow = FALSE,
                               reshId = NULL,
                               userRole,
                               fromDate = NULL,
                               toDate = NULL, ...){

  . <- ""

  d <- ablanor::getBaseregPros(registryName = registryName,
                               singleRow = singleRow,
                               reshId = reshId,
                               userRole = userRole,
                               fromDate = fromDate,
                               toDate = toDate)
  d_pros <- d$pros
  d_basereg <- d$basereg
  d_mce <- d$mce
  d_patientlist <- d$patientlist
  d_mcepatientdata <- d$mcepatientdata




  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TRE TABELLER (status for skjema etc)
  # Vi angir en prefix for å få med variablene fra alle tabellene
  # Slik finner Kodeboken alle variablene

  d_pros %<>%
    dplyr::select(- TSUPDATED,
                  - UPDATEDBY,
                  - FIRST_TIME_CLOSED,
                  - FIRST_TIME_CLOSED_BY,
                  - TSCREATED,
                  - CREATEDBY) %>%
    dplyr::rename("PROS_STATUS" = "STATUS",
                  "PROS_USERCOMMENT" = "USERCOMMENT")

  d_basereg %<>%
    dplyr::select(- TSUPDATED,
                  - UPDATEDBY,
                  - FIRST_TIME_CLOSED,
                  - FIRST_TIME_CLOSED_BY,
                  - TSCREATED,
                  - CREATEDBY,
                  - DATO_PROS) %>%
    dplyr::rename("BASEREG_STATUS" = "STATUS",
                  "BASEREG_USERCOMMENT" = "USERCOMMENT")



  # MERGE DATASETTENE :
  # NB: I Ablanor skal berre skjema som høyrer til forløp som har resultert i
  # ein prosedyre (eventuelt ein avbroten ein) analyserast.
  # Oppføringar for andre forløp vert filtrerte vekk.
  # Viss ein person for eksempel berre har eit
  # basisskjema men ikkje (enno) eit prosedyreskjema, vil personen også vera
  # filtrert vekk frå basisskjema-datsettet.


  # REKKEFØLGE FILER: BASISDATA + PASIENT-DATA FØRST, PROSEDYRE ETTERPÅ
  d_ablanor <-  dplyr::right_join(d_basereg,
                                  d_pros,
                                  by = c("MCEID", "CENTREID")) %>%
    # Legg til pasient_id til venstre
    dplyr::right_join(d_mce %>% dplyr::select(MCEID,
                                              PATIENT_ID),
                      .,
                      by = "MCEID") %>%
    # Legg til kommunmenummer til venstre
    # Kommunenummer på forløpstidspunktet.
    dplyr::right_join(d_mcepatientdata %>% dplyr::select(-PID),
                      .,
                      by = "MCEID") %>%
    # Legg til pasientinformasjon til venstre
    # Lik for alle pasientens forløp,
    # Men repetert for hvert sykehus!
    dplyr::right_join(d_patientlist %>%
                        dplyr::rename("PATIENT_ID" = "ID") %>%
                        dplyr::select(-CENTREID) %>%
                        dplyr::distinct(),
                      .,
                      by = "PATIENT_ID",
                      multiple = "all") %>%
    dplyr::relocate(c("MCEID", "CENTREID"), .before = "PATIENT_ID")


  names(d_ablanor) <- tolower(names(d_ablanor))



  # UTLEDETE VARIABLER

  # ALDER :
  d_ablanor %<>%
    ablanor::utlede_alder(.) %>%
    ablanor::utlede_alder_75(.) %>%
    ablanor::utlede_aldersklasse(.)

  # BMI klasse
  # NB: BMI i datadumpen er litt feil! bruke denne (bmi_manual)
  d_ablanor %<>%
    ablanor::utlede_bmi(.) %>%
    ablanor::utlede_bmi_klasse(.)



  # AFLI : ICD
  d_ablanor %<>% ablanor::utlede_kateg_afli_aryt_i48(.)


  # VT : KARDIOMYOPATI
  d_ablanor %<>% ablanor::utlede_kardiomyopati(.)


  # HJERTESVIKT OG REDUSERT EF
  d_ablanor %<>% ablanor::utlede_hjertesvikt_redusert_ef(.)


  # Indikator tamponade, indikator for avbrudd
  d_ablanor %<>%
    ablanor::indik_tamponade(.) %>%
    ablanor::indik_ferdig_komplik(.) %>%
    ablanor::indik_akuttsuksess(.) %>%
    ablanor::indik_pacemaker(.) %>%
    ablanor::indik_avbrudd(.)

  d_ablanor %>%
    dplyr::mutate(

      # Tidsvariabler for prosedyre
      aar_prosedyre = as.ordered(lubridate::year(.data$dato_pros)),
      maaned_nr_prosedyre = as.ordered(sprintf(fmt = "%02d",
                                               lubridate::month(.data$dato_pros))),
      maaned_prosedyre = ifelse(test = is.na(.data$aar_prosedyre) | is.na(.data$maaned_nr_prosedyre),
                                yes = NA,
                                no = paste0(.data$aar_prosedyre, "-", .data$maaned_nr_prosedyre))) %>%
    dplyr::arrange(.data$mceid)
}





#' @rdname getPrepDataAblanor
#' @export
getBaseregProsHendelseData <- function(registryName,
                               singleRow = FALSE,
                               reshId = NULL,
                               userRole,
                               fromDate = NULL,
                               toDate = NULL, ...){

  d_hendelse <- getHendelse(registryName = registryName,
                            singleRow = singleRow,
                            reshId = reshId,
                            userRole = userRole,
                            fromDate = fromDate,
                            toDate = toDate)$d_hendelse

  d_mce <- getMce(registryName = registryName,
                            singleRow = singleRow,
                            reshId = reshId,
                            userRole = userRole,
                            fromDate = fromDate,
                            toDate = toDate)$d_mce

  d_pros <- getPros(registryName = registryName,
                            singleRow = singleRow,
                            reshId = reshId,
                            userRole = userRole,
                            fromDate = fromDate,
                            toDate = toDate)$d_pros


  d_basereg <- getBasereg(registryName = registryName,
                    singleRow = singleRow,
                    reshId = reshId,
                    userRole = userRole,
                    fromDate = fromDate,
                    toDate = toDate)$d_basereg




  d_hendelse %<>%
    dplyr::rename_at(dplyr::vars(KOMP_JANEI:STATUS),
                     function(x) {
                       paste0("adhoc_", x)
                     }) %>%
    dplyr::rename("MCEID_adhoc" = "MCEID") %>%
    dplyr::select(MCEID_adhoc,
                  CENTREID,
                  DATO_ADHOC,
                  adhoc_KOMP_JANEI:adhoc_STATUS)

  d_pros %<>%
    dplyr::select(MCEID, CENTREID, FORLOPSTYPE, DATO_PROS)

  d_basereg %<>%
    dplyr::select(MCEID, CENTREID, HOYDE, VEKT)


  d_mce %<>%
    dplyr::select(MCEID, CENTREID, MCETYPE, PARENTMCEID)

  names(d_hendelse) <- tolower(names(d_hendelse))
  names(d_mce) <- tolower(names(d_mce))
  names(d_pros) <- tolower(names(d_pros))
  names(d_basereg) <- tolower(names(d_basereg))




  d_hendelse %<>% dplyr::left_join(.,
                                   d_mce %>% dplyr::filter(mcetype == 8) %>%
                                     dplyr::select(mceid, parentmceid) %>%
                                     dplyr::rename(mceid_adhoc = mceid,
                                                   mceid = parentmceid),
                                   by = "mceid_adhoc")


  d_ut <- dplyr::right_join(
    x = dplyr::left_join(d_basereg, d_pros, by = c("mceid", "centreid")),
    y = d_hendelse,
    by = c("mceid", "centreid")) %>%

    # Antall dager fra prosedyre til hendelse
    dplyr::mutate(dager_pros_hendelse = as.numeric(difftime(
      dato_adhoc,
      dato_pros,
      units = "days"
    )))

  d_ut
}
#' @rdname getPrepDataAblanor
#' @export
getBaseregProsFollowup1Data <- function(registryName,
                                        singleRow = FALSE,
                                        reshId = NULL,
                                        userRole,
                                        fromDate = NULL,
                                        toDate = NULL, ...){

  . <- ""

  d <- ablanor::getBaseregProsFollowup1(registryName = registryName,
                                        singleRow = singleRow,
                                        reshId = reshId,
                                        userRole = userRole,
                                        fromDate = fromDate,
                                        toDate = toDate)
  d_baseregPat <- d$d_baseregPat
  d_followup <- d$d_followup
  d_proms <- d$d_proms



  d_followup %<>%
    dplyr::rename("FOLLOWUP_STATUS" = "STATUS",
                  "FOLLOWUP_TSCREATED" = "TSCREATED",
                  "MCEID_FOLLOWUP" = "MCEID",
                  "MCEID" = "PARENTMCEID") %>%
    dplyr::mutate(eprom_opprettet = "ja") %>%
    dplyr::select(-MCETYPE)


  d_proms %<>%
    dplyr::rename("PROMS_STATUS" = "STATUS",
                  "MCEID_FOLLOWUP" = "MCEID",
                  "PROMS_TSSENDT" = "TSSENDT",
                  "PROMS_EXPIRY_DATE" = "EXPIRY_DATE") %>%
    dplyr::mutate(eprom_sendt = "ja")

  names(d_followup) <- tolower(names(d_followup))
  names(d_proms) <- tolower(names(d_proms))
  names(d_baseregPat) <- tolower(names(d_baseregPat))



  # Sjekk at bare en oppfølging per forløp
  # (I starten ble flere skjema sendt ut da er det nyeste skjema som gjelder)
  followup_data <- d_followup %>%
    dplyr::filter(!is.na(followup_status)) %>%
    dplyr::left_join(.,
                     d_proms,
                     by = "mceid_followup")


  # Legg til follow-up i pasient - prosedyre - data
  d_ablanor <- d_baseregPat %>%
    dplyr::left_join(.,
                     followup_data,
                     by = c("mceid", "centreid", "patient_id"))

  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    d_ablanor %>%
      dplyr::filter(!is.na(followup_status)) %>%
      dplyr::pull(dato_pros)))





  d_ablanor %<>%
    dplyr::mutate(

      # I Versjon 1.5 ble opprettelse/bestilling av eproms skilt.
      # I ny versjon så sjekkes alle kriterier før opprettelse av e-prom
      versjon_1_5_eller_mer = ifelse(
        test = (dato_pros >= as.Date("2022-11-22", format = "%Y-%m-%d")),
        yes = "ja",
        no = "nei"),

      # 50 uker etter prosedyredato blir e-proms  opprettet
      dato_followup_teoretisk = dato_pros + lubridate::days(351)) %>%

    # KRITERIER FOR UTSENDING
    # KRITERIE 1. Alder. Under 16 på prosedyretidspunktet.
    ablanor::utlede_alder() %>%
    dplyr::mutate(kriterie_alder = ifelse(test = alder >= 16,
                                          yes = "ja",
                                          no = "nei")) %>%

    # KRITERIE 2. Norsk fødselsnummer
    dplyr::mutate(kriterie_norsk = ifelse(
      test = (ssn_type %in% 1 & ssnsubtype %in% c(1, 3)),
      yes = "ja",
      no = "nei")) %>%

    # KRITERIE 3. Levende 50 uker etter prosedyren
    dplyr::mutate(kriterie_levende = ifelse(
      test = (deceased %in% 0 |
                (deceased %in% 1 & deceased_date > dato_followup_teoretisk )),
      yes = "ja",
      no = "nei"))

    # KRITERIE 4: Ingen ny prosedyre av samme type
    d_ablanor %<>%
    dplyr::arrange(dato_pros) %>%
    dplyr::group_by(patient_id, forlopstype) %>%
    dplyr::mutate(
      antall_pros = dplyr::n(),
      dg_til_neste = as.numeric(difftime(dplyr::lead(dato_pros),
                                         dato_pros,
                                         units = "days"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      kriterie_nyeste = ifelse(
        test= (is.na(dg_til_neste) | dg_til_neste > 351),
        yes = "ja",
        no = "nei"),

      # KRITERIE ALLE
      kriterie_alle = ifelse(
        test = (kriterie_nyeste %in% "ja" &
                  kriterie_alder %in% "ja" &
                  kriterie_levende %in% "ja" &
                  kriterie_norsk %in% "ja"),
        yes = "ja",
        no = "nei"))











  d_ablanor %>%
    dplyr::mutate(

      # Tidsvariabler for prosedyre
      aar_prosedyre = as.ordered(lubridate::year(dato_pros)),
      maaned_nr_prosedyre = as.ordered(sprintf(fmt = "%02d",
                                               lubridate::month(dato_pros))),
      maaned_prosedyre = ifelse(
        test = (is.na(aar_prosedyre) | is.na(maaned_nr_prosedyre)),
        yes = NA,
        no = paste0(aar_prosedyre, "-", maaned_nr_prosedyre)),

      # Tidsvariabler for besvart followup
      aar_followup = as.ordered(lubridate::year(dato_followup)),
      maaned_nr_followup = as.ordered(sprintf(fmt = "%02d",
                                              lubridate::month(dato_followup))),
      maaned_followup = ifelse(
        test = is.na(aar_followup) | is.na(maaned_nr_followup),
        yes = NA,
        no = paste0(aar_followup, "-", maaned_nr_followup)),




      # Tidsvariabler for opprettet followup
      aar_followup_tscreated = as.ordered(lubridate::year(followup_tscreated)),
      maaned_nr_followup_tscreated = as.ordered(sprintf(fmt = "%02d",
                                              lubridate::month(followup_tscreated))),
      maaned_followup = ifelse(
        test = is.na(aar_followup_tscreated) | is.na(maaned_nr_followup_tscreated),
        yes = NA,
        no = paste0(aar_followup_tscreated, "-", maaned_nr_followup_tscreated)),



      # Tidsvariabler for bestilt followup
      aar_proms_tssendt = as.ordered(lubridate::year(proms_tssendt)),
      maaned_nr_proms_tssendt = as.ordered(sprintf(fmt = "%02d",
                                                        lubridate::month(proms_tssendt))),
      maaned_followup = ifelse(
        test = is.na(aar_proms_tssendt) | is.na(maaned_nr_proms_tssendt),
        yes = NA,
        no = paste0(aar_proms_tssendt, "-", maaned_nr_proms_tssendt)),



      dg_pros_opprettet = as.numeric(difftime(
        followup_tscreated,
        dato_pros,
        units = "days"
      ))
      ) %>%
    dplyr::arrange(mceid) %>%

    dplyr::mutate(
      eprom_kjente_feil = dplyr::case_when(

        dato_pros == as.Date("2021-09-01", format = "%Y-%m-%d") ~
        "teknisk problem",

        (dato_pros >= as.Date("2020-01-01", format = "%Y-%m-%d") &
           dato_pros <= as.Date("2020-01-24", format = "%Y-%m-%d")) ~
          "teknisk problem",

        (dato_pros >= as.Date("2022-11-22", format = "%Y-%m-%d") &
           dato_pros <= as.Date("2022-11-25", format = "%Y-%m-%d") &
           eprom_opprettet %in% "ja" &
           kriterie_alle %in% "ja" &
           is.na(eprom_sendt))~
          "teknisk problem",



      TRUE ~ "nei"),

      eprom_datagrunnlag = factor(
        x = dplyr::case_when(

          #  ALT FOR NYE REGISTRERINGER
          dato_pros > nyeste_eprom_bestilling ~
          "nei, registreringen er for ny",

          # ALT FOR GAMLE REGISTRERINGER
         dato_pros < as.Date("2020-01-01", format = "%Y-%m-%d") ~
           "nei, før innføring av 1års oppf.",

         dato_pros  == as.Date("2020-01-01", format = "%Y-%m-%d") &
           is.na(eprom_opprettet) ~
           "nei, før innføring av 1års oppf.",

         # EPROMS OPPRETTET OG SATT TIL AVDØD MED EN GANG
         (has_followup %in% 1 &
            eprom_opprettet %in% "ja" &
            incomplete_reason %in% 3) ~
           "nei, opprettet satt til død",

         # EPROMS SENDT UT UTEN AT ALLE KRITERIER VAR OPPFYLT
         (has_followup %in% 1 &
            eprom_opprettet %in% "ja" &
            kriterie_alle %in% "nei" &
            eprom_sendt %in% "ja" &
            eprom_kjente_feil %in% "nei") ~
           "nei, eprom feilaktig sendt, sjekk kriterier",


         # NY VERSJON: KONTROLL KRITIER FØR OPPRETTELSE
         (has_followup %in% 1 &
            versjon_1_5_eller_mer %in% "ja" &
            is.na(eprom_opprettet)) ~
           "nei, ikke opprettet etter sjekk kriterier",

         # NY VERSJON: OPPRETTELES EN DAG, OG BESTILLING INNEN 30 DAGER ETTER
         # "BESTILT I DAG, SENDES I MORGEN"
         (has_followup %in% 1 &
            eprom_opprettet %in% "ja" &
            kriterie_alle %in% "ja" &
            eprom_kjente_feil %in% "nei" &
            is.na(eprom_sendt)) ~
           "nei, eprom venter på utsendelse",


         # GAMMEL VERSJON: EPROM  OPPRETTET FOR ALLE, MEN
         # KONTROLL KRITERIER FØR UTSENDING
         (has_followup %in% 1 &
            versjon_1_5_eller_mer %in% "nei" &
            eprom_opprettet %in% "ja" &
            is.na(eprom_sendt) &
            (kriterie_levende %in% "nei" |
               kriterie_norsk %in% "nei" |
               kriterie_alder %in% "nei") &
            !incomplete_reason %in% 3 &
            eprom_kjente_feil %in% "nei") ~
           "nei, opprettet men ikke sendt etter sjekk kriterier",


         # NY VERJSON: OPPRETTET, MEN IKKE SENDT SKYLES TEKNISKE PROBLEM
         # RETT ETTER RELEASE
         (has_followup %in% 1 &
            versjon_1_5_eller_mer %in% "ja" &
            eprom_opprettet %in% "ja" &
            is.na(eprom_sendt) &
            !eprom_kjente_feil %in% "nei") ~
           "nei, opprettet men teknisk feil ved bestilling",

         # GAMMEL VERSJON: OPPRETTET, TEKNISK PROBLEM VED UTSENDELSE I 2021/22,
         # MED ELLER UTEN FEILAKTIG UTSENDING I 2023
         (has_followup %in% 1 &
            versjon_1_5_eller_mer %in% "nei" &
            eprom_opprettet %in% "ja" &
            !eprom_kjente_feil %in% "nei") ~
           "nei, teknisk, mangler utsending eller feilaktig sendt i 2023",


         # DISSE ER MED I DATAGRUNNLAGET!
         (has_followup %in% 1 &
            eprom_opprettet %in% "ja" &
            kriterie_alle %in% "ja" &
            eprom_kjente_feil %in% "nei") ~ "ja"),


        levels = c("ja",
                   "nei, registreringen er for ny",
                   "nei, før innføring av 1års oppf.",
                   "nei, opprettet satt til død",
                   "nei, eprom feilaktig sendt, sjekk kriterier",
                   "nei, ikke opprettet etter sjekk kriterier",
                   "nei, eprom venter på utsendelse",
                   "nei, opprettet men ikke sendt etter sjekk kriterier",
                   "nei, opprettet men teknisk feil ved bestilling",
                   "nei, teknisk, mangler utsending eller feilaktig sendt i 2023"),
        ordered  = TRUE),


      eprom_status_txt = dplyr::case_when(

        eprom_datagrunnlag %in% "ja" &
          proms_status %in% 1 ~ "avventer svar",

        eprom_datagrunnlag %in% "ja" &
          proms_status %in% 2 ~ "utgaatt uten svar",

         eprom_datagrunnlag %in% "ja" &
          proms_status %in% 3 ~ "svar mottatt",

        eprom_datagrunnlag %in% "ja" &
          proms_status %in% 4 ~ "digitalt inaktiv",

        eprom_datagrunnlag %in% "ja" &
          is.na(eprom_sendt) ~ "sjekk utsending",

        TRUE ~ NA_character_

      ))






}




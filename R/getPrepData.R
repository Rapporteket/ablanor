#' Data managment on tables
#'
#' Load data and apply data-management operations. Tables can be used in
#' \emph{pivot-tables} or monthly reports. Notice, only tables when a
#' procedure-date exists!
#'
#'


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
#' getFollowupFiveYrData
#' getGkvData
#' getPromsData
#' getBaseregProsData
#' getBaseregProsHendelseData
#' getBaseregProsFollowup1Data
#' getBaseregProsFollowup0Data
#' getBaseregProsFollowup5Data
NULL

#' @rdname getPrepDataAblanor
#' @export
getBaseregData <- function(singleRow = FALSE,
                           reshId = NULL,
                           userRole,
                           fromDate = NULL,
                           toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getBasereg(singleRow = singleRow,
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
getProsData <- function(singleRow = FALSE,
                        reshId = NULL,
                        userRole,
                        fromDate = NULL,
                        toDate = NULL, ...) {
  . <- ""

  d <- ablanor::getPros(singleRow = singleRow,
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
getMceData <- function(singleRow = FALSE,
                       reshId = NULL,
                       userRole,
                       fromDate = NULL,
                       toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getMce(singleRow = singleRow,
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
getRand12Data <- function(singleRow = FALSE,
                          reshId = NULL,
                          userRole,
                          fromDate = NULL,
                          toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getRand12(singleRow = singleRow,
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
getFollowupBasisData <- function(singleRow = FALSE,
                                 reshId = NULL,
                                 userRole, ...) {

  . <- ""

  d <- ablanor::getFollowupBasis(singleRow = singleRow,
                                 reshId = reshId,
                                 userRole = userRole, ...)
  d_followupBasis <- d$d_followupBasis


  # Samme navn som i kodeboken
  d_followupBasis %<>%
    dplyr::rename_with(.data = .,
                       ~ paste0("FOLLOWUPBASIS_", .x),
                       .cols =c("DATO_FOLLOWUP":"STATUS"))
  names(d_followupBasis) <- tolower(names(d_followupBasis))

  d_followupBasis %>%
    dplyr::mutate(


      # Tidsvariabler for oppfolging
      aar_followup_basis = as.ordered(
        x = lubridate::year(.data$followupbasis_dato_followup)),

      maaned_followup_basis = as.ordered(
        x = sprintf(fmt = "%02d",
                    lubridate::month(.data$followupbasis_dato_followup))),

      maaned_followup = ifelse(
        test = is.na(.data$aar_followup_basis) |
          is.na(.data$maaned_followup_basis),
        yes = NA,
        no = paste0(.data$aar_followup_basis, "-", .data$maaned_followup_basis)))

}


#' @rdname getPrepDataAblanor
#' @export
getFollowupOneYrData <- function(singleRow = FALSE,
                                 reshId = NULL,
                                 userRole, ...) {

  . <- ""

  d <- ablanor::getFollowupOneYr(singleRow = singleRow,
                                 reshId = reshId,
                                 userRole = userRole, ...)
  d_followup <- d$d_followup1


  # Samme navn som i kodeboken
  d_followup %<>%
    dplyr::rename_with(.data = .,
                       ~ paste0("FOLLOWUP1_", .x),
                       .cols =c("DATO_FOLLOWUP":"STATUS"))

  names(d_followup) <- tolower(names(d_followup))

  d_followup %>%
    dplyr::mutate(


      # Tidsvariabler for oppfolging
      aar_followup_1aar = as.ordered(
        x = lubridate::year(.data$followup1_dato_followup)),

      maaned_followup_1aar = as.ordered(
        x = sprintf(fmt = "%02d",
                    lubridate::month(.data$followup1_dato_followup))),

      maaned_followup = ifelse(
        test = is.na(.data$aar_followup_1aar) |
          is.na(.data$maaned_followup_1aar),
        yes = NA,
        no = paste0(.data$aar_followup_1aar,
                    "-",
                    .data$maaned_followup_1aar)))

}


#' @rdname getPrepDataAblanor
#' @export
getFollowupFiveYrData <- function(singleRow = FALSE,
                                  reshId = NULL,
                                  userRole, ...) {

  . <- ""

  d <- ablanor::getFollowupFiveYr(singleRow = singleRow,
                                  reshId = reshId,
                                  userRole = userRole, ...)
  d_followup5 <- d$d_followup5


  # Samme navn som i kodeboken
  d_followup5 %<>%
    dplyr::rename_with(.data = .,
                       ~ paste0("FOLLOWUP5_", .x),
                       .cols =c("DATO_FOLLOWUP":"STATUS"))

  names(d_followup5) <- tolower(names(d_followup5))

  d_followup5 %>%
    dplyr::mutate(


      # Tidsvariabler for oppfolging
      aar_followup_5aar = as.ordered(
        x = lubridate::year(.data$followup5_dato_followup)),

      maaned_followup_5aar = as.ordered(
        x = sprintf(fmt = "%02d",
                    lubridate::month(.data$followup5_dato_followup))),

      maaned_followup_5aar = ifelse(
        test = is.na(.data$aar_followup_5aar) |
          is.na(.data$maaned_followup_5aar),
        yes = NA,
        no = paste0(.data$aar_followup_5aar,
                    "-",
                    .data$maaned_followup_5aar)))

}


#' @rdname getPrepDataAblanor
#' @export
getGkvData <- function(singleRow = FALSE,
                       reshId = NULL,
                       userRole,
                       fromDate = NULL,
                       toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getGkv(singleRow = singleRow,
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
getPromsData <- function(singleRow = FALSE,
                         reshId = NULL,
                         userRole,
                         fromDate = NULL,
                         toDate = NULL, ...) {

  . <- ""

  d <- ablanor::getProms(singleRow = singleRow,
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
getBaseregProsData <- function(singleRow = FALSE,
                               reshId = NULL,
                               userRole,
                               fromDate = NULL,
                               toDate = NULL, ...){

  . <- ""

  d <- ablanor::getBaseregPros(singleRow = singleRow,
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
getBaseregProsHendelseData <- function(singleRow = FALSE,
                                       reshId = NULL,
                                       userRole,
                                       fromDate = NULL,
                                       toDate = NULL, ...){

  d_hendelse <- getHendelse(singleRow = singleRow,
                            reshId = reshId,
                            userRole = userRole,
                            fromDate = fromDate,
                            toDate = toDate)$d_hendelse

  d_mce <- getMce(singleRow = singleRow,
                  reshId = reshId,
                  userRole = userRole,
                  fromDate = fromDate,
                  toDate = toDate)$d_mce

  d_pros <- getPros(singleRow = singleRow,
                    reshId = reshId,
                    userRole = userRole,
                    fromDate = fromDate,
                    toDate = toDate)$d_pros


  d_basereg <- getBasereg(singleRow = singleRow,
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
getBaseregProsFollowup0Data <- function(singleRow = FALSE,
                                        reshId = NULL,
                                        userRole,
                                        fromDate = NULL,
                                        toDate = NULL, ...){

  d_basereg <- ablanor::getBasereg(singleRow = FALSE,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)$d_basereg

  d_pros <- ablanor::getPros(singleRow = FALSE,
                             reshId = reshId,
                             userRole = userRole,
                             fromDate = fromDate,
                             toDate = toDate)$d_pros

  d_followupBasis <- ablanor::getFollowupBasis(singleRow = FALSE,
                                               reshId = reshId,
                                               userRole = userRole,
                                               fromDate = NULL,
                                               toDate = NULL)$d_followupBasis

  d_proms <- ablanor::getProms(singleRow = FALSE,
                               reshId = reshId,
                               userRole = userRole,
                               fromDate = NULL,
                               toDate = NULL)$d_proms

  d_rand12 <- ablanor::getRand12(singleRow = FALSE,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = NULL,
                                 toDate = NULL)$d_rand12

  d_mce <- ablanor::getMce(singleRow = FALSE,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = NULL,
                           toDate = NULL)$d_mce

  d_mcePatientdata <- ablanor::getMcepatientdata(
    singleRow = FALSE,
    reshId = reshId,
    userRole = userRole,
    fromDate = NULL,
    toDate = NULL)$d_mce_patient_data


  d_patientlist <- ablanor::getPatientlist(singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = NULL,
                                           toDate = NULL)$d_patientlist

  d_gkv <- ablanor::getGkv(singleRow = FALSE,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = NULL,
                           toDate = NULL)$d_gkv

  names(d_followupBasis) <- tolower(names(d_followupBasis))
  names(d_proms) <- tolower(names(d_proms))
  names(d_rand12) <- tolower(names(d_rand12))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))
  names(d_gkv) <- tolower(names(d_gkv))




  # 2. PROCESS FOLLOWUP DATA----

  # A) PROMS - KUN BASISFOLLOWUP
  d_proms %<>%
    dplyr::filter(registration_type == "Basisfollowup") %>%
    dplyr::rename("proms_status" = "status",
                  "mceid_followupbasis" = "mceid",
                  "proms_tssendt" = "tssendt",
                  "proms_expiry_date" = "expiry_date") %>%
    dplyr::mutate(eprom_sendt_basis = "ja") %>%
    dplyr::select(mceid_followupbasis,
                  proms_tssendt,
                  proms_status,
                  proms_expiry_date,
                  eprom_sendt_basis)

  # B) RAND 12 - KUN  BASIS-FOLLOWUP, MANUELL OG ELEKTRONISKE
  # RAND12 skjema fra før eprom ved basis, ble samlet inn og plottet manuelt,
  # på utskrivelses-skjema (mceid til prosedyren)
  d_rand12_manual <- d_rand12  %>%
    dplyr::filter(followup_parent_type %in% 1:4, complete == 1) %>%
    dplyr::mutate(besvart_rand12 = "manuelt")

  # RAND12 skjema etter før eprom ved basis,
  # henger på elekronisk oppfølging (mceid til followup)
  d_rand12_eprom <- d_rand12 %>%
    dplyr::filter(followup_parent_type %in% 7, complete == 1) %>%
    dplyr::mutate(besvart_rand12 = "elektronisk") %>%
    dplyr::rename("mceid_followupbasis" = "mceid") %>%
    dplyr::left_join(.,
                     d_mce %>% dplyr::transmute(mceid_followupbasis = mceid,
                                                mceid = parentmceid,),
                     by = "mceid_followupbasis") %>%
    dplyr::relocate(mceid, .before = followup_parent_type)


  # Slå sammen rand12 fra manuell plotting og fra eprom ved basis
  # Merk, to pasienter fra 8/11-2023 (innføring eprom basis) har begge deler,
  # vi bruker da eprom.
  # SAMLET ALLE RAND 12 henger på MCEID
  dobbel_rand12 <- dplyr::inner_join(
    d_rand12_eprom %>% dplyr::select(mceid),
    d_rand12_manual %>% dplyr::select(mceid),
    by = "mceid") %>%
    dplyr::pull()
  d_rand12_basis <- dplyr::bind_rows(
    d_rand12_manual %>% dplyr::filter(!mceid %in% dobbel_rand12),
    d_rand12_eprom %>% dplyr::select(-mceid_followupbasis))

  d_rand12_basis %<>%
    dplyr::select(mceid, centreid, dato_rand12, besvart_rand12,
                  rand_1:rand_7)


  # GKV
  d_gkv %<>%
    dplyr::filter(complete == 1 & form_completed_via_proms == 1) %>%
    dplyr::select(mceid, centreid, dato_gkv, gkv_1:gkv_12) %>%
    dplyr::rename("mceid_followupbasis"  = mceid)


  # Tar utgangspunkt i alle tilgjengelige oppfølgingsdata for 1 aar
  # Legger til mceid for followup og proms variabler
  followup_dataBasis <- d_followupBasis %>%
    dplyr::rename("mceid_followupbasis" = mceid) %>%
    dplyr::rename_with(.data = .,
                       ~ paste0("followupbasis_", .x),
                       .cols =c("complete":"status", "tscreated")) %>%
    dplyr::select(-tsupdated,
                  -updatedby,
                  -form_completed_via_proms,
                  -first_time_closed,
                  -first_time_closed_by,
                  -createdby) %>%
    dplyr::left_join(.,
                     d_mce %>%
                       dplyr::filter(mcetype == 7) %>%
                       dplyr::select(mceid, parentmceid) %>%
                       dplyr::rename("mceid_followupbasis" = mceid,
                                     "mceid" = parentmceid),
                     by = "mceid_followupbasis") %>%
    dplyr::mutate(eprom_opprettet_basis = "ja") %>%
    dplyr::left_join(.,
                     d_proms,
                     by = "mceid_followupbasis") %>%

    dplyr::left_join(.,
                     d_gkv,
                     by = c("mceid_followupbasis", "centreid")) %>%
    dplyr::relocate("mceid", .before = "mceid_followupbasis") %>%
    dplyr::relocate("eprom_opprettet_basis",
                    "eprom_sendt_basis",
                    .before = "dato_followup") %>%
    dplyr::relocate("proms_tssendt",
                    "proms_status",
                    "proms_expiry_date",
                    .before ="dato_followup"  )






  # PROCESS PATIENT - BASEREG AND PROCEDURE DATA ----
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros,
      redo, redo_times, narkose,
      pros_varighet, rtg_tid, abla_varighet,
      dplyr::contains("aryt_i"),
      dplyr::contains("sys_"),
      dplyr::contains("abla_strat"),
      akutt_suksess,
      oppsummering,
      dplyr::contains("komp_")
    )


  d_basereg %<>% dplyr::select(mceid:forskyvning, ehra_sympt)

  d_mcePatientdata %<>%
    dplyr::select(pid, mceid) %>%
    dplyr::rename(patient_id = pid)

  d_patientlist %<>%
    dplyr::select(id, birth_date, gender,
                  deceased, deceased_date,
                  ssn_type, ssnsubtype) %>%
    dplyr::rename(patient_id = id)


  # ENDELIG DATASETT MED PASIENT - BASEREG - PROSEDYRE - FOLLOWUPDATA ----
  df <- dplyr::right_join(d_basereg,
                          d_pros,
                          by = c("mceid", "centreid")) %>%
    dplyr::filter(!is.na(forlopstype))%>%
    dplyr::right_join(x = d_mce %>%
                        dplyr::select(mceid, patient_id, has_basisfollowup),
                      y = .,
                      by = "mceid") %>%
    dplyr::right_join(x = d_patientlist %>% dplyr::distinct(),
                      y = .,
                      by = c("patient_id"),
                      multiple = "all") %>%
    dplyr::left_join(.,
                     followup_dataBasis,
                     by = c("mceid", "centreid")) %>%
    dplyr::left_join(.,
                     d_rand12_basis,
                     by = c("mceid", "centreid"))


  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followupbasis_status)) %>%
      dplyr::pull(dato_pros)))




  # KRITERIER FOR EPROM ----
  df %<>%
    # KRITERIER FOR UTSENDING
    # KRITERIE 1. Alder. Under 16 p<U+00E5> prosedyretidspunktet.
    ablanor::utlede_alder() %>%
    ablanor::utlede_aldersklasse() %>%
    dplyr::mutate(kriterie_alder = ifelse(test = alder >= 16,
                                          yes = "ja",
                                          no = "nei")) %>%

    # KRITERIE 2. Norsk f<U+00F8>dselsnummer
    dplyr::mutate(kriterie_norsk = ifelse(
      test = (ssn_type %in% 1 & ssnsubtype %in% c(1, 3)),
      yes = "ja",
      no = "nei")) %>%

    # KRITERIE 3. Levende dagen etter etter prosedyren
    dplyr::mutate(kriterie_levende = ifelse(
      test = (deceased %in% 0 |
                (deceased %in% 1 & deceased_date > dato_pros)),
      yes = "ja",
      no = "nei")) %>%

    # KRITERIE 4: Minst en av prosedyrevarighet, rtg_tid eller abla_varighet
    # er fylt ut
    dplyr::mutate(
      kriterie_tid = ifelse(
        test = (!is.na(pros_varighet) |
                  !is.na(rtg_tid) |
                  !is.na(abla_varighet)),
        yes = "ja",
        no = "nei"),

      # KRITERIE ALLE
      kriterie_alle_basis = ifelse(
        test = (kriterie_tid %in% "ja" &
                  kriterie_alder %in% "ja" &
                  kriterie_levende %in% "ja" &
                  kriterie_norsk %in% "ja"),
        yes = "ja",
        no = "nei"))


  # UTLEDE TIDSVARIABLER -----
  df %<>%
    dplyr::mutate(

      # Tidsvariabler for prosedyre
      aar_prosedyre = as.ordered(lubridate::year(dato_pros)),
      maaned_nr_prosedyre = as.ordered(sprintf(fmt = "%02d",
                                               lubridate::month(dato_pros))),
      maaned_prosedyre = ifelse(
        test = (is.na(aar_prosedyre) | is.na(maaned_nr_prosedyre)),
        yes = NA,
        no = paste0(aar_prosedyre, "-", maaned_nr_prosedyre))) %>%
    dplyr::select(-maaned_nr_prosedyre) %>%
    dplyr::arrange(mceid)



  # DATAGRUNNLAG ----
  df %<>%

    dplyr::mutate(
      eprom_datagrunnlag_basis = factor(
        x = dplyr::case_when(

          # ALT FOR GAMLE REGISTRERINGER
          dato_pros < as.Date("2023-11-08", format = "%Y-%m-%d") ~
            "foer innfoering av eproms basis",

          # EPROMS OPPRETTET OG SATT TIL AVD<U+00D8>D MED EN GANG
          (has_basisfollowup %in% 1 &
             eprom_opprettet_basis %in% "ja" &
             followupbasis_incomplete_reason %in% 3) ~
            "nei, opprettet satt til doed",

          # EPROMS SENDT UT UTEN AT ALLE KRITERIER VAR OPPFYLT
          (has_basisfollowup %in% 1 &
             eprom_opprettet_basis %in% "ja" &
             kriterie_alle_basis %in% "nei" &
             eprom_sendt_basis %in% "ja") ~
            "nei, eprom feilaktig sendt, sjekk kriterier",

          # NY VERSJON: KONTROLL KRITIER F<U+00D8>R OPPRETTELSE
          (kriterie_alle_basis %in% "nei" &
             is.na(eprom_opprettet_basis)) ~
            "nei, ikke opprettet etter kriteriesjekk",

          # NY VERSJON: OPPRETTELES EN DAG, OG BESTILLING INNEN 30 DAGER ETTER
          # "BESTILT I DAG, SENDES I MORGEN"
          (has_basisfollowup %in% 1 &
             eprom_opprettet_basis %in% "ja" &
             kriterie_alle_basis %in% "ja" &
             is.na(eprom_sendt_basis)) ~
            "nei, eprom venter paa utsendelse",

          # DISSE ER MED I DATAGRUNNLAGET!
          (has_basisfollowup %in% 1 &
             eprom_opprettet_basis %in% "ja" &
             eprom_sendt_basis %in% "ja" &
             kriterie_alle_basis %in% "ja" ) ~ "ja"),


        levels = c("ja",
                   "foer innfoering av eproms basis",
                   "nei, opprettet satt til doed",
                   "nei, eprom feilaktig sendt, sjekk kriterier",
                   "nei, ikke opprettet etter kriteriesjekk",
                   "nei, eprom venter paa utsendelse"),
        ordered  = TRUE),

      eprom_besvart_basis =  dplyr::case_when(
        eprom_datagrunnlag_basis %in% "ja" &
          proms_status %in% 3 ~ "datagrunnlag og besvart",

        eprom_datagrunnlag_basis %in% "ja" &
          !proms_status %in% 3 ~ "datagrunnlag, men ikke besvart")

    )




  if(singleRow == TRUE) {
    # Return first row only
    df %>% dplyr::filter(dplyr::row_number() == 1)
  } else {
    # Return all
    df
  }
}




#' @rdname getPrepDataAblanor
#' @export
getBaseregProsFollowup1Data <- function(singleRow = FALSE,
                                        reshId = NULL,
                                        userRole,
                                        fromDate = NULL,
                                        toDate = NULL, ...){



  # 1 GET ALL TABELS NEEDED ----
  d_basereg <- ablanor::getBasereg(singleRow = FALSE,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)$d_basereg

  d_pros <- ablanor::getPros(singleRow = FALSE,
                             reshId = reshId,
                             userRole = userRole,
                             fromDate = fromDate,
                             toDate = toDate)$d_pros

  d_followup1 <- ablanor::getFollowupOneYr(singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = NULL,
                                           toDate = NULL)$d_followup1

  d_proms <- ablanor::getProms(singleRow = FALSE,
                               reshId = reshId,
                               userRole = userRole,
                               fromDate = NULL,
                               toDate = NULL)$d_proms

  d_rand12 <- ablanor::getRand12(singleRow = FALSE,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = NULL,
                                 toDate = NULL)$d_rand12

  d_mce <- ablanor::getMce(singleRow = FALSE,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = NULL,
                           toDate = NULL)$d_mce

  d_mcePatientdata <- ablanor::getMcepatientdata(
    singleRow = FALSE,
    reshId = reshId,
    userRole = userRole,
    fromDate = NULL,
    toDate = NULL)$d_mce_patient_data


  d_patientlist <- ablanor::getPatientlist(singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = NULL,
                                           toDate = NULL)$d_patientlist

  names(d_followup1) <- tolower(names(d_followup1))
  names(d_proms) <- tolower(names(d_proms))
  names(d_rand12) <- tolower(names(d_rand12))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))



  # 2. PROCESS FOLLOWUP DATA----

  # VELGER KUN 1 års oppfølging (PROMS)
  d_proms %<>%
    dplyr::filter(registration_type == "Followup") %>%
    dplyr::rename("proms_status" = "status",
                  "mceid_followup" = "mceid",
                  "proms_tssendt" = "tssendt",
                  "proms_expiry_date" = "expiry_date") %>%
    dplyr::mutate(eprom_sendt_1aar = "ja") %>%
    dplyr::select(mceid_followup,
                  proms_tssendt,
                  proms_status,
                  proms_expiry_date,
                  eprom_sendt_1aar)

  # RAND 12 fra kun 1 års oppfølging
  d_rand12 %<>%
    dplyr::filter(followup_parent_type %in% 9, complete == 1) %>%
    dplyr::rename("mceid_followup" = "mceid",
                  "rand_complete" = "complete",
                  "rand_incomplete_reason"  = "incomplete_reason") %>%
    dplyr::select(mceid_followup:rand_7) %>%
    dplyr::select(-followup_parent_type)

  # Tar utgangspunkt i alle tilgjengelige oppfølgingsdata for 1 aar
  # Legger til mceid for followup og proms variabler
  followup_data <- d_followup1 %>%
    dplyr::rename("mceid_followup" = mceid) %>%
    dplyr::rename_with(.data = .,
                       ~ paste0("followup1_", .x),
                       .cols =c("complete":"status", "tscreated")) %>%
    dplyr::select(-tsupdated,
                  -updatedby,
                  -form_completed_via_proms,
                  -first_time_closed,
                  -first_time_closed_by,
                  -createdby) %>%
    dplyr::left_join(.,
                     d_mce %>%
                       dplyr::filter(mcetype == 9) %>%
                       dplyr::select(mceid, parentmceid) %>%
                       dplyr::rename("mceid_followup" = mceid,
                              "mceid" = parentmceid),
                     by = "mceid_followup") %>%
    dplyr::mutate(eprom_opprettet_1aar = "ja") %>%
    dplyr::left_join(.,
                     d_proms,
                     by = "mceid_followup") %>%
    dplyr::left_join(.,
                     d_rand12,
                     by = c("mceid_followup", "centreid")) %>%
    dplyr::relocate("mceid", .before = "mceid_followup") %>%
    dplyr::relocate("eprom_opprettet_1aar",
                    "eprom_sendt_1aar",
                    .before = "dato_followup") %>%
    dplyr::relocate("proms_tssendt",
                    "proms_status",
                    "proms_expiry_date",
                    .before ="dato_followup"  )



  # PROCESS PATIENT - BASEREG AND PROCEDURE DATA ----
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros,
      redo, redo_times, narkose,
      dplyr::contains("aryt_i"),
      dplyr::contains("sys_"),
      dplyr::contains("abla_strat"),
      akutt_suksess,
      oppsummering,
      dplyr::contains("komp_")
    )


  d_basereg %<>%
    dplyr::select(
      mceid:forskyvning,
      ehra_sympt
    )

  d_mcePatientdata %<>%
    dplyr::select(pid, mceid) %>%
    dplyr::rename(patient_id = pid)

  d_patientlist %<>%
    dplyr::select(id, birth_date, gender,
                  deceased, deceased_date,
                  ssn_type, ssnsubtype) %>%
    dplyr::rename(patient_id = id)


  # ENDELIG DATASETT MED PASIENT - BASEREG - PROSEDYRE - FOLLOWUPDATA ----
  df <- dplyr::right_join(d_basereg,
                          d_pros,
                          by = c("mceid", "centreid")) %>%
    dplyr::filter(!is.na(forlopstype))%>%
    dplyr::right_join(x = d_mce %>%
                        dplyr::select(mceid, patient_id, has_followup),
                      y = .,
                      by = "mceid") %>%
    dplyr::right_join(x = d_patientlist %>% dplyr::distinct(),
                      y = .,
                      by = c("patient_id"),
                      multiple = "all") %>%
    dplyr::left_join(.,
                     followup_data,
                     by = c("mceid", "centreid"))



  # HJELPEVARIABLER EPROM ----
  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followup1_status)) %>%
      dplyr::pull(dato_pros)))


  df %<>%
    dplyr::mutate(

      # I Versjon 1.5 ble opprettelse/bestilling av eproms skilt.
      # I ny versjon så sjekkes alle kriterier før opprettelse av e-prom
      versjon_1_5_eller_mer = ifelse(
        test = (dato_pros >= as.Date("2022-11-22", format = "%Y-%m-%d")),
        yes = "ja",
        no = "nei"),

      # 50 uker etter prosedyredato blir e-proms  opprettet
      dato_followup_teoretisk_1aar = dato_pros + lubridate::days(351)) %>%

    # KRITERIER FOR UTSENDING
    # KRITERIE 1. Alder. Under 16 på prosedyretidspunktet.
    ablanor::utlede_alder() %>%
    ablanor::utlede_aldersklasse() %>%
    dplyr::mutate(kriterie_alder = ifelse(test = alder >= 16,
                                          yes = "ja",
                                          no = "nei")) %>%

    # KRITERIE 2. Norsk fødselsnummer
    dplyr::mutate(kriterie_norsk = ifelse(
      test = (ssn_type %in% 1 & ssnsubtype %in% c(1, 3)),
      yes = "ja",
      no = "nei")) %>%

    # KRITERIE 3. Levende 50 uker etter prosedyren
    dplyr::mutate(kriterie_levende_1aar = ifelse(
      test = (deceased %in% 0 |
                (deceased %in% 1 &
                   deceased_date > dato_followup_teoretisk_1aar)),
      yes = "ja",
      no = "nei"))

  # KRITERIE 4: Ingen ny prosedyre av samme type
  df %<>%
    dplyr::arrange(dato_pros) %>%
    dplyr::group_by(patient_id, forlopstype) %>%
    dplyr::mutate(
      antall_pros = dplyr::n(),
      dg_til_neste = as.numeric(difftime(dplyr::lead(dato_pros),
                                         dato_pros,
                                         units = "days"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      kriterie_nyeste_1aar = ifelse(
        test= (is.na(dg_til_neste) | dg_til_neste > 351),
        yes = "ja",
        no = "nei"),

      # KRITERIE ALLE
      kriterie_alle_1aar = ifelse(
        test = (kriterie_nyeste_1aar %in% "ja" &
                  kriterie_alder %in% "ja" &
                  kriterie_levende_1aar %in% "ja" &
                  kriterie_norsk %in% "ja"),
        yes = "ja",
        no = "nei"))



  df %<>%
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
      aar_followup_1aar = as.ordered(
        x = lubridate::year(dato_followup)),

      # Tidsvariabler for opprettet followup
      aar_followup_tscreated_1aar = as.ordered(
        x = lubridate::year(followup1_tscreated)),

      # Tidsvariabler for bestilt followup
      aar_proms_tssendt_1aar = as.ordered(
        x = lubridate::year(proms_tssendt)),

      dg_pros_opprettet = as.numeric(difftime(
        followup1_tscreated,
        dato_pros,
        units = "days"))) %>%
    dplyr::select(-maaned_nr_prosedyre) %>%
    dplyr::arrange(mceid) %>%

    dplyr::mutate(
      eprom_kjente_feil_1aar = dplyr::case_when(

        dato_pros == as.Date("2021-09-01", format = "%Y-%m-%d") ~
          "teknisk problem",

        (dato_pros >= as.Date("2020-01-01", format = "%Y-%m-%d") &
           dato_pros <= as.Date("2020-01-24", format = "%Y-%m-%d")) ~
          "teknisk problem",

        (dato_pros >= as.Date("2022-11-22", format = "%Y-%m-%d") &
           dato_pros <= as.Date("2022-11-25", format = "%Y-%m-%d") &
           eprom_opprettet_1aar %in% "ja" &
           kriterie_alle_1aar %in% "ja" &
           is.na(eprom_sendt_1aar))~
          "teknisk problem",

        TRUE ~ "nei"),

      eprom_datagrunnlag_1aar = factor(
        x = dplyr::case_when(

          #  ALT FOR NYE REGISTRERINGER
          dato_pros > nyeste_eprom_bestilling ~
            "nei, registreringen er for ny",

          # ALT FOR GAMLE REGISTRERINGER
          dato_pros < as.Date("2020-01-01", format = "%Y-%m-%d") ~
            "nei, før innføring av 1års oppf.",

          dato_pros  == as.Date("2020-01-01", format = "%Y-%m-%d") &
            is.na(eprom_opprettet_1aar) ~
            "nei, før innføring av 1års oppf.",

          # EPROMS OPPRETTET OG SATT TIL AVDØD MED EN GANG
          (has_followup %in% 1 &
             eprom_opprettet_1aar %in% "ja" &
             followup1_incomplete_reason %in% 3) ~
            "nei, opprettet satt til død",

          # EPROMS SENDT UT UTEN AT ALLE KRITERIER VAR OPPFYLT
          (has_followup %in% 1 &
             eprom_opprettet_1aar %in% "ja" &
             kriterie_alle_1aar %in% "nei" &
             eprom_sendt_1aar %in% "ja" &
             eprom_kjente_feil_1aar %in% "nei") ~
            "nei, eprom feilaktig sendt, sjekk kriterier",


          # NY VERSJON: KONTROLL KRITIER FØR OPPRETTELSE
          (has_followup %in% 1 &
             versjon_1_5_eller_mer %in% "ja" &
             is.na(eprom_opprettet_1aar)) ~
            "nei, ikke opprettet etter kriteriesjekk",

          # NY VERSJON: OPPRETTELES EN DAG, OG BESTILLING INNEN 30 DAGER ETTER
          # "BESTILT I DAG, SENDES I MORGEN"
          (has_followup %in% 1 &
             eprom_opprettet_1aar %in% "ja" &
             kriterie_alle_1aar %in% "ja" &
             eprom_kjente_feil_1aar %in% "nei" &
             is.na(eprom_sendt_1aar)) ~
            "nei, eprom venter på utsendelse",


          # GAMMEL VERSJON: EPROM  OPPRETTET FOR ALLE, MEN
          # KONTROLL KRITERIER FØR UTSENDING
          (has_followup %in% 1 &
             versjon_1_5_eller_mer %in% "nei" &
             eprom_opprettet_1aar %in% "ja" &
             is.na(eprom_sendt_1aar) &
             (kriterie_levende_1aar %in% "nei" |
                kriterie_norsk %in% "nei" |
                kriterie_alder %in% "nei") &
             !followup1_incomplete_reason %in% 3 &
             eprom_kjente_feil_1aar %in% "nei") ~
            "nei, opprettet men ikke sendt etter kriteriesjekk",


          # NY VERJSON: OPPRETTET, MEN IKKE SENDT SKYLES TEKNISKE PROBLEM
          # RETT ETTER RELEASE
          (has_followup %in% 1 &
             versjon_1_5_eller_mer %in% "ja" &
             eprom_opprettet_1aar %in% "ja" &
             is.na(eprom_sendt_1aar) &
             !eprom_kjente_feil_1aar %in% "nei") ~
            "nei, opprettet men teknisk feil ved bestilling",

          # GAMMEL VERSJON: OPPRETTET, TEKNISK PROBLEM VED
          # UTSENDELSE I 2021/22,
          # MED ELLER UTEN FEILAKTIG UTSENDING I 2023
          (has_followup %in% 1 &
             versjon_1_5_eller_mer %in% "nei" &
             eprom_opprettet_1aar %in% "ja" &
             !eprom_kjente_feil_1aar %in% "nei") ~
            "nei, teknisk, mangler utsending eller feilaktig sendt i 2023",


          # DISSE ER MED I DATAGRUNNLAGET!
          (has_followup %in% 1 &
             eprom_opprettet_1aar %in% "ja" &
             kriterie_alle_1aar %in% "ja" &
             eprom_kjente_feil_1aar %in% "nei") ~ "ja"),


        levels = c(
          "ja",
          "nei, registreringen er for ny",
          "nei, før innføring av 1års oppf.",
          "nei, opprettet satt til død",
          "nei, eprom feilaktig sendt, sjekk kriterier",
          "nei, ikke opprettet etter kriteriesjekk",
          "nei, eprom venter på utsendelse",
          "nei, opprettet men ikke sendt etter kriteriesjekk",
          "nei, opprettet men teknisk feil ved bestilling",
          "nei, teknisk, mangler utsending eller feilaktig sendt i 2023"),
        ordered  = TRUE),



      eprom_besvart_1aar =  dplyr::case_when(
        eprom_datagrunnlag_1aar %in% "ja" &
          proms_status %in% 3 ~ "datagrunnlag og besvart",

        eprom_datagrunnlag_1aar %in% "ja" &
          !proms_status %in% 3 ~ "datagrunnlag, men ikke besvart")
    )




  # RYDDE ENDELIG DATASETT ----
  df %<>%
    dplyr::relocate("patient_id",
                    "mceid",
                    "mceid_followup",
                    "centreid",
                    "eprom_datagrunnlag_1aar",
                    "eprom_besvart_1aar",
                    .before = "birth_date") %>%
    dplyr::select(-dato_bas) %>%
    dplyr::relocate("has_followup", .after = "eprom_kjente_feil_1aar")



  if(singleRow == TRUE) {
    # Return first row only
    df %>% dplyr::filter(dplyr::row_number() == 1)
  } else {
    # Return all
    df
  }
}






#' @rdname getPrepDataAblanor
#' @export
getBaseregProsFollowup5Data <- function(singleRow = FALSE,
                                        reshId = NULL,
                                        userRole,
                                        fromDate = NULL,
                                        toDate = NULL, ...){

  # 1 GET ALL TABELS NEEDED ----
  d_basereg <- ablanor::getBasereg(singleRow = FALSE,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)$d_basereg

  d_pros <- ablanor::getPros(singleRow = FALSE,
                             reshId = reshId,
                             userRole = userRole,
                             fromDate = fromDate,
                             toDate = toDate)$d_pros

  d_followup5 <- ablanor::getFollowupFiveYr(singleRow = FALSE,
                                            reshId = reshId,
                                            userRole = userRole,
                                            fromDate = NULL,
                                            toDate = NULL)$d_followup5

  d_proms <- ablanor::getProms(singleRow = FALSE,
                               reshId = reshId,
                               userRole = userRole,
                               fromDate = NULL,
                               toDate = NULL)$d_proms

  d_rand12 <- ablanor::getRand12(singleRow = FALSE,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = NULL,
                                 toDate = NULL)$d_rand12

  d_mce <- ablanor::getMce(singleRow = FALSE,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = NULL,
                           toDate = NULL)$d_mce

  d_mcePatientdata <- ablanor::getMcepatientdata(
    singleRow = FALSE,
    reshId = reshId,
    userRole = userRole,
    fromDate = NULL,
    toDate = NULL)$d_mce_patient_data


  d_patientlist <- ablanor::getPatientlist(singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = NULL,
                                           toDate = NULL)$d_patientlist

  names(d_followup5) <- tolower(names(d_followup5))
  names(d_proms) <- tolower(names(d_proms))
  names(d_rand12) <- tolower(names(d_rand12))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))



  # 2. PROCESS FOLLOWUP DATA----

  # VELGER KUN 5 års oppfølging (PROMS)
  d_proms %<>%
    dplyr::filter(registration_type == "Fiveyearfollowup") %>%
    dplyr::rename("proms_status" = "status",
                  "mceid_followup" = "mceid",
                  "proms_tssendt" = "tssendt",
                  "proms_expiry_date" = "expiry_date") %>%
    dplyr::mutate(eprom_sendt_5aar = "ja") %>%
    dplyr::select(mceid_followup,
                  proms_tssendt,
                  proms_status,
                  proms_expiry_date,
                  eprom_sendt_5aar)

  # RAND 12 fra kun 1 års oppfølging
  d_rand12 %<>%
    dplyr::filter(followup_parent_type %in% 10, complete == 1) %>%
    dplyr::rename("mceid_followup" = "mceid",
                  "rand_complete" = "complete",
                  "rand_incomplete_reason"  = "incomplete_reason") %>%
    dplyr::select(mceid_followup:rand_7) %>%
    dplyr::select(-followup_parent_type)

  # Tar utgangspunkt i alle tilgjengelige oppfølgingsdata for 1 aar
  # Legger til mceid for followup og proms variabler
  followup_data <- d_followup5 %>%
    dplyr::rename("mceid_followup" = mceid) %>%
    dplyr::rename_with(.data = .,
                       ~ paste0("followup5_", .x),
                       .cols =c("complete":"status", "tscreated")) %>%
    dplyr::select(-tsupdated,
                  -updatedby,
                  -form_completed_via_proms,
                  -first_time_closed,
                  -first_time_closed_by,
                  -createdby) %>%
    dplyr::left_join(.,
                     d_mce %>%
                       dplyr::filter(mcetype == 10) %>%
                       dplyr::select(mceid, parentmceid) %>%
                       dplyr::rename("mceid_followup" = mceid,
                              "mceid" = parentmceid),
                     by = "mceid_followup") %>%
    dplyr::mutate(eprom_opprettet_5aar = "ja") %>%
    dplyr::left_join(.,
                     d_proms,
                     by = "mceid_followup") %>%
    dplyr::left_join(.,
                     d_rand12,
                     by = c("mceid_followup", "centreid")) %>%
    dplyr::relocate("mceid", .before = "mceid_followup") %>%
    dplyr::relocate("eprom_opprettet_5aar",
                    "eprom_sendt_5aar",
                    .before = "dato_followup") %>%
    dplyr::relocate("proms_tssendt",
                    "proms_status",
                    "proms_expiry_date",
                    .before ="dato_followup"  )




  # PROCESS PATIENT - BASEREG AND PROCEDURE DATA ----
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros,
      redo, redo_times, narkose,
      dplyr::contains("aryt_i"),
      dplyr::contains("sys_"),
      dplyr::contains("abla_strat"),
      akutt_suksess,
      oppsummering,
      dplyr::contains("komp_")
    )


  d_basereg %<>% dplyr::select(mceid:forskyvning, ehra_sympt)

  d_mcePatientdata %<>%
    dplyr::select(pid, mceid) %>%
    dplyr::rename(patient_id = pid)

  d_patientlist %<>%
    dplyr::select(id, birth_date, gender,
                  deceased, deceased_date,
                  ssn_type, ssnsubtype) %>%
    dplyr::rename(patient_id = id)



  # ENDELIG DATASETT MED PASIENT - BASEREG - PROSEDYRE - FOLLOWUPDATA ----
  df <- dplyr::right_join(d_basereg,
                   d_pros,
                   by = c("mceid", "centreid")) %>%
    dplyr::filter(!is.na(forlopstype))%>%
    dplyr::right_join(x = d_mce %>%
                        dplyr::select(mceid, patient_id, has_fiveyearfollowup),
                      y = .,
                      by = "mceid") %>%
    dplyr::right_join(x = d_patientlist %>% dplyr::distinct(),
                      y = .,
                      by = c("patient_id"),
                      multiple = "all") %>%
    dplyr::left_join(.,
                     followup_data,
                     by = c("mceid", "centreid"))




  # HJELPEVARIABLER EPROM ----
  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followup5_status)) %>%
      dplyr::pull(dato_pros)))



  df %<>%
    dplyr::mutate(


      # 4år 50 uker etter prosedyredato blir e-proms  opprettet
      dato_followup_teoretisk_5aar = dato_pros + lubridate::days(1811)) %>%

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

    # KRITERIE 3. Levende 4 år og 50 uker etter prosedyren
    dplyr::mutate(kriterie_levende_5aar = ifelse(
      test = (deceased %in% 0 |
                (deceased %in% 1 &
                   deceased_date > dato_followup_teoretisk_5aar )),
      yes = "ja",
      no = "nei"))

  # KRITERIE 4: Ingen ny prosedyre av samme type
  df %<>%
    dplyr::arrange(dato_pros) %>%
    dplyr::group_by(patient_id, forlopstype) %>%
    dplyr::mutate(
      antall_pros = dplyr::n(),
      dg_til_neste = as.numeric(difftime(dplyr::lead(dato_pros),
                                         dato_pros,
                                         units = "days"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      kriterie_nyeste_5aar = ifelse(
        test= (is.na(dg_til_neste) | dg_til_neste > 1811),
        yes = "ja",
        no = "nei"),

      # KRITERIE ALLE
      kriterie_alle_5aar = ifelse(
        test = (kriterie_nyeste_5aar %in% "ja" &
                  kriterie_alder %in% "ja" &
                  kriterie_levende_5aar %in% "ja" &
                  kriterie_norsk %in% "ja"),
        yes = "ja",
        no = "nei"))



  df %<>%
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
      aar_followup_5aar = as.ordered(
        x = lubridate::year(dato_followup)),


      # Tidsvariabler for opprettet followup
      aar_followup_tscreated_5aar = as.ordered(
        x = lubridate::year(followup5_tscreated)),


      # Tidsvariabler for bestilt followup
      aar_proms_tssendt_5aar = as.ordered(lubridate::year(proms_tssendt)),


      dg_pros_opprettet = as.numeric(difftime(
        followup5_tscreated,
        dato_pros,
        units = "days"
      ))
    ) %>%
    dplyr::select(-maaned_nr_prosedyre) %>%
    dplyr::arrange(mceid) %>%


    dplyr::mutate(

      eprom_datagrunnlag_5aar = factor(
        x = dplyr::case_when(

          #  ALT FOR NYE REGISTRERINGER
          dato_pros > nyeste_eprom_bestilling ~
            "nei, registreringen er for ny",


          # EPROMS OPPRETTET OG SATT TIL AVDØD MED EN GANG
          (has_fiveyearfollowup %in% 1 &
             eprom_opprettet_5aar %in% "ja" &
             followup5_incomplete_reason %in% 3) ~
            "nei, opprettet satt til død",


          # NY VERSJON: KONTROLL KRITIER FØR OPPRETTELSE
          (has_fiveyearfollowup %in% 1 &
             is.na(eprom_opprettet_5aar)) ~
            "nei, ikke opprettet etter kriteriesjekk",

          # NY VERSJON: OPPRETTELES EN DAG, OG BESTILLING INNEN 30 DAGER ETTER
          # "BESTILT I DAG, SENDES I MORGEN"
          (has_fiveyearfollowup %in% 1 &
             eprom_opprettet_5aar %in% "ja" &
             kriterie_alle_5aar %in% "ja" &
             is.na(eprom_sendt_5aar)) ~
            "nei, eprom venter på utsendelse",


          # DISSE ER MED I DATAGRUNNLAGET!
          (has_fiveyearfollowup %in% 1 &
             eprom_opprettet_5aar %in% "ja" &
             kriterie_alle_5aar %in% "ja") ~ "ja"
          ),


          levels = c("ja",
                     "nei, registreringen er for ny",
                     "nei, opprettet satt til død",
                     "nei, ikke opprettet etter kriteriesjekk",
                     "nei, eprom venter på utsendelse"),

          ordered  = TRUE),

      eprom_besvart_5aar =  dplyr::case_when(
        eprom_datagrunnlag_5aar %in% "ja" &
          proms_status %in% 3 ~ "datagrunnlag og besvart",

        eprom_datagrunnlag_5aar %in% "ja" &
          !proms_status %in% 3 ~ "datagrunnlag, men ikke besvart")
    )


  # RYDDE ENDELIG DATASETT ----
  df %<>%
    dplyr::relocate("patient_id",
                    "mceid",
                    "mceid_followup",
                    "centreid",
                    "eprom_datagrunnlag_5aar",
                    "eprom_besvart_5aar",
                    .before = "birth_date") %>%
    dplyr::select(-dato_bas) %>%
    dplyr::relocate("has_fiveyearfollowup", .after = "kriterie_alle_5aar")




  if(singleRow == TRUE) {
    # Return first row only
    df %>% dplyr::filter(dplyr::row_number() == 1)
  } else {
    # Return all
    df
  }
}




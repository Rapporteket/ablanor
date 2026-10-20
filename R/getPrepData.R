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
#' @aliases getBaseregData getProsData getMceData getRand12Data getFollowupBasisData getFollowupOneYrData getFollowupFiveYrData getGkvData getPromsData getBaseregProsData getBaseregProsHendelseData getBaseregProsFollowup1Data getBaseregProsFollowup0Data getBaseregProsFollowup5Data
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
    dplyr::relocate(c("MCEID", "CENTREID"), .before = "PATIENT_ID") %>%
    dplyr::relocate("DATO_PROS", .after = "CENTREID")


  names(d_ablanor) <- tolower(names(d_ablanor))



  # UTLEDETE VARIABLER

  # ALDER :
  d_ablanor %<>%
    ablanor::utlede_alder(.) %>%
    ablanor::utlede_alder_75(.) %>%
    ablanor::utlede_alder_65_75(.) %>%
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

  # CHA2DS2VASc og CHA2DS2VA
  d_ablanor %<>% ablanor::utlede_CHA2DS2VASc(.)
  d_ablanor %<>% ablanor::utlede_CHA2DS2VA(.)

  # Indikator tamponade, indikator for avbrudd
  d_ablanor %<>%
    ablanor::indik_tamponade(.) %>%
    ablanor::indik_ferdig_komplik(.) %>%
    ablanor::indik_akuttsuksess(.) %>%
    ablanor::indik_pacemaker(.) %>%
    ablanor::indik_avbrudd(.)

  d_ablanor %>%
    ablanor::utlede_tidsvariabler() %>%
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
getBaseregProsFollowup0Data_v2 <- function(singleRow = FALSE,
                                           reshId = NULL,
                                           userRole,
                                           fromDate = NULL,
                                           toDate = NULL, ...){

  # 1. HENTE DATA
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
  d_mce <- ablanor::getMce(singleRow = FALSE,
                           reshId = reshId,
                           userRole = userRole,
                           fromDate = NULL,
                           toDate = NULL)$d_mce


  d_basisProm <- ablanor::getBasisProm( ...)$d_basisprom
  d_basisRand <- ablanor::getBasisProm( ...)$d_basisrand

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

  names(d_basisProm) <- tolower(names(d_basisProm))
  names(d_basisRand) <- tolower(names(d_basisRand))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))


  # 2. VELGE VARIABLER
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros, redo, redo_times, narkose, pros_varighet, rtg_tid,
      abla_varighet, dplyr::contains("aryt_i"), dplyr::contains("sys_"),
      dplyr::contains("abla_strat"), akutt_suksess, oppsummering,
      dplyr::contains("komp_"), tscreated, tsupdated)

  d_basereg %<>% dplyr::select(mceid:forskyvning, ehra_sympt)

  d_mcePatientdata %<>%
    dplyr::select(pid, mceid) %>%
    dplyr::rename(patient_id = pid)

  d_patientlist %<>%
    dplyr::select(id, birth_date, gender, deceased, deceased_date,
                  ssn_type, ssnsubtype) %>%
    dplyr::rename(patient_id = id)


  # 3. SETTE SAMMEN PASIENT - BASEREG - PROSEDYRE - FOLLOWUP - RAND12
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
                     d_basisProm,
                     by = dplyr::join_by("mceid" == "parentmceid",
                                         "centreid" == "centreid")) %>%
    dplyr::left_join(.,
                     d_basisRand,
                     by = dplyr::join_by("mceid" == "parentmceid",
                                         "centreid" == "centreid",
                                         "mceid_followupbasis"))


  # 4. UTLEDE HJELPEVARIABLER
  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followupbasis_status)) %>%
      dplyr::pull(dato_pros)))

  # Kriterier for ePROM
  df %<>%
    ablanor::utlede_alder() %>%
    ablanor::utlede_aldersklasse() %>%
    dplyr::mutate(
      kriterie_alder = ifelse(test = alder >= 16,
                              yes = "ja",
                              no = "nei"),
      kriterie_norsk = ifelse(test = (ssn_type %in% 1), # norsk FNR
                              yes = "ja",
                              no = "nei"),

      kriterie_levende = ifelse(test = (deceased %in% 0 |
                                          (deceased %in% 1 &
                                             deceased_date > dato_pros)), # levende dagen derpå
                                yes = "ja",
                                no = "nei"),
      kriterie_tid = ifelse(test = (!is.na(pros_varighet) |
                                      !is.na(rtg_tid) |
                                      !is.na(abla_varighet)),
                            yes = "ja",
                            no = "nei"),
      dager_pros_til_created = as.numeric(difftime(as.Date(tscreated),
                                                   dato_pros,,
                                                   units = "days")),
      kriterie_registreringsdato = ifelse(test = (dager_pros_til_created <= 14),
                                          yes = "ja",
                                          no = "nei"),  #Prosedyren må registreres innen 14 dager (frist for utsending)
      kriterie_basis_midlertidig = ifelse(       # KRITERIE ALLE ENKLE SJEKKER
        test = (kriterie_alder %in% "ja" &
                  kriterie_norsk %in% "ja" &
                  kriterie_levende %in% "ja" &
                  kriterie_tid %in% "ja" &
                  kriterie_registreringsdato %in% "ja"),
        yes = "ja",
        no = "nei")) %>%
    ablanor::kriterie_basisprom_siste_aar(.,        # KRITERIE - SJEKK INGEN TIDLIGERE PROM SISTE 12MND AV SAMME FORLOPSTYPE
                                          reshId = reshId,
                                          userRole = userRole) %>%
    dplyr::mutate(
      kriterie_basis_alle = ifelse(
        test = (kriterie_basis_midlertidig %in% "ja" &
                  !kriterie_ingen_basisprom_siste12mnd %in% "nei"),
        yes = "ja",
        no = "nei"))  %>%
    ablanor::utlede_tidsvariabler() %>%
    dplyr::select(-maaned_nr_prosedyre) %>%
    dplyr::relocate(c("aar_prosedyre", "maaned_prosedyre"),
                    .after = centreid) %>%
    dplyr::arrange(mceid)



  # 5. DATAGRUNNLAG
  df %<>%
    dplyr::mutate(
      eprom_datagrunnlag_basis = factor(
        x = dplyr::case_when(

          kriterie_basis_alle %in% "ja" &
            has_basisfollowup %in% 1 ~
            "Ja",

          dato_pros < as.Date("2023-11-08", format = "%Y-%m-%d") ~
            "Nei: Før innføring av basisprom",

          kriterie_basis_alle %in% "nei" &
            is.na(has_basisfollowup) ~
            "Nei: Mangler kriterier",

          kriterie_basis_alle %in% "nei" &
            has_basisfollowup %in% 1 &
            is.na(proms_status) ~
            "Nei. Mangler kriterie (opprettet, ikke sendt)",

          kriterie_basis_alle %in% "nei" &
            has_basisfollowup %in% 1 &
            ! is.na(proms_status)~
            "Nei: Mangler kriterier, feilaktig sendt",

          kriterie_basis_alle %in% "ja" &
            is.na(has_basisfollowup) ~
            "Nei: Kriterier OK, mangler utsending (mulig etterreg./manglende sjekk av prosedyretype v. tidl. prom)",

          TRUE ~ NA_character_
        ),
        levels = c("Ja",
                   "Nei: Før innføring av basisprom",
                   "Nei: Mangler kriterier",
                   "Nei. Mangler kriterie (opprettet, ikke sendt)",
                   "Nei: Mangler kriterier, feilaktig sendt",
                   "Nei: Kriterier OK, mangler utsending (mulig etterreg./manglende sjekk av prosedyretype v. tidl. prom)",
                   NA_character_),
        ordered  = TRUE
      )
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
getBaseregProsFollowup1Data_v2 <- function(singleRow = FALSE,
                                           reshId = NULL,
                                           userRole,
                                           fromDate = NULL,
                                           toDate = NULL, ...){



  # 1 GET ALL TABELS NEEDED
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

  d_prom1 <- ablanor::getProm1yr(singleRow = FALSE,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = NULL,
                                 toDate = NULL)$d_prom1

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

  names(d_prom1) <- tolower(names(d_prom1))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))



  # 2. VELGE VARIABLER
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros,
      redo, redo_times, narkose,
      dplyr::contains("aryt_i"),
      dplyr::contains("sys_"),
      dplyr::contains("abla_strat"),
      akutt_suksess,
      oppsummering,
      dplyr::contains("komp_"), pros_varighet
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


  # 3. SETTE SAMMEN PASIENT - BASEREG - PROSEDYRE - FOLLOWUPDATA
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
                     d_prom1 %>% dplyr::rename("mceid" = parentmceid),
                     by = c("mceid", "centreid"))


  # 4. UTLEDE HJELPEVARIABLER
  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followup1_status)) %>%
      dplyr::pull(dato_pros)))

  df %<>%
    dplyr::mutate(
      dato_followup_teoretisk_1aar = dato_pros + lubridate::weeks(51)) %>%
    ablanor::utlede_alder() %>%
    ablanor::utlede_aldersklasse() %>%
    dplyr::mutate(
      kriterie_alder = ifelse(test = alder >= 16,
                              yes = "ja",
                              no = "nei"),
      kriterie_norsk = ifelse(test = (ssn_type %in% 1),
                              yes = "ja",
                              no = "nei"),
      kriterie_levende_1aar = ifelse(
        test = (deceased %in% 0 |
                  (deceased %in% 1 &
                     deceased_date > dato_followup_teoretisk_1aar)),
        yes = "ja",
        no = "nei"),

      kriterie_1yr_midlertidig = ifelse(
        test = (kriterie_alder %in% "ja" &
                  kriterie_norsk %in% "ja" &
                  kriterie_levende_1aar %in% "ja"),
        yes = "ja",
        no = "nei")) %>%
    ablanor::kriterie_1yr_prom_siste_aar(reshId = reshId,
                                         userRole = userRole)  %>%
    ablanor::kriterie_ingen_ny_pros_neste_aar(reshId = reshId,
                                              userRole = userRole) %>%
    dplyr::mutate(
      kriterie_1yr_alle = ifelse(
        test = (kriterie_1yr_midlertidig %in% "ja" &
                  !kriterie_ingen_1yr_prom_siste_aar %in% "nei" &
                  !kriterie_ingen_ny_pros %in% "nei"),
        yes = "ja",
        no = "nei")) %>%
    ablanor::utlede_tidsvariabler() %>%
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
           kriterie_1yr_alle %in% "ja" &
           !is.na(followup1_tscreated) & # Eprom er opprettet
           is.na(proms_status)  # eprom er ikke sendt
        ) ~ "teknisk problem",

        TRUE ~ "nei"),

      eprom_datagrunnlag_1aar =  dplyr::case_when(

        kriterie_1yr_alle %in% "ja" &
          eprom_kjente_feil_1aar %in% "nei" &
          !is.na(followup1_tscreated) & # Eprom er opprettet
          !is.na(proms_tssendt)  # Eprom sendt
        ~ "Ja",

        dato_pros > nyeste_eprom_bestilling ~  "Nei: Registreringen er for ny",

        dato_pros < as.Date("2020-01-01", format = "%Y-%m-%d") ~
          "Nei: Før innføring av 1års oppfølging",

        kriterie_1yr_alle %in% "ja" &
          eprom_kjente_feil_1aar %in% "nei" &
          (is.na(followup1_tscreated)| is.na(proms_tssendt))
        ~ "Nei: Kriterier OK, mangler opprettelse/utsendelse ",

        eprom_kjente_feil_1aar %in% "teknisk problem"
        ~ "Nei: Teknisk feil",

        kriterie_1yr_alle %in% "nei" &
          eprom_kjente_feil_1aar %in% "nei" &
          !is.na(followup1_tscreated) &
          !is.na(proms_tssendt)
        ~ "Nei: Mangler kriterier, feilaktig sendt",

        kriterie_1yr_alle %in% "nei" &
          eprom_kjente_feil_1aar %in% "nei" &
          (is.na(followup1_tscreated) | is.na(proms_tssendt))
        ~ "Nei: Mangler kriterie",

        TRUE ~ NA_character_
      )
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
getBaseregProsFollowup5Data_v2 <- function(singleRow = FALSE,
                                        reshId = NULL,
                                        userRole,
                                        fromDate = NULL,
                                        toDate = NULL, ...){

  # 1 GET ALL TABELS NEEDED
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

  d_prom5 <- ablanor::getProm5yr(singleRow = FALSE,
                                            reshId = reshId,
                                            userRole = userRole,
                                            fromDate = NULL,
                                            toDate = NULL)$d_prom5


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

  names(d_prom5) <- tolower(names(d_prom5))
  names(d_basereg) <- tolower(names(d_basereg))
  names(d_pros) <- tolower(names(d_pros))
  names(d_mcePatientdata) <- tolower(names(d_mcePatientdata))
  names(d_patientlist) <- tolower(names(d_patientlist))
  names(d_mce) <- tolower(names(d_mce))


  # 2. VELGE VARIABLER
  d_pros %<>%
    dplyr::select(
      mceid:dato_pros,
      redo, redo_times, narkose,
      dplyr::contains("aryt_i"),
      dplyr::contains("sys_"),
      dplyr::contains("abla_strat"),
      akutt_suksess,
      oppsummering,
      dplyr::contains("komp_"), pros_varighet
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



  # 3. SETTE SAMMEN PASIENT - BASEREG - PROSEDYRE - FOLLOWUPDATA
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
                     d_prom5 %>% dplyr::rename("mceid" = parentmceid),
                     by = c("mceid", "centreid"))



  # 4. UTLEDE HJELPEVARIABLER
  # Nyeste prosedyredato som har eprom:
  nyeste_eprom_bestilling <- lubridate::date(max(
    df %>%
      dplyr::filter(!is.na(followup5_status)) %>%
      dplyr::pull(dato_pros)))

  df %<>%
    dplyr::mutate(
      dato_followup_teoretisk_5aar = dato_pros %m+% years(5)) %>%
    ablanor::utlede_alder() %>%
    ablanor::utlede_aldersklasse() %>%
    dplyr::mutate(
      kriterie_alder = ifelse(test = alder >= 16,
                              yes = "ja",
                              no = "nei"),
      kriterie_norsk = ifelse(test = (ssn_type %in% 1),
                              yes = "ja",
                              no = "nei"),
      kriterie_levende_5aar = ifelse(
        test = (deceased %in% 0 |
                  (deceased %in% 1 &
                     deceased_date > dato_followup_teoretisk_5aar)),
        yes = "ja",
        no = "nei"),

      kriterie_5yr_midlertidig = ifelse(
        test = (kriterie_alder %in% "ja" &
                  kriterie_norsk %in% "ja" &
                  kriterie_levende_5aar %in% "ja"),
        yes = "ja",
        no = "nei")) %>%
    ablanor::kriterie_5yr_prom_siste_5aar(reshId = reshId,
                                         userRole = userRole)  %>%
    ablanor::kriterie_ingen_ny_pros_neste_5aar(reshId = reshId,
                                              userRole = userRole) %>%
    dplyr::mutate(
      kriterie_5yr_alle = ifelse(
        test = (kriterie_5yr_midlertidig %in% "ja" &
                  !kriterie_ingen_5yr_prom_siste_5aar %in% "nei" &
                  !kriterie_ingen_ny_pros_neste_5aar %in% "nei"),
        yes = "ja",
        no = "nei")) %>%
    ablanor::utlede_tidsvariabler() %>%
    dplyr::arrange(mceid) %>%

    dplyr::mutate(
      eprom_datagrunnlag_5aar =  dplyr::case_when(

        dato_pros < as.Date("2020-01-01", format = "%Y-%m-%d") ~
          "Nei: Før innføring av 5års oppfølging",

        kriterie_5yr_alle %in% "ja" &
          !is.na(followup5_tscreated) & # Eprom er opprettet
          !is.na(proms_tssendt)  # Eprom sendt
        ~ "Ja",

        dato_pros > nyeste_eprom_bestilling ~  "Nei: Registreringen er for ny",

        kriterie_5yr_alle %in% "ja" &
          (is.na(followup5_tscreated)| is.na(proms_tssendt))
        ~ "Nei: Kriterier OK, mangler opprettelse/utsendelse ",

        kriterie_5yr_alle %in% "nei" &
          !is.na(followup5_tscreated) &
          !is.na(proms_tssendt)
        ~ "Nei: Mangler kriterier, feilaktig sendt",

        kriterie_5yr_alle %in% "nei" &
          (is.na(followup5_tscreated) | is.na(proms_tssendt))
        ~ "Nei: Mangler kriterie",

        TRUE ~ NA_character_
      )
    )
  if(singleRow == TRUE) {
    # Return first row only
    df %>% dplyr::filter(dplyr::row_number() == 1)
  } else {
    # Return all
    df
  }
}




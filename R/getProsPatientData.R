#' Hent merget datasett
#' Hent datasett fra basis-skjema, prosedyreskjema, mce, og pasientskjema. Merge
#' sammen, filtrere på forløp der prosedyre er utført. Legge til utledete
#' variabler og nye variabler.
#' @param registryName Dersom verdien "test_ablanor_lokalt" leser vi inn
#' lokal RData-fil. Ellers er det SQL spørring
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param tekstVars legge til tekstvariabler hentet fra kodebok for kategoriske
#' variabler eller fra tabeller med tekst/labels for kategoriske variabler
#' @param ... Optional arguments to be passed to the function
#'
#' @return data.frame med rad per forløp og kolonner for variabler
#' @export

getProsPatientData <- function(registryName,
                               singleRow = FALSE,
                               tekstVars = FALSE,
                               ...) {
  . <- ""

  if (registryName == "test_ablanor_lokalt") {
    # LASTE INN DATA LOKALT
    load(file = Sys.getenv("filbane_ablanor_test"))

  } else {

    dbType <- "mysql"
    ## SQL SPØRRING :
    # SPM ARE : Hva heter databasen til Basereg? Basisregisteret.
    # BLIR DETTE RIKTIG ?
    query_basereg <- "
    SELECT
      *
    FROM
      basreg
    "

    # SPM ARE : Hva heter databasen til Prosedyrene?  BLIR DETTE RIKTIG ?
    query_procedure <- "
    SELECT
      *
    FROM
      pros
    "
    # SPM ARE : Hva heter databasen til MCE?  BLIR DETTE RIKTIG ?
    query_mce <- "
    SELECT
      MCEID,
      PATIENT_ID,
      STATUS
    FROM
      mce
    # "

    # SPM ARE : Hva heter databasen til PATIENTLIST?  BLIR DETTE RIKTIG ?
    query_patientlist <- "
    SELECT
      *
    FROM
      patientlist
    "

    if(singleRow) {
      msg_basereg <- "Query metadata for merged dataset, basereg"
      msg_procedure <- "Query metadata for merged dataset, procedure"
      msg_mce <- "Query metadata for merged dataset, mce"
      msg_patientlist <- "Query metadata for merged dataset, patientlist"

      query_basereg <- paste0(query_basereg, "\nLIMIT\n  1;")
      query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
      query_mce <- paste0(query_mce, "\nLIMIT\n  1;")
      query_patientlist <- paste0(query_patientlist, "\nLIMIT\n  1;")
    } else {
      msg_basereg <- "Query data for merged dataset, basereg"
      msg_procedure <- "Query data for merged dataset, procedure"
      msg_mce <- "Query data for merged dataset, mce"
      msg_patientlist <- "Query data for merged dataset, patientlist"

      query_basereg <- paste0(query_basereg, ";")
      query_procedure <- paste0(query_procedure, ";")
      query_mce <- paste0(query_mce, ";")
      query_patientlist <- paste0(query_patientlist, ";")
    }

    # ARE : Hva er denne koden til? Tilpasse til 4 spørringer ?
    # SVAR: For logging hver gang det hentes ut data. Kan her gjøres felles for
    # alle spørringer eller for hver enkelt spørring (som i dette tilfellet).
    # NB Skjer bare når session sendes inn til funksjonen som et vilkårlig
    # argument (...)
    if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_basereg)
      d_basereg <- rapbase::loadRegData(registryName, query_basereg, dbType)
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_procedure)
      d_pros <- rapbase::loadRegData(registryName, query_procedure, dbType)
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_mce)
      d_mce <- rapbase::loadRegData(registryName, query_mce, dbType)
      rapbase::repLogger(session = list(...)[["session"]],
                         msg = msg_patientlist)
      d_patientlist <- rapbase::loadRegData(registryName, query_patientlist,
                                            dbType)
    } else {
      d_basereg <- rapbase::loadRegData(registryName, query_basereg, dbType)
      d_pros <- rapbase::loadRegData(registryName, query_procedure, dbType)
      d_mce <- rapbase::loadRegData(registryName, query_mce, dbType)
      d_patientlist <- rapbase::loadRegData(registryName, query_patientlist, dbType)
    }
  }




  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TO TABELLER (status for skjema etc)
  # intersect(names(d_pros), names(d_basereg)) # samme variabel-navn.
  # Vi angir en prefix for å få med variablene fra begge tabellene
  # KRISTINA: variabelnavnene i databasen er stort sett CAPS så da må nok koden
  # under oppdateres
  d_basereg %<>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby),
                     function(x) {
                       paste0("basereg_", x)
                     })
  d_pros %<>%
    dplyr::rename_at(dplyr::vars(.data$usercomment:.data$createdby),
                     function(x) {
                       paste0("pros_", x)
                     })



  # MERGE DATASETTENE :
  # NB: I Ablanor skal berre skjema som høyrer til forløp som har resultert i ein
  # prosedyre (eventuelt ein avbroten ein) analyserast. Oppføringar for andre
  # forløp vert filtrerte vekk. Viss ein person for eksempel berre har eit
  # basisskjema men ikkje (enno) eit prosedyreskjema, vil personen også vera
  # filtrert vekk frå basisskjema-datsettet (og forløpsdatasettet,
  # pasientdatasettet og andre datasett).
  # Her brukar me left_join, for å sikre at berre forløpsid der prosedyre
  # finst vert tekne med.

  # NB: variablene aryt_i* er duplikert i basereg datasettet, derfor fjernes de.
  d_ablanor <- d_pros %>%
    dplyr::left_join(., d_mce %>% dplyr::select(.data$mceid,
                                                .data$patient_id,
                                                .data$status),
                     by = "mceid") %>%
    dplyr::left_join(., d_patientlist,
                     by = c("patient_id" = "id")) %>%
    dplyr::left_join(., d_basereg %>%
                       dplyr::select(!tidyselect::starts_with("aryt_i"),
                                     -.data$dato_pros),
                     by = c("mceid", "centreid"))

  # Sjekk at ingen variabel-navn teller dobbelt.
  # TEST I getLocalProsedyrePasientData
  # d_ablanor %>% dplyr::select(ends_with(".x") | ends_with(".y"))




  # UTLEDETE VARIABLER

  # ALDER :
  d_ablanor %<>%
    dplyr::mutate(alder = lubridate::as.period(
      lubridate::interval(
        start = .data$birth_date, end = .data$dato_pros),
      unit = "years")$year)

  d_ablanor %<>%
    dplyr::mutate(alder_75 = ifelse(.data$alder >= 75,
                                    yes = ">=75",
                                    no = "<75"))


  # BMI: (Mangler verdier i variabelem bmi for noen pasienter.
  #       Bruker vekt og høyde for å generere denne variabelen på nytt)
  d_ablanor %<>%
    dplyr::mutate(
      bmi_manual = round(.data$vekt / (.data$hoyde / 100) ^ 2, 1),
      bmi_category_manual =
        factor(dplyr::case_when(
          .data$bmi_manual <= 16 ~ "Alvorlig undervekt",
          .data$bmi_manual > 16 & .data$bmi_manual <= 17 ~ "Moderat undervekt",
          .data$bmi_manual > 17 & .data$bmi_manual <= 18.5 ~ "Mild undervekt",
          .data$bmi_manual > 18.5 & .data$bmi_manual < 25 ~ "Normal",
          .data$bmi_manual >= 25 & .data$bmi_manual < 30 ~ "Overvekt",
          .data$bmi_manual >= 30 & .data$bmi_manual < 35 ~
            "Moderat fedme, klasse I",
          .data$bmi_manual >= 35 & .data$bmi_manual < 40 ~ "Fedme, klasse II",
          .data$bmi_manual >= 40 ~ "Fedme, klasse III"),
          levels = c("Alvorlig undervekt",
                     "Moderat undervekt",
                     "Mild undervekt",
                     "Normal",
                     "Overvekt",
                     "Moderat fedme, klasse I",
                     "Fedme, klasse II",
                     "Fedme, klasse III")),
      bmi_over35 = ifelse(.data$bmi_manual >= 35,
                          yes = "BMI >=35",
                          no = "BMI <35")
    )

  # Kontroll 1 : Grense for normal/Mild undervekt går på 18,5.
  # Anbefaler å bruke bmi_category_manual i stedet for bmi_category.
  # Kan man slette bmi_categry og erstatte med bmi_category_manual ?

  # d_ablanor %>%  filter(bmi_category_manual!= bmi_category) %>%
  # select(bmi, bmi_manual, vekt, hoyde, bmi_category, bmi_category_manual)

  # Kontroll 2 : Mangler BMI for noen pasienter, dette skal bli rettet opp
  # i ny versjon av OpenQReg. Må testes
  # d_ablanor  %>%  filter(is.na(bmi)) %>%
  # select(bmi, bmi_manual, bmi_numeric,vekt, hoyde)



  # UKE, MÅNED, ÅR
  d_ablanor %<>%
    dplyr::mutate(
      year = as.ordered(lubridate::year(.data$dato_pros)),
      aar = .data$year,
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$dato_pros))),
      maaned = as.ordered(paste0(.data$year, "-", .data$maaned_nr))
    )


  # AFLI : ICD
  d_ablanor %<>%
    dplyr::mutate(
      kategori_afli_aryt_i48 =
        factor(dplyr::case_when(
          .data$forlopstype == 1 & .data$aryt_i48_0 == TRUE ~
            "AFLI-ICD 48.0 Paroksymal atrieflimmer",
          .data$forlopstype == 1 &
            .data$aryt_i48_1 == TRUE &
            .data$aryt_i48_1_underkat == 1 ~
            "AFLI-ICD 48.1 Persisterende atrieflimmer",
          .data$forlopstype == 1 &
            .data$aryt_i48_1 == TRUE &
            .data$aryt_i48_1_underkat == 2 ~
            "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer"),
          levels = c("AFLI-ICD 48.0 Paroksymal atrieflimmer",
                     "AFLI-ICD 48.1 Persisterende atrieflimmer",
                     "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer")))


  # VT : KARDIOMYOPATI
  d_ablanor %<>%
    dplyr::mutate(
      kategori_vt_kardiomyopati =
        factor(dplyr::case_when(
          .data$forlopstype == 2 & .data$ kardiomyopati == 0
          ~ "Uten kardiomyopati",
          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            .data$type_kardiomyopati == 1
          ~ "Iskemisk KM (ICM)",
          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            .data$type_kardiomyopati == 2
          ~ "Dilatert KM (DCM)",
          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            !(.data$type_kardiomyopati %in% 1:2)
          ~ "Annen KM",
          .data$forlopstype == 2 &
            .data$kardiomyopati == 9
          ~ "Ukjent om kardiomyopati"),
          levels = c("Uten kardiomyopati",
                     "Iskemisk KM (ICM)",
                     "Dilatert KM (DCM)",
                     "Annen KM",
                     "Ukjent om kardiomyopati")))



  # HJERTESVIKT OG REDUSERT EF
  d_ablanor %<>%
    dplyr::mutate(
      kategori_afli_hjsviktEF = dplyr::case_when(
        .data$forlopstype ==  1 &
          (.data$hjertesvikt == 1 | .data$ejekfrak %in% 2:3) ~
          "AFLI-Hjertesvikt eller redusert EF",
        .data$forlopstype ==  1 &
          !(.data$hjertesvikt == 1 | .data$ejekfrak %in% 2:3) ~
          "AFLI-Verken hjertesvikt eller redusert EF"))




  if (tekstVars ==TRUE) {
    # LEGGE TIL VARIABLER I TEKST FORMAT FOR KATEGORISEK VARIABLER
    # (Bare for pivot table, ikke for månedsrapport)
    # Her må vi komme frem til en løsning. Kan vi bruke tekst-variablene direkte?
    # Evt merge tekst-variablene og de numeriske for å ha begge variantene for
    # noen variabler ? Vi må ha de numeriske for å bruke samme kode for å
    # utlede variabler.
  }
  d_ablanor
}

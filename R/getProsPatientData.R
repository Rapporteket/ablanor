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

  d <- getProsPatient(registryName, singleRow, ...)

  d_basereg <- d$basereg
  d_pros <- d$pros
  d_mce <- d$mce
  d_patientlist <- d$patientlist

  ## BEHANDLING AV DATABASEN I R:
  # FELLES VARIABEL-NAVN I TO TABELLER (status for skjema etc)
  # intersect(names(d_pros), names(d_basereg)) # samme variabel-navn.
  # Vi angir en prefix for å få med variablene fra begge tabellene
  # KRISTINA: variabelnavnene i databasen er stort sett CAPS så da må nok koden
  # under oppdateres (jeg har gjort litt, men ikke ferdig...)
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
    dplyr::left_join(., d_mce %>% dplyr::select(.data$MCEID,
                                                .data$PATIENT_ID,
                                                .data$STATUS),
                     by = "MCEID") %>%
    dplyr::left_join(., d_patientlist,
                     by = c("PATIENT_ID" = "ID")) %>%
    # Hm, det finnes hverken ARYT_I* vaiabler eller DATO_PROS i basreg-tabellen
    # Utkommentert kode er erstattet av påfølgende line
    # KRISTINA: sjekk!
    #dplyr::left_join(., d_basereg %>%
    #                   dplyr::select(!tidyselect::starts_with("aryt_i"),
    #                                 -.data$DATO_PROS),
    #                 by = c("MCEID", "CENTREID"))
    dplyr::left_join(., d_basereg, by = c("MCEID", "CENTREID"))

  # Sjekk at ingen variabel-navn teller dobbelt.
  # TEST I getLocalProsedyrePasientData
  # d_ablanor %>% dplyr::select(ends_with(".x") | ends_with(".y"))




  # UTLEDETE VARIABLER

  # ALDER :
  d_ablanor %<>%
    dplyr::mutate(alder = lubridate::as.period(
      lubridate::interval(
        start = .data$BIRTH_DATE, end = .data$DATO_PROS),
      unit = "years")$year)

  d_ablanor %<>%
    dplyr::mutate(alder_75 = ifelse(.data$alder >= 75,
                                    yes = ">=75",
                                    no = "<75"))


  # BMI: (Mangler verdier i variabelem bmi for noen pasienter.
  #       Bruker vekt og høyde for å generere denne variabelen på nytt)
  d_ablanor %<>%
    dplyr::mutate(
      bmi_manual = round(.data$VEKT / (.data$HOYDE / 100) ^ 2, 1),
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
      year = as.ordered(lubridate::year(.data$DATO_PROS)),
      aar = .data$year,
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$DATO_PROS))),
      maaned = as.ordered(paste0(.data$year, "-", .data$maaned_nr))
    )


  # AFLI : ICD
  d_ablanor %<>%
    dplyr::mutate(
      kategori_afli_aryt_i48 =
        factor(dplyr::case_when(
          .data$FORLOPSTYPE == 1 & .data$ARYT_I48_0 == TRUE ~
            "AFLI-ICD 48.0 Paroksymal atrieflimmer",
          .data$FORLOPSTYPE == 1 &
            .data$ARYT_I48_1 == TRUE &
            .data$ARYT_I48_1_UNDERKAT == 1 ~
            "AFLI-ICD 48.1 Persisterende atrieflimmer",
          .data$FORLOPSTYPE == 1 &
            .data$ARYT_I48_1 == TRUE &
            .data$ARYT_I48_1_UNDERKAT == 2 ~
            "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer"),
          levels = c("AFLI-ICD 48.0 Paroksymal atrieflimmer",
                     "AFLI-ICD 48.1 Persisterende atrieflimmer",
                     "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer")))


  # VT : KARDIOMYOPATI
  d_ablanor %<>%
    dplyr::mutate(
      kategori_vt_kardiomyopati =
        factor(dplyr::case_when(
          .data$FORLOPSTYPE == 2 & .data$KARDIOMYOPATI == 0
          ~ "Uten kardiomyopati",
          .data$FORLOPSTYPE == 2 &
            .data$KARDIOMYOPATI == 1 &
            .data$TYPE_KARDIOMYOPATI == 1
          ~ "Iskemisk KM (ICM)",
          .data$FORLOPSTYPE == 2 &
            .data$KARDIOMYOPATI == 1 &
            .data$TYPE_KARDIOMYOPATI == 2
          ~ "Dilatert KM (DCM)",
          .data$FORLOPSTYPE == 2 &
            .data$KARDIOMYOPATI == 1 &
            !(.data$TYPE_KARDIOMYOPATI %in% 1:2)
          ~ "Annen KM",
          .data$FORLOPSTYPE == 2 &
            .data$KARDIOMYOPATI == 9
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
        .data$FORLOPSTYPE ==  1 &
          (.data$HJERTESVIKT == 1 | .data$EJEKFRAK %in% 2:3) ~
          "AFLI-Hjertesvikt eller redusert EF",
        .data$FORLOPSTYPE ==  1 &
          !(.data$HJERTESVIKT == 1 | .data$EJEKFRAK %in% 2:3) ~
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

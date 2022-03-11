#' Kvalitetsindikatorer for AblaNor
#'
#' Oppdaterte versjoner av kvalitetsindikatorer. I bruk fra og med våren 2022.
#'
#' For hver av kvalitetsindikatorene, legge til en variabel for datagrunnlag
#' (suffix 'data') og en for indikatoren ja/nei.
#'
#' __Tamponade i forbindelse med prosedyren__
#' \code{indik_tamponade()}
#' \itemize{
#' \item nevneren \code{indik_tamponade_data} (datagrunnlag) har verdien \emph{ja} dersom
#' forløpstype er AFLI (\code{forlopstype} = 1)
#' uten AV-knuter (\code{abla_strat_av_his} = 0).
#' \item telleren \code{indik_tamponade} har verdien \emph{ja} dersom
#'  \code{indik_tamp_data} = \emph{ja} og \code{komp_tamp} = 1,
#'   verdien \emph{nei} dersom \code{indik_tamp_data} = \emph{ja} og \code{komp_tamp} = 0,
#'  verdien \emph{manglende} dersom \code{indik_tamp_data} = \emph{ja} og
#'   \code{komp_tamp} er manglende,
#'   og verdien \emph{NA} dersom forløpet ikke er i datagrunnlaget (\code{indik_tamp_data} = \emph{nei}).
#' }
#'
#' __Prosedyreavbrudd på grunn av tekniske problemer eller komplikasjoner__
#' \code{indik_avbrudd()}
#' \itemize{
#' \item nevneren \code{indik_avbrudd_data} (datagrunnlag) har
#' verdien \emph{ja} dersom forløpstype ikke er manglende.
#' \item telleren \code{indik_avbrudd} har verdien \emph{ja} dersom
#'  \code{indik_avbrudd_data} = \emph{ja} og
#'  \code{abla_strat_ingen_arsak} = 4(tekniske problemer) eller 5(Komplikasjon),
#'   verdien \emph{nei} dersom \code{indik_avbrudd_data} = \emph{ja} og
#'   ingen avbrudd eller avbrudd av andre årsaker.
#'  Verdien \emph{NA} dersom forløpet ikke er i datagrunnlaget.
#'  }
#'
#' __Overlevelse 30 dager etter prosedyren__
#' \code{indik_overlevelse30dg}
#' \itemize{
#'
#' \item nevneren \code{indik_overlevelse30dg_data} (datagrunnlag) har verdien
#' \emph{ja} dersom forløpstype er AFLI (\code{forlopstype} = 1)
#' uten AV-knuter (\code{abla_strat_av_his} = 0) og dersom tid til sensur er
#' over 30 dager (\code{dager_pros_sensur_gyldig} = \emph{ja]}). Variabelen har
#'  verdi \emph{nei} for andre forløpstyper, for kort sensur-tid. Dersom
#'  flere enn et forløp (AFLI, uten AV knuter, med gyldig tid) i et 30-dagers
#'  intervall, brukes kun nyeste forløp og alle eldre forløp har verdi
#'  \code{indik_overlevelse_30dg} = \emph{nei}.
#'
#' \item telleren \code{indik_overlevelse30dg} har verdien \emph{ja} dersom
#' pasienten er levende 30 dager etter prosedyren, var verdien \emph{nei}
#' dersom pasienten er død 0-29 dager etter prosedyren.
#' }
#'
#' \code{indik_overlevelse30dg} bruker hjelpe-funksjonen
#' \code{utlede_dager_sensur} som lager to nye variabler
#' \code{dager_pros_sensur} og \code{dager_pros_sensur_gyldig}.
#' \code{indik_overlevelse30dg} inneholder antall dager fra proseyre til
#' dødsdato for avdøde pasienter og antall dager fra proseydre til sensurdato
#' (dato for nedlastet datadump) for levende pasienter.
#' \code{indik_overlevelse30dg_gyldig} har verdien nei/manglende dersom for
#' kort sensur-tid eller datoer er manglende.
#'
#' __Klinisk effekt 12 måneder etter prosedyren__
#' \code{indik_prom_klineff}
#' \itemize{
#'
#' \item nevneren \code{indik_prom_klineff_data} (datagrunnlag) har verdien
#' \emph{ja} dersom forløpstype er AFLI (\code{forlopstype} = 1)
#' uten AV-knuter (\code{abla_strat_av_his} = 0) og dersom  oppfølgingsskjema
#' er utfylt. Variabelen har
#'  verdi \emph{nei} for andre forløpstyper eller manglende oppfølging.
#'
#' \item telleren \code{indik_prom_klineff} har verdien \emph{ja} dersom
#' pasienten har svart \emph{Helt bra}, \emph{Mye bedre} eller \emph{Bedre}
#' sammenlignet med før prosedyre. Variabelen har verdien  \emph{nei} dersom
#' pasienten svarer \emph{Uforandret} eller \emph{Verre}. Variabelen har verdien
#' \emph{manglende} dersom oppfølgingsskjemaet er fylt ut, men spørsmålet om
#' klinisk effekt er ubesvart.
#' }
#'
#' __Behov for pacemaker__
#' \code{indik_pacemaker}
#' \itemize{
#'
#' \item nevneren \code{indik_pacemaker_data} (datagrunnlag) har verdien
#' \emph{ja} dersom forløpstype er SVT (\code{forlopstype} = 3)
#' uten AV-knuter (\code{abla_strat_av_his} = 0). Variabelen har
#'  verdi \emph{nei} for andre forløpstyper.
#'
#' \item telleren \code{indik_pacemaker} har verdien \emph{ja} dersom
#' pasienten har hatt komplikasjon AV-blokk etterfulgt av innsetting av
#' pacemaker (\code{komp_avblokk_pm} = 1). Variabelen har verdien  \emph{nei}
#' eller og verdien \emph{manglende} dersom \code{komp_avblokk_pm} mangler.
#' }
#'
#' __Vellykket prosedyre (akutt suksess)__
#' \code{indik_akuttsuksess}
#' \itemize{
#'
#' \item nevneren \code{indik_akuttsuksess_data} (datagrunnlag) har verdiene
#' \emph{AFLI}, \emph{VT}, \emph{AVRT} eller \emph{AVNRT} avhenging av
#' forløpstype (forlopstype og aryt_i47_1_underkat) og kun dersom pasienten er
#' abladert (\code{abla_strat_ingen} =0) og uten AV-knuter
#' (\code{abla_strat_av_his} = 0).  Variabelen har
#'  verdi \emph{nei} for andre forløpstyper, ikke abladert, eller AV-knuter.
#'
#' \item telleren \code{indik_akuttsuksess} har verdien \emph{ja} dersom
#' \code{akutt_suksess} = 1.
#'  Variabelen har verdien  \emph{nei} dersom \code{akutt_suksess} = 0 eller 2,
#'  og verdien \emph{manglende} dersom \code{akutt_suksess} mangler.
#' }
#'
#'
#' @param df data.frame med ablanor-data. Må inneholde ulike variabler for de
#' ulike funksjonene.  F.eks. \code{forlopstype}, \code{abla_strat_av_his} og
#' \code{komp_tamp} for indikatoren "Komplikasjon tamponade for AFLI uten AV
#' knuter" .
#'
#' @name utlede_kvalitetsindikatorer
#' @aliases
#' indik_tamponade
#' indik_avbrudd
#' utlede_dager_sensur
#' indik_overlevelse30dg
#' indik_prom_klineff
#' indik_pacemaker
#' indik_akuttsuksess
#'
#' @examples
#' # TAMPONADE
#'  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1),
#'                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 0, 0),
#'                   komp_tamp = c(rep(0, 6), 1, 1))
#' ablanor::indik_tamponade(df = df)
#'
#'# AVBRUDD
#'  df <- data.frame(forlopstype = c(3, 4, NA, 1, 1, 1, 1, 1, 1, 1, 1),
#'                   abla_strat_av_his = c(1, 0, 0, 1, NA, 0, 0, 0, 0, 0, 0),
#'                   abla_strat_ingen = c(rep(0, 5), NA,  1, 1,1, 1, 0),
#'                   abla_strat_ingen_arsak = c(rep(NA, 6), 1, 4,5, NA, NA))
#'  ablanor::indik_avbrudd(df = df)
#'
#' # KLINISK EFFEKT
#' df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1),
#'                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 0, 0),
#'                   followup_status = c(0, 0, 0, 1, 1, 1, 1, 1),
#'                   q2 = c(NA, NA, NA, 1:5))
#' ablanor::indik_prom_klineff(df = df)
#'
#'
#'#PACEMAKERBEHOV
#' df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 3, 3, 3),
#'                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 0, 0),
#'                   komp_avblokk_pm = c(NA, NA, NA, 0, 1, 0, 1, 0))
#' ablanor::indik_pacemaker(df = df)
#'
#' # AKUTT SUKSESS
#' df <- data.frame(
#'    abla_strat_ingen = c(1, NA, rep(0, 18)),
#'    abla_strat_av_his = c(0, 0, 1, NA,  rep(0, 16)),
#'    forlopstype = c(rep(1, 4), NA, rep(1, 5), rep(2, 3), rep(3, 6), 4),
#'    aryt_i47_1_underkat = c(rep(NA, 13), NA, 1:5, NA),
#'    akutt_suksess = c(rep(NA, 5), NA, 9, 0, 1, 2, 0:2, 0:2, 0:2 , 1))
#' ablanor::indik_akuttsuksess(df)
#'
#' # OVERLEVELSE
#' data.frame(
#'     patient_id = rep(1, 3),
#'     forlopstype = rep(1, 3),
#'     abla_strat_av_his = rep(0, 3),
#'     dato_pros = as.Date(c(rep("2020-10-15",2),"2021-10-15"),
#'                         format = "%Y-%m-%d"),
#'     deceased = c(0, 1, 0),
#'     deceased_date = as.Date(c(NA, "2020-10-18", NA),
#'                         format = "%Y-%m-%d")) %>%
#' ablanor::utlede_dager_sensur(
#'     df=.,
#'     dato_sensur = as.Date("2021-10-20", format = "%Y-%m-%d")) %>%
#' ablanor::indik_overlevelse30dg()



NULL

#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_tamponade <- function(df) {
  stopifnot(c("forlopstype",
              "abla_strat_av_his",
              "komp_tamp") %in% names(df))

  df %>%
    dplyr::mutate(
      indik_tamp_data = dplyr::if_else(
        condition = (.data$forlopstype %in% 1  &
                       .data$abla_strat_av_his %in% 0),
        true = "ja",
        false = "nei",
        missing = "nei"),

      indik_tamp = dplyr::case_when(
        .data$indik_tamp_data %in% "ja" &
          .data$komp_tamp %in% 1  &
          !is.na(.data$komp_tamp) ~ "ja",

        .data$indik_tamp_data %in% "ja" &
          .data$komp_tamp %in% 0 &
          !is.na(.data$komp_tamp) ~ "nei",

        .data$indik_tamp_data %in% "ja" &
          is.na(.data$komp_tamp) ~ "manglende",

        .data$indik_tamp_data %in% "nei" ~ NA_character_,

        TRUE ~ NA_character_))

}



#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_avbrudd <- function(df){


  stopifnot(c("forlopstype",
              "abla_strat_av_his",
              "abla_strat_ingen",
              "abla_strat_ingen_arsak") %in% names(df))



  df %>% dplyr::mutate(
    indik_avbrudd_data = ifelse(
      test = (!is.na(.data$abla_strat_ingen) &
                .data$forlopstype %in% 1 &
                .data$abla_strat_av_his %in% 0),
      yes = "ja",
      no = "nei"),

    indik_avbrudd = dplyr::case_when(

      # dersom avbrudd på grunn av komplikasjoner blir det "ja"
      .data$indik_avbrudd_data == "ja" &
        .data$abla_strat_ingen == 1 &
        .data$abla_strat_ingen_arsak %in% 4:5 ~ "ja",

      # ingen avbrudd eller avbrudd av andre årsaker er "nei"
      .data$indik_avbrudd_data == "ja" &
        .data$abla_strat_ingen == 1 &
        .data$abla_strat_ingen_arsak %in% c(1, 2, 3, 9) ~ "nei",

      .data$indik_avbrudd_data == "ja" &
        .data$abla_strat_ingen == 0 ~ "nei",

      .data$indik_avbrudd_data == "nei" ~ NA_character_,

      TRUE ~ NA_character_))
}



#' @rdname utlede_kvalitetsindikatorer
#' @export
utlede_dager_sensur <- function(df, dato_sensur) {


  stopifnot(c("dato_pros", "deceased", "deceased_date") %in% names(df))


  df %>% dplyr::mutate(

    # FOR ALLE: Antall dager mellom prosedyre og sensur/død
    dager_pros_sensur = dplyr::case_when(
      # For avdøde:
      .data$deceased == 1 ~ as.numeric(difftime(deceased_date,
                                                .data$dato_pros,
                                                units = "days")),
      # For levende
      .data$deceased == 0 ~ as.numeric(difftime(dato_sensur,
                                                .data$dato_pros,
                                                units = "days")),
      TRUE ~ NA_real_),


    # FOR ALLE: er dager_pros_sensur gyldig (ja, nei, manglende) ?
    dager_pros_sensur_gyldig = dplyr::case_when(

      # alle døde, som har datoer, er med
      .data$deceased == 1 &
        !is.na(.data$dato_pros) &
        !is.na(.data$deceased_date) ~ "ja",


      # døde, som manger datoer, er ikke med
      .data$deceased == 1 &
        (is.na(.data$dato_pros) | is.na(.data$deceased_date)) ~ "manglende",


      # levende med lang nok sensur er med
      .data$deceased == 0 &
        !is.na(.data$dato_pros) &
        .data$dager_pros_sensur >= 30 ~ "ja",

      # levende med for kort sensur er ikke med
      .data$deceased == 0 &
        !is.na(.data$dato_pros) &
        .data$dager_pros_sensur < 30  ~ "nei",

      # levende manglende dato for prosedyre er ikke med
      .data$deceased == 0 &
        is.na(.data$dato_pros) ~ "manglende",

      TRUE ~ NA_character_))

}


#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_overlevelse30dg <- function(df) {

  stopifnot(c("forlopstype", "abla_strat_av_his",
              "dager_pros_sensur", "dager_pros_sensur_gyldig",
              "dato_pros", "deceased",
              "patient_id") %in% names(df))


  # Hjelpevariabel: Filter på riktig forløpstype
  # (vi ser kun på 30-dagers intervall for disse)
  df %>% dplyr::mutate(
    utvalgt = dplyr::case_when(
      # Kun AFLI uten AV-knuter, med gyldig overlevelses-tid:
      .data$forlopstype %in% 1 &
        .data$abla_strat_av_his %in% 0 &
        .data$dager_pros_sensur_gyldig %in% "ja" ~ "ja",
      TRUE ~ "nei")) %>%



    # Dager mellom forløpene, Kun dersom utvalgt er "ja" at dette er aktuelt
    dplyr::group_by(.data$patient_id, .data$utvalgt) %>%
    dplyr::arrange(.data$dato_pros) %>%
    dplyr::mutate(

      # Antall dager mellom forløpene, lead og lag
      time.diff_lag = as.numeric(difftime(.data$dato_pros,
                                          lag(.data$dato_pros),
                                          unit = 'days')),
      time.diff_lead = -1 * as.numeric(difftime(.data$dato_pros,
                                                lead(.data$dato_pros),
                                                unit = 'days')),

      # For pasienter med >1 forløp innen et 30 dagers intervall,
      # teller kun det SISTE forløpet:
      indik_overlevelse30dg_data = dplyr::case_when(

        # INGEN DOBLE FORLØP (Kun et forløp)
        is.na(.data$time.diff_lead) &
          is.na(.data$time.diff_lag) &
          .data$utvalgt == "ja" ~ "ja",

        # ENESTE FORLØP I INTERVALLET
        (.data$time.diff_lag >= 31 | is.na(.data$time.diff_lag)  ) &
          (.data$time.diff_lead >= 31 | is.na(.data$time.diff_lead)) &
          .data$utvalgt == "ja" ~ "ja",

        # FLERE FORLØP, DETTE ER DET SISTE
        (.data$time.diff_lead >= 31 |is.na(.data$time.diff_lead)) &
          .data$time.diff_lag < 31 &
          .data$utvalgt == "ja" ~ "ja",

        # FLERE FORLØP, DETTE ER IKKE DET SISTE
        .data$time.diff_lead < 31 &
          .data$utvalgt == "ja" ~ "nei",

        # IKKE I DATAGRUNNLAGET
        .data$utvalgt == "nei" ~ "nei",

        TRUE ~ NA_character_ )) %>%
    dplyr::ungroup() %>%


    # Indikator ja/nei
    dplyr::mutate(
      indik_overlevelse30dg  = factor(
        x = dplyr::case_when(
          #ikke død
          .data$indik_overlevelse30dg_data == "ja" &
            .data$deceased == 0 ~ "ja",

          #død etter 30 dager
          .data$indik_overlevelse30dg_data == "ja" &
            .data$deceased == 1 &
            .data$dager_pros_sensur >= 30 ~ "ja",

          # død 0-29 dager
          .data$indik_overlevelse30dg_data == "ja" &
            .data$deceased == 1 &
            .data$dager_pros_sensur < 30 ~ "nei",

          #ikke i datagrunnlaget
          .data$indik_overlevelse30dg_data == "nei" ~ NA_character_),

        levels = c("ja", "nei"),
        labels = c("ja", "nei"),
        ordered = TRUE)) %>%

    dplyr::select(- .data$utvalgt, -.data$time.diff_lag, -.data$time.diff_lead)

}


#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_prom_klineff <- function(df){


  stopifnot(c("forlopstype",
              "abla_strat_av_his",
              "followup_status",
              "q2") %in% names(df))



  df %>% dplyr::mutate(

    # INDIKATOR: OPPFØLGING ETTER 12 MND
    # klinisk forbedring, kun AFLI uten HIS
    indik_prom_klineff_data = dplyr::if_else(
      condition = (.data$followup_status %in% c(-1, 0, 1) &
                     .data$forlopstype %in% 1 &
                     .data$abla_strat_av_his %in% 0),
      true = "ja",
      false = "nei",
      missing = "nei"),

    indik_prom_klineff = dplyr::case_when(
      .data$indik_prom_klineff_data == "ja" & .data$q2 %in% 1:3 ~"ja",
      .data$indik_prom_klineff_data == "ja" & .data$q2 %in% 4:5 ~ "nei",
      .data$indik_prom_klineff_data == "ja" & is.na(.data$q2) ~ "manglende",
      .data$indik_prom_klineff_data == "nei" ~ NA_character_,
      TRUE ~ NA_character_))

}



#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_pacemaker <- function(df){

  stopifnot(c("forlopstype",
              "abla_strat_av_his",
              "komp_avblokk_pm") %in% names(df))



  df %>% dplyr::mutate(
    indik_pacemaker_data = ifelse(
      test = (.data$forlopstype %in% 3 &
                .data$abla_strat_av_his %in% 0),
      yes = "ja",
      no = "nei"),

    indik_pacemaker = dplyr::case_when(

      .data$indik_pacemaker_data == "ja" &
        .data$komp_avblokk_pm %in% 1  ~ "ja",

      .data$indik_pacemaker_data == "ja" &
        .data$komp_avblokk_pm %in% 0  ~ "nei",

      .data$indik_pacemaker_data == "ja" &
        is.na(.data$komp_avblokk_pm) ~ "manglende",

      .data$indik_pacemaker_data == "nei" ~ NA_character_,

      TRUE ~ NA_character_))

}


#' @rdname utlede_kvalitetsindikatorer
#' @export
indik_akuttsuksess <- function(df){

  stopifnot(all(c("forlopstype",
                  "abla_strat_av_his",
                  "abla_strat_ingen",
                  "aryt_i47_1_underkat",
                  "akutt_suksess") %in% names(df)))


  df %>% dplyr::mutate(

    indik_akuttsuksess_data = factor(
      x = dplyr::case_when(

        # datagrunnlag: abladerte, AFLI
        .data$abla_strat_ingen %in% 0 &
          .data$abla_strat_av_his %in% 0 &
          .data$forlopstype %in% 1 ~ "AFLI",


        # datagrunnlag: abladerte, VT
        .data$abla_strat_ingen %in% 0 &
          .data$abla_strat_av_his %in% 0 &
          .data$forlopstype %in% 2 ~ "VT",


        # datagrunnlag: abladerte, SVT - AVRT
        .data$abla_strat_ingen %in% 0 &
          .data$abla_strat_av_his %in% 0 &
          .data$forlopstype %in% 3 &
          .data$aryt_i47_1_underkat %in% 4 ~ "AVRT",

        # datagrunnlag: abladerte, SVT - AVNRT
        .data$abla_strat_ingen %in% 0 &
          .data$abla_strat_av_his %in% 0 &
          .data$forlopstype %in% 3 &
          .data$aryt_i47_1_underkat %in% 1:2 ~ "AVNRT",

        TRUE ~ "nei"),
      levels = c("AFLI",
                 "VT",
                 "AVRT",
                 "AVNRT",
                 "nei"),
      labels = c("AFLI",
                 "VT",
                 "AVRT",
                 "AVNRT",
                 "nei"),
      ordered =TRUE),



    indik_akuttsuksess = dplyr::case_when(
      ! .data$indik_akuttsuksess_data %in% "nei" &
        .data$akutt_suksess %in% 1 ~ "ja",

      ! .data$indik_akuttsuksess_data %in% "nei" &
        .data$akutt_suksess %in% c(0, 2) ~ "nei",

      ! .data$indik_akuttsuksess_data %in% "nei" &
        (is.na(.data$akutt_suksess) |
           ! .data$akutt_suksess %in% c(0, 2) ) ~ "manglende",

      .data$indik_akuttsuksess_data %in% "nei"  ~ NA_character_,
      TRUE ~ NA_character_)

  )
}


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
#' verdien \emph{ja} dersom forløpstype er AFLI (\code{forlopstype} = 1)
#' uten AV-knuter (\code{abla_strat_av_his} = 0) og dersom pasienten er abladert
#' (\code{abla_strat_ingen} er ikke manglende).
#' \item telleren \code{indik_avbrudd} har verdien \emph{ja} dersom
#'  \code{indik_avbrudd_data} = \emph{ja} og
#'  \code{abla_strat_ingen_arsak} = 4(tekniske problemer) eller 5(Komplikasjon),
#'   verdien \emph{nei} dersom \code{indik_avbrudd_data} = \emph{ja} og
#'   ingen avbrudd eller avbrudd av andre årsaker.
#'  Verdien \emph{NA} dersom forløpet ikke er i datagrunnlaget.
#'  }
#'
#' @param df data.frame med ablanor-data. Må inneholde ulike variabler for de
#' ulike funksjonene.  F.eks. \code{forlopstype}, \code{abla_strat_av_his} og
#' \code{komp_tamp} for indikatoren "Komplikasjon tamponade for AFLI uten AV knuter" .
#'
#' @name utlede_kvalitetsindikatorer
#' @aliases
#' indik_tamponade
#'
#' @examples
#'  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1),
#'                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 0, 0),
#'                   komp_tamp = c(rep(0, 6), 1, 1))
#' ablanor::indik_tamponade(df = df)
#'
#'  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1, 1, 1, 1, 1),
#'                   abla_strat_av_his = c(NA, 1, 0, 0, 1, NA, 0, 0, 0, 0, 0, 0),
#'                   abla_strat_ingen = c(rep(0, 6), NA,  1, 1,1, 1, 0),
#'                   abla_strat_ingen_arsak = c(rep(NA, 7), 1, 4,5, NA, NA))
#'  ablanor::indik_avbrudd(df = df)

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

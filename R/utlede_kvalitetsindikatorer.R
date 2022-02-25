#' Kvalitetsindikatorer for AblaNor
#'
#' Oppdaterte versjoner av kvalitetsindikatorer. I bruk fra og med våren 2022.
#'
#' For hver av kvalitetsindikatorene, legge til en variabel for datagrunnlag
#' (suffix 'data') og en for indikatoren ja/nei.
#'
#' \code{indik_tamponade()}
#' \itemize{
#' \item nevneren \code{indik_tamponade_data} (datagrunnlag) har verdien \emph{ja} dersom
#' forløpstype er AFLI (\code{forlopstype} = 1)
#' uten AV-knuter (\code{abla_strat_av_his} = 0) og
#'  \code{komp_tamp} ikke er manglende.
#' \item telleren \code{indik_tamponade} har verdien \emph{ja} dersom
#'  \code{indik_tamp_data} = \emph{ja} og \code{komp_tamp} = 1,
#'   verdien \emph{nei} dersom \code{indik_tamp_data} = \emph{ja} og \code{komp_tamp} = 0,
#'   og verdien \emph{NA} dersom forløpet ikke er i datagrunnlaget.
#' }
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
                       .data$abla_strat_av_his %in% 0 &
                       !is.na(.data$komp_tamp)),

        true = "ja",
        false = "nei",
        missing = "nei"),

      indik_tamp = dplyr::case_when(
        .data$indik_tamp_data %in% "ja" & .data$komp_tamp %in% 1 ~ "ja",
        .data$indik_tamp_data %in% "ja" & .data$komp_tamp %in% 0 ~ "nei",
        .data$indik_tamp_data %in% "nei" ~ NA_character_,
        TRUE ~ NA_character_)
    )

}
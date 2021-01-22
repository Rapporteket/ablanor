#' Hent kortnavn for forløpstype
#'
#' @description
#' Henter inn kortnavn for forløpstyper som registreres i
#' AblaNor. Disse er i hovedsak ment for bruk i figurer og tabeller for å
#' forbedre lesbarhet.
#'
#' @param df prosedyredatasett som inkluderer forlopstype
#' @param total bools, dersom FALSE har vi ikkje en rad med totalsum. Dersom
#' TRUE har vi en rad med totalsum.
#'#'
#' @return Returnerer inndata med en ekstra kolonne 'forlopstype_tekst'
#' som er en faktor med kortnavn for de ulike forløpstypene.
#'
#' @export
#' @examples
#' \dontrun{
#' legg_til_forlopstype_kortnavn(df = d_ablanor, total = TRUE)
#' }
legg_til_forlopstype_kortnavn <- function(df, total = FALSE) {

  if (total) {
    df %>%
      dplyr::mutate(forlopstype_tekst = dplyr::case_when(
        forlopstype == 1 ~ "AFLI",
        forlopstype == 2 ~ "VT",
        forlopstype == 3 ~ "SVT",
        forlopstype == 4 ~ "EFU",
        forlopstype == "Total" ~ "Totalt" ))
  } else {
    df %>%
      dplyr::mutate(forlopstype_tekst = dplyr::case_when(
        forlopstype == 1 ~ "AFLI",
        forlopstype == 2 ~ "VT",
        forlopstype == 3 ~ "SVT",
        forlopstype == 4 ~ "EFU"))
  }
}




#' Pad strings with empty spaces to obtain same column width in tables
#'
#' @param string_vector Vector of strings to be padded, using length of the
#' longest string as length
#'
#' @return Character string, all strings are of same length
#' @export
#'
#' @examples
#' string_pad(string_vector = c("Ja", "Nei", "Kanskje"))
string_pad <- function(string_vector) {
  mx <- max(nchar(string_vector))
  stringr::str_pad(string_vector, width = mx, side = "both", pad = " ")
}

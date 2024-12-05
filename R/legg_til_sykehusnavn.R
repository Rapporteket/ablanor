#' Add hospital name
#'
#' Add hospital name as text string based on value of \code{centreid}. New
#' variable is called \code{sykehusnavn}. \code{sykehusnavn} will overwrite
#' exising variable of same name.
#'
#'
#' @param df data.frame with AblaNor data. Must contain \code{centreid}.
#' @param short boolean. Short = TRUE gives short names, short = FALSE gives
#' complete names.
#' @return Returns \code{df} with new one new column.
#'
#' @export
#' @examples
#' df <- data.frame(centreid = c(102966, 104284, 4214492, 700328, 4219765))
#' ablanor::legg_til_sykehusnavn(df = df, short = TRUE)
legg_til_sykehusnavn <- function(df, short = TRUE) {

  stopifnot("centreid" %in% names(df))
  stopifnot(short %in% c(TRUE, FALSE))

  if (short == TRUE) {
    df %>% dplyr::mutate(
      sykehusnavn = dplyr::case_when(centreid == 102966 ~ "HUS",
                                     centreid == 104284 ~ "St.Olavs",
                                     centreid == 4218359 ~ "AHus",
                                     centreid == 700328 ~ "OUS",
                                     centreid == 4219765 ~ "UNN",
                                     TRUE ~ NA_character_))
  } else {
    df %>% dplyr::mutate(
      sykehusnavn = dplyr::case_when(
        centreid == 102966 ~ "Haukeland Universitetssykehus",
        centreid == 104284 ~ "St.Olavs Hospital",
        centreid == 4218359 ~ "AHus Gardermoen",
        centreid == 700328 ~ "Oslo Universitetssykehus",
        centreid == 4219765 ~ "Universitetssykehuset Nord-Norge",
        TRUE ~ NA_character_))
  }
}

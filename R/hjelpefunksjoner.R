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
        forlopstype == "Total" ~ "Totalt"))
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



#' Make sorters for pivot table
#'
#' For all factor-variables, get the order of levels and use this as input
#' in  rpivottable.
#' @param df ablanor table
#'
#' @return a text string containing a function that defines the sorters for
#' each factor-variable. This should be given in input in the getPivotTable from shiny
#' @export
#'
#' @examples
#' df <- data.frame(kjonn = factor(x = c("M", "K", "M", "M"),
#'                                 levels = c("M", "K"),
#'                                 ordered = TRUE))
#' s <- ablanor::make_sorters(df)
#' s
#' \dontrun{
#' rpivotTable::rpivotTable(df, sorters = make_sorters(df, sorters =))
#' }
make_sorters <- function(df) {

  if(df %>%  ncol() == 0) return(NULL)

  # Choose factor variables/columns
  f <- sapply(df, is.factor)
  if(!sum(f)) return(NULL)
  fcols <- names(df)[f]

  # Get levels for each variable
  flvls <- sapply(fcols,
                  function(fcol, df) levels(df[[fcol]]),
                  df = df,
                  simplify = FALSE)

  # Separate each level by backslash
  jslvls <- sapply(flvls, function(lvls) paste(paste0("\"", lvls, "\""),
                                               collapse =  " , "))

  sorter <- sprintf("if (attr == \"%s\") { return sortAs([%s]); }",
                    fcols, jslvls)

  #  Paste all strings into one long string
  sprintf("function(attr) {\nvar sortAs = $.pivotUtilities.sortAs;\n%s\n}",
          paste(sorter, collapse = "\n"))
}

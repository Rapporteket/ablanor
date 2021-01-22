
#' Utvid og organiser
#' Funksjon som utvider tabellen og legger til alle manglende kombinasjoner av
#' variablene "maned" og "legend". Deretter brukes tidyr::pivot_wider
#'  for å få en rad per måned og en kolonne per nivå av faktoren "legend"
#' @param data datasett med variablene maaned,legend og value
#' @param remove_NAcols Boolsk variabel. Remove_NA =TRUE dersom nivået "NA" skal
#' fjernes. FALSE, dersom vi ønsker beholde nivået "NA" og ha en kolonne for
#' dette nivået
#'
#' @return data.frame med en rad per nivå av maaned og en kolonne per nivå av
#' "legend"
#' @export
#'
#' @examples
#' x <- data.frame(maaned = rep(c("jan", "feb", "mars", "april"), 2),
#'   legend = factor(x = c(rep("en",4) ,rep("to",4)),
#'   levels = c("en", "to", "tre")),
#'   value = paste0("1/", 1:8))
#'leggTilTomme_PivotWide(x, remove_NA = FALSE)

leggTilTomme_PivotWide <- function(data, remove_NAcols = TRUE) {

  # declare dot
  . <- ""

  #  check if variable names are maaned, legend and value
  if (!(is.data.frame(data) &&
        all(c("maaned", "legend", "value") %in% names(data)))) {
    stop("Inndata må være tibble/data.frame med kolonnene 'maaned',
         'legend' og 'value'")
  }

  # Empty data frame with all possible combinations of maaned and legend
  all_combinations <- data %>% tidyr::expand(., .data$maaned, .data$legend)

  data_out <- data %>%
   dplyr::full_join(., all_combinations, by = c("maaned", "legend")) %>%
   tidyr::replace_na(list(value = " -- ")) %>%
   dplyr::arrange(legend) %>%
   dplyr::arrange(maaned) %>%
   tidyr::pivot_wider(names_from = .data$legend, values_from = .data$value)


  if (!is.null(data) & remove_NAcols == TRUE) {
    data_out %<>% dplyr::select_if(!names(.) %in% c("NA"))
  }

  data_out
}

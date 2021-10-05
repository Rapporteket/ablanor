#' Behandling av kodeboken (klokeboken) før bruk
#'
#' Les inn kodeboken og velge ut enkelte variabler
#'
#' @return data.frame med følgende kolonnener fra kodeboken til AblanNor:
#' \code{fysisk_feltnavn} (variabel-navn)
#' \code{type} (variabeltype), \code{listeverdier} (numeriske verdier for
#'  Listevariabler) og \code{listetekst} (tekst-verdi for Listevariabler).
#'  Hver variabel har en rad i \code{kodeboken}. For variabler av \code{type}
#'  "Listevariabel"  inneholder kodeboken en rad per listeverdi.
#' @export
getKodebokData <- function() {

  ablanor::kbd %>%
    dplyr::mutate(fysisk_feltnavn = tolower(.data$fysisk_feltnavn)) %>%
    dplyr::select(.data$fysisk_feltnavn,
                  .data$listeverdier,
                  .data$listetekst,
                  .data$type)
}

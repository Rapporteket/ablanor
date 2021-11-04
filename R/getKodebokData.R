#' Behandling av kodeboken (klokeboken) før bruk
#'
#' Les inn kodeboken
#'
#' @return data.frame med kodeboken til AblanNor. Nøkkelkolonner:
#' \code{fysisk_feltnavn} (variabel-navn)
#' \code{type} (variabeltype), \code{listeverdier} (numeriske verdier for
#'  Listevariabler) og \code{listetekst} (tekst-verdi for Listevariabler).
#'  Hver variabel har en rad i \code{kodeboken}. For variabler av \code{type}
#'  "Listevariabel"  inneholder kodeboken en rad per listeverdi.
#' @export
getKodebokData <- function() {

  ablanor::kbd %>%
    dplyr::mutate(fysisk_feltnavn = tolower(.data$fysisk_feltnavn))
}

#' Behandling av kodeboken (klokeboken) før bruk
#'
#' Les inn kodeboken og velge ut enkelte variabler
#'
#' @param registryName Dersom verdien "test_ablanor_lokalt" leser vi inn
#' lokal RData-fil. Ellers er det SQL spørring
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param tekstVars legge til tekstvariabler hentet fra kodebok for kategoriske
#' variabler eller fra tabeller med tekst/labels for kategoriske variabler
#' @param ... Optional arguments to be passed to the function
#'
#' @return data.frame med kolonnene \code{fysisk_feltnavn} (variabel-navn)
#' \code{type} (variabeltype), \code{listeverdier} (numeriske verdier for
#'  Listevariabler) og \code{listetekst} (tekst-verdi for Listevariabler).
#'  Hver variabel har en rad i \code{kodeboken}. For variabler av \code{type}
#'  "Listevariabel"  inneholder kodeboken en rad per listeverdi.
#' @export
getKodebokData <- function(registryName,
                           singleRow = FALSE,
                           tekstVars = FALSE,
                           ...) {
  . <- ""

  ablanor::kbd %>%
    dplyr::mutate(fysisk_feltnavn = tolower(.data$fysisk_feltnavn)) %>%
    dplyr::select(.data$fysisk_feltnavn,
                  .data$listeverdier,
                  .data$listetekst,
                  .data$type)
}

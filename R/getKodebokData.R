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



#' Legg til utledete variabler i kodeboken
#'
#' Les inn kodeboken og legg til definisjoner på utledete variabler. Velge ut
#' kun enklelte kolonner. Brukes i "Kodebok"-fanen på Rapporteket.
#'
#' @return data.frame med kodeboken til AblanNor med rader for utledete
#' variabler Nøkkelkolonner:  \code{fysisk_feltnavn} (variabel-navn)
#' \code{listeverdier} (numeriske verdier for Listevariabler),
#' \code{listetekst} (tekst-verdi for Listevariabler),
#' \code{skjemanavn} (opprinnelse av variabelen) og
#' \code{ledetekst}(forklaring av variabelen).
#'  Hver variabel har en rad i \code{kodeboken}. For variabler av \code{type}
#'  "Listevariabel"  inneholder kodeboken en rad per listeverdi.
#' @export

getKodebokMedUtledetedVar <- function() {

  ablanor::kbd %>%
    dplyr::mutate(fysisk_feltnavn = tolower(.data$fysisk_feltnavn)) %>%
    dplyr::select(.data$skjemanavn,
                  .data$ledetekst,
                  .data$fysisk_feltnavn,
                  .data$listeverdier,
                  .data$listetekst) %>%
    dplyr::mutate(listeverdier = as.character(.data$listeverdier)) %>%
    dplyr::bind_rows(ablanor::def_utledete_var)


}

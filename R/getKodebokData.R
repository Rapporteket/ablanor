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
    dplyr::mutate(

      fysisk_feltnavn = tolower(.data$fysisk_feltnavn),

      # Dersom identitiske "fysisk_feltnavn" gi samme prefix som i utforsker.
      fysisk_feltnavn = dplyr::case_when(

        # FRA GKV:
        .data$fysisk_feltnavn %in% c("complete",
                                     "incomplete_reason",
                                     "status") &
          .data$skjemanavn == "GKV spørreskjema somatikk, voksne, døgn" ~
          paste0("gkv_", .data$fysisk_feltnavn),

        # FRA RAND12
        .data$fysisk_feltnavn %in% c("complete",
                                     "incomplete_reason",
                                     "status") &
          .data$skjemanavn == "Livskvalitetsskjema" ~
          paste0("rand_", .data$fysisk_feltnavn),

        # FRA OPPFØLGING
        .data$fysisk_feltnavn %in% c("complete",
                                     "dato_followup",
                                     "incomplete_reason",
                                     "q1",
                                     "q2",
                                     "q3",
                                     "q4",
                                     "q5",
                                     "q6",
                                     "q7",
                                     "status") &
          .data$skjemanavn == "Oppfølging etter 1 år" ~
          paste0("followup1_", .data$fysisk_feltnavn),


        # FRA OPPFØLGING
        .data$fysisk_feltnavn %in% c("complete",
                                     "dato_followup",
                                     "incomplete_reason",
                                     "q1",
                                     "q2",
                                     "q3",
                                     "q4",
                                     "q5",
                                     "q6",
                                     "q7",
                                     "status") &
          .data$skjemanavn == "Oppfølging etter 5 år" ~
          paste0("followup5_", .data$fysisk_feltnavn),


        # FRA OPPFØLGING
        .data$fysisk_feltnavn %in% c("complete",
                                     "dato_followup",
                                     "incomplete_reason",
                                     "q1",
                                     "q2",
                                     "q3",
                                     "q4",
                                     "q5",
                                     "q6",
                                     "q7",
                                     "status") &
          .data$skjemanavn == "Oppfølging etter behandling" ~
          paste0("followupbasis_", .data$fysisk_feltnavn),



        # FRA PROSEDYRE
        .data$fysisk_feltnavn %in% c("status",
                                     "usercomment") &
          .data$skjemanavn == "Prosedyre" ~
          paste0("pros_", .data$fysisk_feltnavn),

        # FRA BASISSKJEMAET
        .data$fysisk_feltnavn %in% c("status",
                                     "usercomment") &
          .data$skjemanavn == "Basisskjema" ~
          paste0("basereg_", .data$fysisk_feltnavn),


        TRUE ~ .data$fysisk_feltnavn))
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

   ablanor::getKodebokData() %>%
    dplyr::select(.data$skjemanavn,
                  .data$fysisk_feltnavn,
                  .data$ledetekst,
                  .data$listeverdier,
                  .data$listetekst) %>%
    dplyr::mutate(listeverdier = as.character(.data$listeverdier)) %>%
    dplyr::bind_rows(ablanor::def_utledete_var %>%
                       tidyr::replace_na(replace = list(listeverdier= "NA")))


}

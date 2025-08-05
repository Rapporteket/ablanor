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
                                     "q5_burn_freeze",
                                     "q5_pacemaker",
                                     "q5_electroconversion",
                                     "q5_other",
                                     "q5_other_specify",
                                     "q6",
                                     "q6_regular_ekg",
                                     "q6_24_hour_ekg",
                                     "q6_pacemaker",
                                     "q6_pulse_watch",
                                     "q6_other",
                                     "q6_other_specify",
                                     "q7",
                                     "q7_stroke",
                                     "q7_block",
                                     "q7_operation",
                                     "q7_pacemaker",
                                     "q7_other",
                                     "q7_other_specify",
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
                                     "q5_burn_freeze",
                                     "q5_pacemaker",
                                     "q5_electroconversion",
                                     "q5_other",
                                     "q5_other_specify",
                                     "q6",
                                     "q6_regular_ekg",
                                     "q6_24_hour_ekg",
                                     "q6_pacemaker",
                                     "q6_pulse_watch",
                                     "q6_other",
                                     "q6_other_specify",
                                     "q7",
                                     "q7_stroke",
                                     "q7_block",
                                     "q7_operation",
                                     "q7_pacemaker",
                                     "q7_other",
                                     "q7_other_specify",
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
                                     "q5_burn_freeze",
                                     "q5_pacemaker",
                                     "q5_electroconversion",
                                     "q5_other",
                                     "q5_other_specify",
                                     "q6",
                                     "q6_regular_ekg",
                                     "q6_24_hour_ekg",
                                     "q6_pacemaker",
                                     "q6_pulse_watch",
                                     "q6_other",
                                     "q6_other_specify",
                                     "status") &
          .data$skjemanavn == "Oppfølging etter behandling" ~
          paste0("followupbasis_", .data$fysisk_feltnavn),



        # FRA HENDELSE
        .data$fysisk_feltnavn %in% c("komp_janei",
                                     "komp_av_fistel",
                                     "komp_pseudoan",
                                     "komp_blodning",
                                     "komp_infek",
                                     "komp_tamp",
                                     "komp_nfren",
                                     "komp_apoplexi",
                                     "komp_avblokk_pm",
                                     "komp_pulm",
                                     "komp_osofag",
                                     "komp_koronar",
                                     "komp_perikard",
                                     "komp_dod",
                                     "komp_annen",
                                     "komp_annet_spes",
                                     "sykehus_opph",
                                     "sykehus_elektrokonv",
                                     "sykehus_medikakonv",
                                     "sykehus_annet",
                                     "sykehus_annet_spes",
                                     "residiv",
                                     "residiv_annet_spes",
                                     "ekg_vanlig",
                                     "ekg_24h",
                                     "pacemaker",
                                     "pulsklokke",
                                     "residiv_annet",
                                     "usercomment",
                                     "status") &
          .data$skjemanavn == "Hendelse" ~
          paste0("adhoc_", .data$fysisk_feltnavn),


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
    dplyr::select("skjemanavn",
                  "fysisk_feltnavn",
                  "ledetekst",
                  "listeverdier",
                  "listetekst") %>%
    dplyr::mutate(listeverdier = as.character(.data$listeverdier)) %>%
    dplyr::bind_rows(ablanor::def_utledete_var %>%
                       tidyr::replace_na(replace = list(listeverdier= "NA")))


}


#' Kvalitetsindikator
#' Kvalitetsindikator for komplikasjoner
#' @param df data frame med kolonnen komp_janei
#' @return data frame med to nye kolonner : ki_krit_teller og ki_krit_nevner.
#' Dette KI-datasettet kan bli brukt med [ablanor::aggreger_ki_prop()]
#' @export
ki_komplikasjonar <- function(df) {
  # Eig. obligatorisk, men i tilfelle me har ikkje-ferdigstilte skjema
  df %>%
    dplyr::mutate(ki_krit_teller = .data$komp_janei == 1,
                  ki_krit_nevner = .data$komp_janei %in% 0:1)
}




#' Kvalitetsindikator for behov for pacemaker
#' Lag KI-datasett for prosedyrerelatert AV-blokk med behov
#' for etterfølgjande pacemakerimplantasjon (ja/nei).
#' @param df Prosedyredatasett.
#' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#' @export
ki_komplikasjonar_pacemaker <- function(df) {
  df %>%
    dplyr::mutate(ki_krit_teller = .data$komp_avblokk_pm,
                  ki_krit_nevner = !is.na(.data$komp_avblokk_pm))
}

#' Kvalitetsindikator for tamponade
#' Lag KI-datasett for tamponade som følge av operasjon
#' som fører til intervensjon eller forlenget sykehusopphold.
#' @param df Prosedyredatasett.
#' @return KI-datasett egnet for bruk med [ablanor::aggreger_ki_prop()].
#' @export
ki_komplikasjonar_tamponade <- function(df) {
  df %>%
    dplyr::mutate(ki_krit_teller = .data$komp_tamp,
                  ki_krit_nevner = !is.na(.data$komp_tamp))
}

#' Kvalitetsindikator for død
#' Lag KI-datasett for om pasienten døde innanfor eit
#' visst tal dagar frå prosedyren.
#' @param d_pros Prosedyredatasett.
#' @param d_mce Forløpsdatasett.
#' @param d_pas Pasientdatasett (`patientlist`-tabellen).
#' @param dagar_innan Talet på dagar som ein skal sjå om
#'   pasienten døde innan. Dødsfall *på* denne dagen
#'   vert òg rekna med. Standardverdien er 29, dvs. ein
#'   ser om pasienten døde < 30 dagar frå prosedyredatoen.
#'
#' @details Berre prosedyrar gjort minst `dagar_innan`
#'   før *dagens dato* inngår i indikatoren, pluss
#'   prosedyrar der pasienten døde under operasjon.
#'   Grunnen er at det berre er desse me garantert kan avgjera
#'   om pasienten overlevde eller ikkje.
#'
#' @note Indikatoren kan ha misvisande tal (for mange overlevande)
#'   for prosedyrar veldig nær datadumpdatoen, spesielt for historiske
#'   datadumpar. I framtida vil funksjonen kanskje
#'   endrast til å ta inn ein eksplisitt dato
#'   for når me har oppdatert dødsdatoinformasjon frå.
#'
#' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#'
#' @export
ki_dod <- function(d_pros, d_mce, d_pas, dagar_innan = 29) {
  # Dato me reknar med å ha oppdatert dødsinformasjon for
  # fixme: Bør få eksplisitt dato her, eks. basert på datadumpdatoen,
  #        ev. fråtrekt nokre dagar (for forseinka registrering av død).
  #        Sjå @note ovanfor.
  . <-
    dato_oppdatert_dodsinfo <- Sys.Date()

  d_dod <- d_pros %>%
    dplyr::left_join(., dplyr::select(d_mce, .data$mceid, .data$patient_id),
                     by = "mceid") %>%
    dplyr::left_join(., dplyr::select(d_pas, .data$id, .data$deceased_date),
                     by = c("patient_id" = "id"))

  d_dod <- d_dod %>%
    dplyr::mutate(dagar_sidan_op = difftime(dato_oppdatert_dodsinfo,
                                            .data$dato_pros,
                                            units = "days"),

                  dagar_op_til_dod = ifelse(.data$komp_dod,
                                            0, # Død ved / rett etter prosedyren
                                            difftime(.data$deceased_date,
                                                     .data$dato_pros,
                                                     units = "days")),

                  ki_krit_nevner = (.data$dagar_sidan_op >= dagar_innan) |
                    .data$komp_dod,

                  ki_krit_teller = .data$ki_krit_nevner &
                    !is.na(.data$dagar_op_til_dod) &
                    (.data$dagar_op_til_dod <= dagar_innan))
  d_dod
}


#' Kvalitetsindikatorar for akutt suksess
#' Lag KI-datasett for om ein prosedyre skal reknast som akutt suksess (ja/nei).
#'
#' @param df Prosedyredatasett.
#' Viss variabelen for akutt suksess har verdien «usikker» (2), vert det rekna
#'  som «nei». Berre prosedyrarar der pasienten faktisk vart abladert er med i
#'  utrekninga (observasjonar med verdien «ikke aktuelt» (3) for akutt suksess
#'  inngår altså ikkje).
#'
#' Det finst òg nokre spesialfunksjonar for undergrupper:
#'   - `ki_akutt_suksess_svt()`: Ser berre på SVT-forløp.
#'   - `ki_akutt_suksess_svt_avrt()`: Ser berre på SVT-forløp med undergruppa
#'   AVRT (aksessoriske baner).
#'   - `ki_akutt_suksess_svt_avnrt()`: Ser berre på SVT-forløp med undergruppa
#'   AVNRT (AV nodal reentry).
#'   - `ki_akutt_suksess_svt_ikkje_avrt_el_avnrt()`: Ser berre på SVT-forløp,
#'   men ekskluderer undergruppene AVRT og AVNRT.
#'
#' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#' @export
ki_akutt_suksess <- function(df) {
  # Må vera abladert (*skal* tilsvara «akutt_suksess == 3»,
  # men er med for sikkerheits skuld)
  df %>%
    dplyr::filter(!.data$abla_strat_ingen) %>% #
    dplyr::mutate(ki_krit_nevner = .data$akutt_suksess %in% 0:2,
                  ki_krit_teller = .data$akutt_suksess == 1)
}


#' @rdname ki_akutt_suksess
#' @export
ki_akutt_suksess_svt <- function(df) {
  df %>%
    dplyr::filter(.data$forlopstype == 3) %>%
    ablanor::ki_akutt_suksess()
}

#' @rdname ki_akutt_suksess
#' @export
ki_akutt_suksess_svt_avrt <- function(df) {
  df %>%
    dplyr::filter(.data$forlopstype == 3,
                  .data$aryt_i47_1_underkat == 4) %>%
    ablanor::ki_akutt_suksess()
}

#' @rdname ki_akutt_suksess
#' @export
ki_akutt_suksess_svt_avnrt <- function(df) {
  df %>%
    dplyr::filter(.data$forlopstype == 3,
                  .data$aryt_i47_1_underkat %in% 1:2) %>%
    ablanor::ki_akutt_suksess()
}

#' @rdname ki_akutt_suksess
#' @export
ki_akutt_suksess_svt_ikkje_avrt_el_avnrt <- function(df) {
  df %>%
    dplyr::filter(.data$forlopstype == 3,
                  !(.data$aryt_i47_1_underkat %in% c(1, 2, 4))) %>%
    ablanor::ki_akutt_suksess()
}

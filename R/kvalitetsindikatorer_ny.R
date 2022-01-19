#' Ablanor sine kvalitetsindikatorer
#'
#' Funksjoner som lager 2 nye variabler for hver av kvalitetsindikatorene: Den
#' første variabelen forteller hvorvidt forløpet er med i nevneren
#' (datagrunnlaget) for den aktuelle indikatorer, den andre variabelen
#' forteller om forløpet er med i telleren (ja, nei, NA). Telleren er alltid
#' NA dersom forløpet ikke er med i datagrunnlaget. Nevneren har suffix
#' \emph{_data}.
#'
#' Følgende variabler er laget for de ulike funksjonene:
#'
#' \code{ki_komplikasjoner()}
#' \itemize{
#' \item nevneren \code{indikator_komplikasjon_data} (datagrunnlag) er \emph{ja}
#'  for alle ferdigstilte skjema og \emph{nei} for ikke-ferdigstilte skjema.
#' \item telleren \code{indikator_komplikasjon} er \emph{ja} dersom
#'  \code{komp_janei} er lik 1 og \emph{nei} dersom \code{komp_janei} er lik 0.
#'   }
#'
#' \code{ki_dod_ny()}
#' \itemize{
#' \item nevneren \code{indikator_dod_data} (datagrunnlag) er \emph{nei}
#'  for avdøde pasienter der enten \code{dato_pros} eller \code{deceased_date}
#'  mangler, og \emph{nei} for levende pasienter der \code{dato_pros} mangler
#'  eller det er færre enn \code{dager_innen} dager mellom prosedyren og
#'  sensurdatoen (dette gjelder forløp nær datadump-dato, der eventuell
#'  informasjon om død ikke har fått tid til å bli overført fra
#'  folkeregisteret).
#'  andre forløp er \code{indikator_dod_data} lik \emph{ja}.
#' \item telleren \code{indikator_dod} er \emph{ja} dersom pasienten er død innen
#' \code{dager_inne} dager etter prosedyren. \code{indikator_dod} er \emph{nei}
#' dersom pasienten ikke er død, eller er død over \code{dager_innen} (ofte 30)
#'  dager etter prosedyren. Dersom forløpet ikke er med i datagrunnlaget, så
#'   er \code{indikator_dod} lik \emph{NA}.
#' }
#'
#' @param df Ablanors koblete tabell (pros_patient_followup). Må inneholde
#' utvalgte variabler, avhengeig av hvilke indikator som skal regnes ut. F.eks.
#' \code{ki_komplikasjoner()} trenger \emph{komp_janei} og \emph{pros_status}.
#' @param dager_innen Numerisk. Default verdi er 30. Brukes som parameter i
#' \code{ki_dod_ny()}, dersom antall dager mellom prosedyre og dødsfall er
#' under dette sifferet så vil forløpet telle i indikatoren.
#' @param dato_sensur Dato. Sensur-dato for overlevelse. Brukes som parameter i
#' \code{ki_dod_ny()} Det tar inntil 30 dager for å overføre informasjon om død
#' fra folkeregisteret til databasen. Forløp for nær dato for datadump bør ikke
#' være med i datagrunnlaget for indikatoren.
#' @name kvalitetsindikatorer_ny
#' @aliases
#' ki_komplikasjoner
#' ki_dod_ny
#'
#' @examples
#' x <- data.frame(
#'         mceid = 1:5,
#'         pros_status = c(-1, 0, 1, 1, 1),
#'         komp_janei = c(NA, 0, 1, 1, 1))
#' ablanor::ki_komplikasjoner(df = x)
#' x <- data.frame(
#'   dato_pros = as.Date(c("2021-01-19",
#'                         "2020-12-16",
#'                         "2020-12-16"), format = "%Y-%m-%d"),
#'   deceased = c(0, 1, 1),
#'   deceased_date = as.Date(c(NA, "2022-01-12", "2020-12-16"),
#'                           format = "%Y-%m-%d"))
#' ablanor::ki_dod_ny(df = x,
#'                    dato_sensur = as.Date("2022-01-20", format ="%Y-%m-%d"))

NULL

#' @rdname kvalitetsindikatorer_ny
#' @export
ki_komplikasjoner <- function(df) {

  stopifnot(c("pros_status", "komp_janei") %in% names(df))

  df %>%
    dplyr::mutate(

      # datagrunnlag
      indikator_komplikasjon_data = dplyr::if_else(
        condition = .data$pros_status == 1,
        true = "ja",
        false = "nei",
        missing = "nei"),

      indikator_komplikasjon = dplyr::case_when(
        .data$indikator_komplikasjon_data == "ja" &
          .data$komp_janei == 1 ~ "ja",

        .data$indikator_komplikasjon_data == "ja" &
          .data$komp_janei == 0 ~ "nei",

        .data$indikator_komplikasjon_data == "nei" ~ NA_character_,

        FALSE ~ NA_character_)
    )
}

#' @rdname kvalitetsindikatorer_ny
#' @export
ki_dod_ny <- function(df, dager_innen = 30, dato_sensur) {

  stopifnot(c("dato_pros", "deceased_date" , "deceased") %in% names(df))

  df %>%
    dplyr::mutate(


      dager_pros_sensur = dplyr::case_when(
        # For avdøde:
        .data$deceased == 1 ~ as.numeric(difftime(deceased_date,
                                                  .data$dato_pros,
                                                  units = "days")),
        # For levende
        .data$deceased == 0 ~ as.numeric(difftime(dato_sensur,
                                                  .data$dato_pros,
                                                  units = "days")),
        TRUE ~ NA_real_),


      # datagrunnlag : Kun pasienter som med over 30 dager mellom nedlasting av
      # datadump (sensur) og prosedyre
      indikator_dod_data = dplyr::case_when(

        # alle døde, som har datoer, er med
        .data$deceased == 1 &
          !is.na(.data$dato_pros) &
          !is.na(.data$deceased_date) ~ "ja",


        # døde, som manger datoer, er  ikke med
        .data$deceased == 1 &
          (is.na(.data$dato_pros) | is.na(.data$deceased_date)) ~ "nei",


        # levende med lang nok sensur
        .data$deceased == 0 &
          !is.na(.data$dato_pros) &
          .data$dager_pros_sensur >= dager_innen ~ "ja",

        # levende med for kort sensur, eller manglende dato for prosedyre
        .data$deceased == 0 &
          !is.na(.data$dato_pros) &
          (.data$dager_pros_sensur < dager_innen |
             is.na(.data$dager_pros_sensur)) ~ "nei",

        # levende med for kort sensur, eller manglende dato for prosedyre
        .data$deceased == 0 &
          is.na(.data$dato_pros) ~ "nei",

        TRUE ~ NA_character_),

      indikator_dod = dplyr::case_when(
        # Med i datagrunnlaget, død, død innen 30 dager
        .data$indikator_dod_data == "ja" &
          .data$dager_pros_sensur < dager_innen &
          .data$deceased == 1 ~ "ja",

        # Med i datagrunnlaget, død, over 30 dager
        .data$indikator_dod_data == "ja" &
          .data$dager_pros_sensur >= dager_innen &
          .data$deceased == 1 ~ "nei",

        # Med i datagrunnlaget, levende
        .data$indikator_dod_data == "ja" &
          .data$deceased == 0 ~ "nei",


        .data$indikator_dod_data == "nei" ~ NA_character_,

        FALSE ~ NA_character_)
    )
}


#'
#'
#' #' Kvalitetsindikator for behov for pacemaker
#' #' Lag KI-datasett for prosedyrerelatert AV-blokk med behov
#' #' for etterfølgjande pacemakerimplantasjon (ja/nei).
#' #' @param df Prosedyredatasett.
#' #' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#' #' @export
#' ki_komplikasjonar_pacemaker <- function(df) {
#'   df %>%
#'     dplyr::mutate(ki_krit_teller = .data$komp_avblokk_pm == 1,
#'                   ki_krit_nevner = !is.na(.data$komp_avblokk_pm))
#' }
#'
#' #' Kvalitetsindikator for tamponade
#' #' Lag KI-datasett for tamponade som følge av operasjon
#' #' som fører til intervensjon eller forlenget sykehusopphold.
#' #' @param df Prosedyredatasett.
#' #' @return KI-datasett egnet for bruk med [ablanor::aggreger_ki_prop()].
#' #' @export
#' ki_komplikasjonar_tamponade <- function(df) {
#'   df %>%
#'     dplyr::mutate(ki_krit_teller = .data$komp_tamp == 1,
#'                   ki_krit_nevner = !is.na(.data$komp_tamp))
#' }
#'
#' #' Kvalitetsindikator for død
#' #' Lag KI-datasett for om pasienten døde innanfor eit
#' #' visst tal dagar frå prosedyren.
#' #' @param d_pros Prosedyredatasett.
#' #' @param d_mce Forløpsdatasett.
#' #' @param d_pas Pasientdatasett (`patientlist`-tabellen).
#' #' @param dagar_innan Talet på dagar som ein skal sjå om
#' #'   pasienten døde innan. Dødsfall *på* denne dagen
#' #'   vert òg rekna med. Standardverdien er 29, dvs. ein
#' #'   ser om pasienten døde < 30 dagar frå prosedyredatoen.
#' #'
#' #' @details Berre prosedyrar gjort minst `dagar_innan`
#' #'   før *dagens dato* inngår i indikatoren, pluss
#' #'   prosedyrar der pasienten døde under operasjon.
#' #'   Grunnen er at det berre er desse me garantert kan avgjera
#' #'   om pasienten overlevde eller ikkje.
#' #'
#' #' @note Indikatoren kan ha misvisande tal (for mange overlevande)
#' #'   for prosedyrar veldig nær datadumpdatoen, spesielt for historiske
#' #'   datadumpar. I framtida vil funksjonen kanskje
#' #'   endrast til å ta inn ein eksplisitt dato
#' #'   for når me har oppdatert dødsdatoinformasjon frå.
#' #'
#' #' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#' #'
#' #' @export
#' ki_dod <- function(d_pros, d_mce, d_pas, dagar_innan = 29) {
#'   # Dato me reknar med å ha oppdatert dødsinformasjon for
#'   # fixme: Bør få eksplisitt dato her, eks. basert på datadumpdatoen,
#'   #        ev. fråtrekt nokre dagar (for forseinka registrering av død).
#'   #        Sjå @note ovanfor.
#'   . <-
#'     dato_oppdatert_dodsinfo <- Sys.Date()
#'
#'   d_dod <- d_pros %>%
#'     dplyr::left_join(., dplyr::select(d_mce, .data$mceid, .data$patient_id),
#'                      by = "mceid") %>%
#'     dplyr::left_join(., dplyr::select(d_pas, .data$id, .data$deceased_date),
#'                      by = c("patient_id" = "id"))
#'
#'   d_dod <- d_dod %>%
#'     dplyr::mutate(dagar_sidan_op = difftime(dato_oppdatert_dodsinfo,
#'                                             .data$dato_pros,
#'                                             units = "days"),
#'
#'                   dagar_op_til_dod = ifelse(.data$komp_dod,
#'                                             0, # Død ved / rett etter prosedyren
#'                                             difftime(.data$deceased_date,
#'                                                      .data$dato_pros,
#'                                                      units = "days")),
#'
#'                   ki_krit_nevner = (.data$dagar_sidan_op >= dagar_innan) |
#'                     .data$komp_dod,
#'
#'                   ki_krit_teller = .data$ki_krit_nevner &
#'                     !is.na(.data$dagar_op_til_dod) &
#'                     (.data$dagar_op_til_dod <= dagar_innan))
#'   d_dod
#' }
#'
#'
#' #' Kvalitetsindikatorar for akutt suksess
#' #' Lag KI-datasett for om ein prosedyre skal reknast som akutt suksess (ja/nei).
#' #'
#' #' @param df Prosedyredatasett.
#' #' Viss variabelen for akutt suksess har verdien «usikker» (2), vert det rekna
#' #'  som «nei». Berre prosedyrarar der pasienten faktisk vart abladert er med i
#' #'  utrekninga (observasjonar med verdien «ikke aktuelt» (3) for akutt suksess
#' #'  inngår altså ikkje).
#' #'
#' #' Det finst òg nokre spesialfunksjonar for undergrupper:
#' #'   - `ki_akutt_suksess_svt()`: Ser berre på SVT-forløp.
#' #'   - `ki_akutt_suksess_svt_avrt()`: Ser berre på SVT-forløp med undergruppa
#' #'   AVRT (aksessoriske baner).
#' #'   - `ki_akutt_suksess_svt_avnrt()`: Ser berre på SVT-forløp med undergruppa
#' #'   AVNRT (AV nodal reentry).
#' #'   - `ki_akutt_suksess_svt_ikkje_avrt_el_avnrt()`: Ser berre på SVT-forløp,
#' #'   men ekskluderer undergruppene AVRT og AVNRT.
#' #'
#' #' @return KI-datasett eigna for bruk med [ablanor::aggreger_ki_prop()].
#' #' @export
#' ki_akutt_suksess <- function(df) {
#'   # Må vera abladert (*skal* tilsvara «akutt_suksess == 3»,
#'   # men er med for sikkerheits skuld)
#'   df %>%
#'     dplyr::filter(!.data$abla_strat_ingen) %>% #
#'     dplyr::mutate(ki_krit_nevner = .data$akutt_suksess %in% 0:2,
#'                   ki_krit_teller = .data$akutt_suksess == 1)
#' }
#'
#'
#' #' @rdname ki_akutt_suksess
#' #' @export
#' ki_akutt_suksess_svt <- function(df) {
#'   df %>%
#'     dplyr::filter(.data$forlopstype == 3) %>%
#'     ablanor::ki_akutt_suksess()
#' }
#'
#' #' @rdname ki_akutt_suksess
#' #' @export
#' ki_akutt_suksess_svt_avrt <- function(df) {
#'   df %>%
#'     dplyr::filter(.data$forlopstype == 3,
#'                   .data$aryt_i47_1_underkat == 4) %>%
#'     ablanor::ki_akutt_suksess()
#' }
#'
#' #' @rdname ki_akutt_suksess
#' #' @export
#' ki_akutt_suksess_svt_avnrt <- function(df) {
#'   df %>%
#'     dplyr::filter(.data$forlopstype == 3,
#'                   .data$aryt_i47_1_underkat %in% 1:2) %>%
#'     ablanor::ki_akutt_suksess()
#' }
#'
#' #' @rdname ki_akutt_suksess
#' #' @export
#' ki_akutt_suksess_svt_ikkje_avrt_el_avnrt <- function(df) {
#'   df %>%
#'     dplyr::filter(.data$forlopstype == 3,
#'                   !(.data$aryt_i47_1_underkat %in% c(1, 2, 4))) %>%
#'     ablanor::ki_akutt_suksess()
#' }

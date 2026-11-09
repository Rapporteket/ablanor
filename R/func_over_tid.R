
##' Lag et SPC-diagram for valgt indikator og sykehus
#'
#' Lager et p-diagram med
#' \code{qicharts2} basert på indikatorverdier filtrert på valgt sykehus
#' og indikator.
#'
#' @param data Dataramme med kolonnene \code{year}, \code{unitName}, \code{var},
#'   \code{denominator} og \code{ind_id}. Metadata som \code{title} og
#'   \code{short_description} brukes dersom de finnes.
#' @param title Valgfri tittel for diagrammet. Hvis ikke spesifisert, brukes
#'   standardtittel "SPC-diagram".
#' @param subtitle Valgfri undertittel for diagrammet. Hvis ikke spesifisert
#'   brukes undertittel fra metadata dersom tilgjengelig, ellers ingen undertittel.
#'
#' @return Et ggplot-objekt med SPC-diagram for valgt indikator.
#' @export
plotSPC <- function(session, reshId, userRole, fromDate, toDate,
                    valg_figur = "alder_chadsva",coord_ymin = 0,
                    coord_ymax = 1) {


  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate)

  if(valg_figur == "alder_chadsva"){
   plot(dat$alder, dat$CHA2DS2VASc)
  }
  if(valg_figur %in% c("tamponade", "pacemakerbehov")){
    if(valg_figur == "tamponade"){
      indik_dat <- dat |>
        dplyr::select(indik_tamp_data, indik_tamp, maaned_prosedyre, aar_prosedyre) |>
        dplyr::filter(indik_tamp_data == "ja",
                      indik_tamp %in% c("ja", "nei")) |>
        dplyr::count(aar_prosedyre, indik_tamp ) %>%
        tidyr::pivot_wider(names_from = indik_tamp, values_from = n, values_fill =  0) %>%
        dplyr::transmute(
          x = aar_prosedyre,
          y = ja/(ja + nei))
      chart_title = "Tamponade"
      chart_subtitle = "Andel pasienter med tamponade per år"

      }

    if(valg_figur == "pacemakerbehov"){
      indik_dat <- dat |>
        dplyr::select(indik_pacemaker_data, indik_pacemaker, maaned_prosedyre, aar_prosedyre) |>
        dplyr::filter(indik_pacemaker_data == "ja",
                      indik_pacemaker %in% c("ja", "nei")) |>
        dplyr::count(aar_prosedyre, indik_pacemaker ) %>%
        tidyr::pivot_wider(names_from = indik_pacemaker, values_from = n, values_fill =  0) %>%
        dplyr::transmute(
          x = aar_prosedyre,
          y = ja/(ja + nei))
      chart_title = "Pacemakerbehov"
      chart_subtitle = "Andel pasienter med pacemakerbehov per år"
    }

    ggplot2::ggplot(data = indik_dat,
                    ggplot2::aes(x = x, y = y, group = 1)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_discrete(name = "År") +
      ggplot2::coord_cartesian(ylim = c(coord_ymin, coord_ymax)) +
      ggplot2::scale_y_continuous(name = "",
                                  breaks = seq(from=coord_ymin,
                                               to =coord_ymax,
                                               length.out = 6),
                                  labels = paste0(seq(from=coord_ymin,
                                                      to =coord_ymax,
                                                      length.out = 6)*100, "%")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = chart_title,
                    subtitle = chart_subtitle)
  }

}

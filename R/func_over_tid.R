
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
                    valg_figur = "alder_chadsva", coord_ymin = 0,
                    coord_ymax = 1, maaned_aar = "maaned_prosedyre" ) {

  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate)

  if(maaned_aar == "aar_prosedyre"){
    timeTable <- data.frame(
      aar_prosedyre = as.ordered(lubridate::year(fromDate):lubridate::year(toDate)))
  }
  if(maaned_aar == "maaned_prosedyre"){
    timeTable <- data.frame(
      monthDato = seq(fromDate,
                      toDate,
                      by = "month")) %>%
      dplyr::transmute(maaned_prosedyre = as.factor(format(x = .data$monthDato,
                                                           format = "%Y-%m"))) %>%
      dplyr::mutate(maaned_prosedyre = as.ordered(maaned_prosedyre))
  }


  if(valg_figur == "pros_varighet") {

    if(maaned_aar == "aar_prosedyre"){

      dat_varighet <- dplyr::left_join(
        x = timeTable,
        y = dat %>%
          dplyr::select(aar_prosedyre, pros_varighet ) |>
          dplyr::filter(!is.na(aar_prosedyre),
                        !is.na(pros_varighet)),
        by = "aar_prosedyre") |>
        dplyr::transmute(
          x = aar_prosedyre,
          y = pros_varighet)
      chart_title = "Prosedyrevarighet"
      chart_subtitle = "Boxplot for prosedyrevarighet (sekunder) per år"
    }
    if(maaned_aar == "maaned_prosedyre"){
      dat_varighet <- dplyr::left_join(
        x = timeTable,
        y = dat %>%
          dplyr::select(maaned_prosedyre, pros_varighet ) |>
          dplyr::filter(!is.na(maaned_prosedyre),
                        !is.na(pros_varighet)) ,
        by = "maaned_prosedyre") |>
        dplyr::transmute(
          x = maaned_prosedyre,
          y = pros_varighet)
      chart_title = "Prosedyrevarighet"
      chart_subtitle = "Boxplot for prosedyrevarighet (sekunder) per måned"
    }

    p1 <-  ggplot2::ggplot(data = dat_varighet,
                           mapping= ggplot2::aes(x = x,
                                                 y = y)) +
      ggplot2::geom_boxplot(na.rm = TRUE)

  }

  if(valg_figur %in% c("tamponade", "pacemakerbehov")){
    if(valg_figur == "tamponade"){
      chart_title = "Tamponade"

      if(maaned_aar == "aar_prosedyre") {
        chart_subtitle = "Andel pasienter med tamponade per år"
        indik_dat <- dplyr::left_join(
          x = timeTable,
          y = dat |>
            dplyr::select(indik_tamp_data, indik_tamp, aar_prosedyre) |>
            dplyr::filter(indik_tamp_data == "ja",
                          indik_tamp %in% c("ja", "nei")) |>
            janitor::tabyl(aar_prosedyre, indik_tamp, show_missing_levels = TRUE),
          by = "aar_prosedyre") |>
          dplyr::transmute(
            x = aar_prosedyre,
            y = ja/(ja + nei))
      }
      if(maaned_aar == "maaned_prosedyre") {
        chart_subtitle = "Andel pasienter med tamponade per måned"
        indik_dat <- dplyr::left_join(
          x = timeTable,
          y = dat |>
            dplyr::select(indik_tamp_data, indik_tamp, maaned_prosedyre) |>
            dplyr::filter(indik_tamp_data == "ja",
                          indik_tamp %in% c("ja", "nei")) |>
            janitor::tabyl(maaned_prosedyre, indik_tamp, show_missing_levels = TRUE),
          by = "maaned_prosedyre") |>
          dplyr::transmute(
            x = maaned_prosedyre,
            y = ja/(ja + nei))
      }
    }

    if(valg_figur == "pacemakerbehov"){
      chart_title = "Pacemakerbehov"
      if(maaned_aar == "aar_prosedyre"){
        indik_dat <- dplyr::left_join(
          x = timeTable,
          y = dat |>
            dplyr::select(indik_pacemaker_data, indik_pacemaker, maaned_prosedyre, aar_prosedyre) |>
            dplyr::filter(indik_pacemaker_data == "ja",
                          indik_pacemaker %in% c("ja", "nei")) |>
            janitor::tabyl(aar_prosedyre, indik_pacemaker, show_missing_levels = TRUE),
          by = "aar_prosedyre") |>
          dplyr::transmute(
            x = aar_prosedyre,
            y = ja/(ja + nei))

        chart_subtitle = "Andel pasienter med pacemakerbehov per år"
      }
      if(maaned_aar == "maaned_prosedyre") {
        indik_dat <- dplyr::left_join(
          x = timeTable,
          y = dat |>
            dplyr::select(indik_pacemaker_data, indik_pacemaker, maaned_prosedyre) |>
            dplyr::filter(indik_pacemaker_data == "ja",
                          indik_pacemaker %in% c("ja", "nei")) |>
            janitor::tabyl(maaned_prosedyre, indik_pacemaker, show_missing_levels = TRUE),
          by = "maaned_prosedyre") |>
          dplyr::transmute(
            x = maaned_prosedyre,
            y = ja/(ja + nei))
        chart_subtitle = "Andel pasienter med pacemakerbehov per måned"
      }
    }


    p1 <- ggplot2::ggplot(data = indik_dat,
                          ggplot2::aes(x = x, y = y, group = 1)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::coord_cartesian(ylim = c(coord_ymin, coord_ymax)) +
      ggplot2::scale_y_continuous(name = "",
                                  breaks = seq(from=coord_ymin,
                                               to =coord_ymax,
                                               length.out = 6),
                                  labels = paste0(seq(from=coord_ymin,
                                                      to =coord_ymax,
                                                      length.out = 6)*100, "%"))


  }
  return(p1 +
           ggplot2::scale_x_discrete(name = "", drop = FALSE) +
           ggplot2::theme(
             legend.text = ggplot2::element_text(size = 12),
             axis.text = ggplot2::element_text(size = 12),
             axis.title = ggplot2::element_text(size = 12)) +
           ggplot2::theme_minimal() +
           ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)) +
           ggplot2::labs(title = chart_title,
                         subtitle = chart_subtitle))
}

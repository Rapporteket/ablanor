
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
plotIndikatorer <- function(session, reshId, userRole, fromDate, toDate,
                            valg_figur = "indik_pacemaker", coord_ymin = 0,
                            coord_ymax = 1, maaned_aar = "maaned_prosedyre",
                            visNasjonal = "shus") {

  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = "LC",
                                     fromDate = fromDate,
                                     toDate = toDate) |>
    dplyr::mutate(
      indik_akuttsuksess_afli = ifelse(
        indik_akuttsuksess_data %in% "AFLI", indik_akuttsuksess, NA_character_),
      indik_akuttsuksess_vt = ifelse(
        indik_akuttsuksess_data %in% "VT", indik_akuttsuksess, NA_character_),
      indik_akuttsuksess_avrt = ifelse(
        indik_akuttsuksess_data %in% "AVRT", indik_akuttsuksess, NA_character_),
      indik_akuttsuksess_avnrt = ifelse(
        indik_akuttsuksess_data %in% "AVNRT", indik_akuttsuksess, NA_character_),
      kvartal_prosedyre = paste0(lubridate::year(dato_pros),
                                 "-Q",
                                 lubridate::quarter(dato_pros))
    )

  if(visNasjonal == "shus_nasj"){
    dat_nasj <- ablanor::getBaseregProsData(singleRow = FALSE,
                                            session = session,
                                            reshId = reshId,
                                            userRole = "SC",
                                            fromDate = fromDate,
                                            toDate = toDate) |>
      dplyr::mutate(
        indik_akuttsuksess_afli = ifelse(
          indik_akuttsuksess_data %in% "AFLI", indik_akuttsuksess, NA_character_),
        indik_akuttsuksess_vt = ifelse(
          indik_akuttsuksess_data %in% "VT", indik_akuttsuksess, NA_character_),
        indik_akuttsuksess_avrt = ifelse(
          indik_akuttsuksess_data %in% "AVRT", indik_akuttsuksess, NA_character_),
        indik_akuttsuksess_avnrt = ifelse(
          indik_akuttsuksess_data %in% "AVNRT", indik_akuttsuksess, NA_character_),
        kvartal_prosedyre = paste0(lubridate::year(dato_pros),
                                   "-Q",
                                   lubridate::quarter(dato_pros))
      )
  }

  if(valg_figur == "indik_tamp"){
    chart_title <- "Tamponade"
    streng_subtitle <- "Andel pasienter med tamponde"
  }
  if(valg_figur == "indik_pacemaker"){
    chart_title <- "Pacemakerbehov"
    streng_subtitle <- "Andel pasienter med pacemakerbehov"
  }
  if(valg_figur == "indik_ferdig_komp"){
    chart_title <- "Ferdigstilt komplikasjonsskjema"
    streng_subtitle <- "Andel pasienter med ferdigstilt komplikasjonsskjema"
  }
  if(valg_figur == "indik_akuttsuksess_afli"){
    chart_title <- "Akutt suksess AFLI"
    streng_subtitle <- "Andel AFLI-prosedyrer med akutt suksess for"
  }
  if(valg_figur == "indik_akuttsuksess_vt"){
    chart_title <- "Akutt suksess VT"
    streng_subtitle <- "Andel VT-prosedyrer med akutt suksess for"
  }
  if(valg_figur == "indik_akuttsuksess_avrt"){
    chart_title <- "Akutt suksess AVRT"
    streng_subtitle <- "Andel AVRT-prosedyrer med akutt suksess for"
  }
  if(valg_figur == "indik_akuttsuksess_avnrt"){
    chart_title <- "Akutt suksess AVNRT"
    streng_subtitle <- "Andel AVNRT-prosedyrer med akutt suksess for"
  }

  if(maaned_aar == "aar_prosedyre"){
    chart_subtitle <- paste0(streng_subtitle, " per år")
    timeTable <- data.frame(
      aar_prosedyre = as.ordered(lubridate::year(fromDate):lubridate::year(toDate)))
  }
  if(maaned_aar == "kvartal_prosedyre"){
    chart_subtitle <- paste0(streng_subtitle, " per kvartal")
    timeTable <- data.frame(
      quarterDato = seq(lubridate::floor_date(fromDate, unit = "quarter"),
                        toDate, by = "quarter")) %>%
      dplyr::transmute(
        kvartal_prosedyre = as.factor(paste0(lubridate::year(quarterDato),
                                             "-Q",
                                             lubridate::quarter(quarterDato)))) %>%
      dplyr::transmute(kvartal_prosedyre = as.ordered(kvartal_prosedyre))
  }

  if(maaned_aar == "maaned_prosedyre"){
    chart_subtitle <- paste0(streng_subtitle, " per måned")
    timeTable <- data.frame(
      monthDato = seq(fromDate, toDate, by = "month")) %>%
      dplyr::transmute(
        maaned_prosedyre = as.factor(format(x = .data$monthDato, format = "%Y-%m"))) %>%
      dplyr::mutate(maaned_prosedyre = as.ordered(maaned_prosedyre))
  }

  indik_dat <- dplyr::left_join(
    x = timeTable,
    y = dat |>
      dplyr::select(.data[[valg_figur]], .data[[maaned_aar]]) |>
      dplyr::filter(.data[[valg_figur]] %in% c("ja", "nei")) |>
      janitor::tabyl(.data[[maaned_aar]], .data[[valg_figur]], show_missing_levels = TRUE),
    by = maaned_aar) |>
    dplyr::transmute(
      x = .data[[maaned_aar]],
      y = ja/(ja + nei),
      sykehusnavn = ablanor::getHospitalName(reshId =reshId, newNames = TRUE))


  if(visNasjonal == "shus_nasj"){
    indik_dat_nasj <- dplyr::left_join(
      x = timeTable,
      y = dat_nasj |>
        dplyr::select(.data[[valg_figur]],  .data[[maaned_aar]]) |>
        dplyr::filter(.data[[valg_figur]] %in% c("ja", "nei")) |>
        janitor::tabyl( .data[[maaned_aar]], .data[[valg_figur]], show_missing_levels = TRUE),
      by = maaned_aar) |>
      dplyr::transmute(
        x =  .data[[maaned_aar]],
        y = ja/(ja + nei),
        sykehusnavn = "Nasjonalt")
    indik_dat <- rbind(indik_dat, indik_dat_nasj)
  }

  if(visNasjonal == "shus"){
    p1 <- ggplot2::ggplot(data = indik_dat,
                          ggplot2::aes(x = x, y = y,
                                       group = sykehusnavn,
                                       color = sykehusnavn)) +
      ggplot2::geom_point(size = 2, mapping = ggplot2::aes(colour = sykehusnavn)) +
      ggplot2::geom_line(linewidth = 1.5,mapping = ggplot2::aes(colour = sykehusnavn)) +
      ggplot2::scale_color_manual(values=c("#3A7BD5"),
                                  drop = FALSE)
  } else {
    p1 <- ggplot2::ggplot(data = indik_dat,
                          ggplot2::aes(x = x,
                                       y = y,
                                       group = sykehusnavn,
                                       color = sykehusnavn)) +
      ggplot2::geom_point(size = 2, mapping = ggplot2::aes(colour = sykehusnavn)) +
      ggplot2::geom_line(linewidth = 1.5, mapping = ggplot2::aes(colour = sykehusnavn)) +
      ggplot2::scale_color_manual(values=c("#3A7BD5", "indianred"),
                                  drop = FALSE)
  }

  p1 +
    ggplot2::coord_cartesian(ylim = c(coord_ymin, coord_ymax)) +
    ggplot2::scale_y_continuous(name = "",
                                breaks = seq(from=coord_ymin,
                                             to =coord_ymax,
                                             length.out = 6),
                                labels = paste0(seq(from=coord_ymin,
                                                    to =coord_ymax,
                                                    length.out = 6)*100, "%"))+
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5,
                                          hjust = 1, color = "#495057",
                                          face = "bold"),
      axis.text.y = ggplot2::element_text(color = "#495057"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = 16,
                                         color = "#212529",
                                         hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 12,
                                            color = "#6c757d",
                                            margin = ggplot2::margin(b = 20)),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 35, l = 10, unit = "pt")) +
    ggplot2::labs(title = chart_title,
                  subtitle = chart_subtitle)


}

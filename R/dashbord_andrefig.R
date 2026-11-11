
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
plotAndrefig <- function(session, reshId, userRole, fromDate, toDate,
                         valg_figur = "pros_varighet",
                         maaned_aar = "maaned_prosedyre" ) {

  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate) |>
    dplyr::mutate(
      kvartal_prosedyre = paste0(lubridate::year(dato_pros),
                                 "-Q",
                                 lubridate::quarter(dato_pros)))

  if(valg_figur == "pros_varighet") {
    chart_title <- "Prosedyrevarighet"
    streng_title <- "prosedyrevarighet (sekunder)"
  }

  if(valg_figur == "rtg_dose") {
    chart_title <- "Stråledose"
    streng_title <- "stråledose (enhet!)"
  }
  if(maaned_aar == "aar_prosedyre"){
    chart_subtitle <- paste0("Boxplot for ", streng_title," per år")
    timeTable <- data.frame(
      aar_prosedyre = as.ordered(lubridate::year(fromDate):lubridate::year(toDate)))
  }
  if(maaned_aar == "kvartal_prosedyre"){
    chart_subtitle <- paste0("Boxplot for ", streng_title," per kvartal")
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
    chart_subtitle <- paste0("Boxplot for ", streng_title," per måned")
    timeTable <- data.frame(
      monthDato = seq(fromDate, toDate, by = "month")) %>%
      dplyr::transmute(
        maaned_prosedyre = as.factor(format(x = .data$monthDato, format = "%Y-%m"))) %>%
      dplyr::mutate(maaned_prosedyre = as.ordered(maaned_prosedyre))
  }


  dat_plot <- dplyr::left_join(
    x = timeTable,
    y = dat %>%
      dplyr::select(.data[[maaned_aar]], .data[[valg_figur]] ) |>
      dplyr::filter(!is.na(.data[[maaned_aar]]),
                    !is.na( .data[[valg_figur]] )) ,
    by = maaned_aar) |>
    dplyr::transmute(
      x = .data[[maaned_aar]],
      y = .data[[valg_figur]])


  ggplot2::ggplot(data = dat_plot,
                  mapping= ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_boxplot(fill = "#3A7BD5",
                          alpha = 0.85,
                          width = 0.6,
                          color = "#6c757d",
                          outlier.color = "#6c757d",
                          outlier.shape = 16,
                          outlier.size = 2,
                          na.rm = TRUE)+
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.text = ggplot2::element_blank(),
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
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 35, l = 10, unit = "pt")

    ) +
    ggplot2::labs(title = chart_title,
                  subtitle = chart_subtitle)

}


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
plotAntallProsedyrer <- function(session, reshId, userRole, fromDate, toDate,
                                 valg_figur = "cumplot") {

  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate) |>
    dplyr::filter(!is.na(pros_dato))

  if(valg_figur == "cumplot") {
    chart_title <- "Kumulert antall prosedyrer"
    chart_subtitle <- "Kumulert per måned"
  }

  timeTable <- data.frame(
    monthDato = seq(fromDate, toDate, by = "month")) %>%
    dplyr::transmute(
      maaned_prosedyre = as.factor(format(x = .data$monthDato, format = "%Y-%m"))) %>%
    dplyr::mutate(
      maaned_prosedyre = as.ordered(maaned_prosedyre),
      Maaned = factor(x = substr(maaned_prosedyre, 6, 7),
                      levels = c("01", "02", "03", "04", "05", "06", "07",
                                 "08", "09", "10", "11", "12"),
                      labels = c("Jan", "Feb", "Mars", "April", "Mai",
                                 "Juni", "Juli", "Aug", "Sept", "Okt",
                                 "Nov", "Des")))


  df_cumplot <- dplyr::left_join(
    x = timeTable,
    y = dat %>%
      dplyr::count(maaned_prosedyre, aar_prosedyre),
    by = "maaned_prosedyre") |>
    dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
    dplyr::group_by(aar_prosedyre) %>%
    dplyr::mutate(
      Total = cumsum(n))

  ylim  = max(df_cumplot$Total)

  ggplot2::ggplot(df_cumplot) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = Maaned,
                                              y = Total,
                                              group = aar_prosedyre,
                                              colour = aar_prosedyre),
                       linewidth = 1.5) +

    ggplot2::xlab("") +
    ggplot2::ylim(c(0, ylim)) +
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
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 35, l = 10, unit = "pt")
      ) +
    ggplot2::labs(title = chart_title,
                  subtitle = chart_subtitle)


}

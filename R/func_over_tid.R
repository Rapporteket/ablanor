
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
plotSPC <- function(session, reshId, userRole, fromDate, toDate, valg_figur = "alder_chadsva") {


  dat <- ablanor::getBaseregProsData(singleRow = FALSE,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate)

  if(valg_figur == "alder_chadsva"){
      plot(dat$CHA2DS2VASc, dat$alder)
  }
  if(valg_figur == "tamponade"){
    indik_dat <- dat |>
      dplyr::select(indik_tamp_data, indik_tamp, maaned_prosedyre, aar_prosedyre) |>
      dplyr::filter(indik_tamp_data == "ja",
                    indik_tamp %in% c("ja", "nei")) |>
      count(aar_prosedyre, indik_tamp ) %>%
      pivot_wider(names_from = indik_tamp, values_from = n, values_fill =  0) %>%
      transmute(
        x = aar_prosedyre,
        nevner = (ja + nei),
        teller = ja,
        y = teller/nevner)

    ggplot2::ggplot(data = indik_dat,
                    aes(x = x,
                        y = y)) +
      ggplot2::geom_point() +
      ggplot2::geom_line()  }


   # required_columns <- c("year", "unitName", "var", "denominator", "ind_id")
  # missing_columns <- setdiff(required_columns, names(data))

  # if (length(missing_columns) > 0L) {
  #   stop(
  #     "plotSPC() requires columns: ",
  #     paste(required_columns, collapse = ", "),
  #     ". Missing: ",
  #     paste(missing_columns, collapse = ", "),
  #     call. = FALSE
  #   )
  # }
  #
  #
  # indikator_data <- data |>
  #   dplyr::mutate(
  #     aar = as.integer(.data$year),
  #     sykehusnavn = as.character(.data$unitName),
  #     teller = as.numeric(.data$var) * as.numeric(.data$denominator),
  #     nevner = as.numeric(.data$denominator),
  #     prosent = as.numeric(.data$var)
  #   ) |>
  #   dplyr::filter(!is.na(.data$aar), !is.na(.data$teller), !is.na(.data$nevner), .data$nevner > 0) |>
  #   dplyr::group_by(.data$aar) |>
  #   dplyr::summarize(
  #     teller = sum(.data$teller, na.rm = TRUE),
  #     nevner = sum(.data$nevner, na.rm = TRUE),
  #     .groups = "drop"
  #   ) |>
  #   dplyr::filter(.data$nevner > 0) |>
  #   dplyr::mutate(
  #     prosent = .data$teller / .data$nevner
  #   ) |>
  #   dplyr::arrange(.data$aar)
  #
  # if (nrow(indikator_data) == 0L) {
  #   stop("plotSPC() has no observations after filtering.", call. = FALSE)
  # }
  #
  # chart_title <- if (!is.null(title)) title else "SPC-diagram"
  # chart_subtitle <- if (!is.null(subtitle)) subtitle else NULL
  #
  # qicharts2::qic(
  #   x = indikator_data$aar,
  #   y = indikator_data$teller,
  #   n = indikator_data$nevner,
  #   data = indikator_data,
  #   chart = "p",
  #   xlab = "År",
  #   ylab = "Andel (%)",
  #   title = chart_title,
  #   subtitle = chart_subtitle
  # ) +
  #   ggplot2::scale_x_continuous(
  #     breaks = indikator_data$aar,
  #     labels = indikator_data$aar
  #   ) +
  #   ggplot2::theme(
  #     panel.grid = ggplot2::element_blank(),
  #     plot.margin = ggplot2::margin(r = 50, l = 20, t = 15, b = 15),
  #     plot.subtitle = ggplot2::element_text(size = 14, color = "black"),
  #     plot.title = ggplot2::element_text(size = 16, face = "bold"),
  #     axis.text.y = ggplot2::element_text(size = 12)
  #   )
}

test_that("legg til sykehusnavn works", {

  df <- data.frame(centreid = c(102966, 104284, 4218359, 700328, 4219765,
                                NA, 123456, 4208026))


  # Navn ny kolonne
  testthat::expect_equal(
    ablanor::legg_til_sykehusnavn(df) %>% names(),
    c("centreid", "sykehusnavn")
  )

  # Forventer Sykehusnavn = NA dersom feil centreid
  testthat::expect_true(all(
    ablanor::legg_til_sykehusnavn(df) %>%
      dplyr::filter(is.na(.data$centreid) | .data$centreid == 123456) %>%
      dplyr::pull(.data$sykehusnavn) %>%
      is.na()))

  # Forventer disse navnene (short)
  testthat::expect_equal(
    ablanor::legg_til_sykehusnavn(df, short = TRUE) %>%
      dplyr::filter(! is.na(.data$centreid) & .data$centreid != 123456) %>%
      dplyr::pull(.data$sykehusnavn),
    c("HUS", "St.Olavs", "AHus", "OUS", "UNN", "Ibsen"))

  # Forventer disse navnene (long)
  testthat::expect_equal(
    ablanor::legg_til_sykehusnavn(df, short = FALSE) %>%
      dplyr::filter(! is.na(.data$centreid) & .data$centreid != 123456) %>%
      dplyr::pull(.data$sykehusnavn),
    c("Haukeland Universitetssykehus", "St.Olavs Hospital", "AHus Gardermoen",
      "Oslo Universitetssykehus", "Universitetssykehuset Nord-Norge",
      "IbsenSykehusene"))


   # Mangler centreid - error
  testthat::expect_error(ablanor::legg_til_sykehusnavn(df = data.frame(a = 1)))

   # Mangler short er annet enn true/false
  testthat::expect_error(
    ablanor::legg_til_sykehusnavn(df = df, short = data.frame(a = 1)))

})

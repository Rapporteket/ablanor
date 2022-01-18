test_that("indikator komplikasjoner fungerer", {
  x <- data.frame(pros_status = c(NA, -1, -1, 0, 0, 1, 1, 1, 1),
                  komp_janei = c(NA, NA, 0, 0, 1, 0, 0, 1, 1))

  x_out <- ablanor::ki_komplikasjoner(df = x)


  testthat::expect_equal(
    colnames(x_out),
    c("pros_status",
      "komp_janei",
      "indikator_komplikasjon_data",
      "indikator_komplikasjon_data"))

  # Alle som er med i datagrunnalget har ferdigstilt
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indikator_komplikasjon_data == "ja") %>%
      dplyr::pull(.data$pros_staus) == 1))

  # alle ferdigstilte er med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$pros_staus == 1) %>%
      dplyr::pull(.data$indikator_komplikasjon_dat) == "ja"))

  # Ingen ikke-ferdigstilte er med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$pros_staus) | .data$pros_status != 1) %>%
      dplyr::pull(.data$indikator_komplikasjon_dat) == "nei"))




})

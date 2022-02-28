# Tester for TAMPONADE
test_that("KI-TAMPONADe wokrs", {

  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1, 1),
                   abla_strat_av_his = c(NA, 1, 0, 0, 1, 0, 1, 0, 0),
                   komp_tamp = c(rep(0, 6), 1, 1, NA))
  df_out <- ablanor::indik_tamponade(df = df)


  # Forventer ikke i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_tamp_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_tamp_data) == "nei"))


  # Forventer i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1,
                    .data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_tamp_data) == "ja"))

  # Forventer ja/nei/manglende
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    .data$komp_tamp == 0) %>%
      dplyr::pull(.data$indik_tamp) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    .data$komp_tamp == 1) %>%
      dplyr::pull(.data$indik_tamp) == "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "ja",
                    is.na(.data$komp_tamp)) %>%
      dplyr::pull(.data$indik_tamp) == "manglende"))


# Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_tamponade(data.frame(forlopstype = 1,
                                        abla_strat_av_his =0))
  )


})

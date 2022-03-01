# Tester for TAMPONADE ----
test_that("KI-TAMPONADE wokss", {

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

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_tamp_data == "nei") %>%
      dplyr::pull(.data$indik_tamp) %>% is.na()))

  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_tamponade(data.frame(forlopstype = 1,
                                        abla_strat_av_his =0))
  )


})





# Tester for AVBRUDD -----
test_that("KI-AVBRUDD wokrs", {

  df <- data.frame(forlopstype = c(2, 3, 4, NA, 1, 1, 1, 1, 1, 1, 1, 1),
                   abla_strat_av_his = c(NA, 1, 0, 0, 1, NA, 0, 0, 0, 0, 0, 0),
                   abla_strat_ingen = c(rep(0, 6), NA,  1, 1,1, 1, 0),
                   abla_strat_ingen_arsak = c(rep(NA, 7), 1, 4,5, NA, NA))

  df_out <- ablanor::indik_avbrudd(df = df)


  # Forventer ikke i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$forlopstype %in% 1) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(! .data$abla_strat_av_his %in% 0) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$abla_strat_ingen)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "nei"))



  # Forventer i datagrunnlaget for disse:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype %in% 1,
                    .data$abla_strat_av_his %in% 0,
                    !is.na(.data$abla_strat_ingen)) %>%
      dplyr::pull(.data$indik_avbrudd_data) == "ja"))


  # Forventer ja/nei/manglende
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "ja",
                    .data$abla_strat_ingen %in% 1,
                    .data$abla_strat_ingen_arsak %in% 4:5) %>%
      dplyr::pull(.data$indik_avbrudd) == "ja"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "ja",
                    .data$abla_strat_ingen %in% 0) %>%
      dplyr::pull(.data$indik_avbrudd) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "ja",
                    .data$abla_strat_ingen %in% 1,
                    .data$abla_strat_ingen_arsak %in% c(1, 2, 3, 9)) %>%
      dplyr::pull(.data$indik_avbrudd) == "nei"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "ja",
                    .data$abla_strat_ingen %in% 1,
                    is.na(.data$abla_strat_ingen_arsak)) %>%
      dplyr::pull(.data$indik_avbrudd) %>% is.na()))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$indik_avbrudd_data == "nei") %>%
      dplyr::pull(.data$indik_avbrudd) %>%  is.na()))


  # Forventer feilmelding
  testthat::expect_error(
    ablanor::indik_avbrudd(data.frame(forlopstype = 1,
                                      abla_strat_av_his =0))
  )


})


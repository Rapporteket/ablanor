test_that("indikator komplikasjoner fungerer", {
  x <- data.frame(pros_status = c(NA, -1, -1, 0, 0, 1, 1, 1, 1),
                  komp_janei = c(NA, NA, 0, 0, 1, 0, 0, 1, 1))

  x_out <- ablanor::ki_komplikasjoner(df = x)


  testthat::expect_equal(
    colnames(x_out),
    c("pros_status",
      "komp_janei",
      "indikator_komplikasjon_data",
      "indikator_komplikasjon"))

  # Alle som er med i datagrunnalget har ferdigstilt
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indikator_komplikasjon_data == "ja") %>%
      dplyr::pull(.data$pros_status) == 1))

  # alle ferdigstilte er med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$pros_status == 1) %>%
      dplyr::pull(.data$indikator_komplikasjon_data) == "ja"))

  # Ingen ikke-ferdigstilte er med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$pros_status) | .data$pros_status != 1) %>%
      dplyr::pull(.data$indikator_komplikasjon_data) == "nei"))

})

test_that("indikator død fungerer", {

  dato_sensur = as.Date("2020-12-31", format = "%Y-%m-%d" )
  x <- data.frame(dato_pros = as.Date(c(NA,
                                        "2020-12-20",
                                        "2020-05-16",
                                        "2020-05-16",
                                        "2020-05-16",
                                        "2020-05-16",
                                        "2020-05-16"), format = "%Y-%m-%d"),
                  deceased = c(0, 0, 1, 0, 1, 1, 1),
                  deceased_date = as.Date(c(NA,
                                            NA,
                                            "2020-05-16",
                                            NA,
                                            "2020-06-14",
                                            "2021-01-15",
                                            "2020-06-15")))

  x_out <- ablanor::ki_dod_ny(df = x,
                              dager_innen = 30,
                              dato_sensur = dato_sensur)


  testthat::expect_equal(
    colnames(x_out),
    c("dato_pros",
      "deceased",
      "deceased_date",
      "dager_pros_sensur",
      "indikator_dod_data",
      "indikator_dod"))

  # Minst 30 dager mellom prosedyre og sensur (dersom levende), eller død for
  #  å være med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indikator_dod_data == "ja",
                    .data$deceased == 0) %>%
      dplyr::pull(.data$dager_pros_sensur) >= 30))

  # Døde, som har både prosedyre og dødsdato er med i datagrunnlaget
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$deceased == 1,
                    !is.na(.data$dato_pros),
                    !is.na(.data$deceased_date)) %>%
      dplyr::pull(.data$.data$indikator_dod_data) == "ja"))

testthat::expect_equal(
  c("nei", "nei", rep("ja", 5)),
  x_out %>% dplyr::pull(.data$indikator_dod_data)
)

testthat::expect_equal(
  c(NA_character_, NA_character_, "ja", "nei", "ja", "nei", "nei"),
  x_out %>% dplyr::pull(.data$indikator_dod)
)
})

testthat::test_that("utlede_alder() works", {

  x <- data.frame(
    birth_date = as.Date(c(rep("1951-12-30",4), NA,
                           "2071-12-31", "1951-15-30"),
                         format = "%Y-%m-%d"),
    dato_pros = as.Date(c("2021-05-29", "2021-12-29", "2021-12-30",
                          NA, rep("2021-12-30", 3)),
                        format = "%Y-%m-%d"))


  x_out <- ablanor::utlede_alder(x)

  #  Forventer NA dersom minst en dato mangler
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$birth_date) | is.na(dato_pros)) %>%
      dplyr::pull(.data$alder) %>%
      is.na()))


  #  Forventer ingen NA dersom begge datoene finnes
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(!is.na(.data$birth_date) & !is.na(dato_pros)) %>%
      dplyr::pull(.data$alder) %>%
      is.na()  == FALSE))


  #  Forventer disse verdiene dersom ingen dato mangler:
  testthat::expect_equal(
    x_out %>%
      dplyr::filter(!is.na(.data$birth_date) & !is.na(dato_pros)) %>%
      dplyr::pull(.data$alder) ,
    c(69, 69, 70, -50))

  # Forventer feilmedling dersom en av variablene mangler i datasettet
  testthat::expect_error(
    ablanor::utlede_alder(df = data.frame(tull1 = 1, tull2 = 2)))

  # Forventer feilmelding dersom ikke dato-variabler
  testthat::expect_error(
    ablanor::utlede_alder(df = data.frame(birth_date = "1951-12-30",
                                          dato_pros = "2021-12-30")))


})



testthat::test_that("utlede_alder_75() works", {

  x <- data.frame(alder = c(NA, 60:80, 100, -20))
  x_out <- utlede_alder_75(x)

  # Forventer NA dersom alder manglende
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(is.na(alder)) %>%
      dplyr::pull(alder_75) %>%
      is.na()
  ))

  # Forventer "<75" dersom yngre enn 75 år
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(alder < 75) %>%
      dplyr::pull(alder_75)  == "<75"
  ))

  # Forventer ">=75" dersom 75 år eller eldre
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(alder >= 75) %>%
      dplyr::pull(alder_75)  == ">=75"
  ))

  #Forventer feilmelding dersom alder mangler i df

  # Forventer "<75" dersom yngre enn 75 år
  testthat::expect_error(
    ablanor::utlede_alder_75(data.frame(tull = 1))
  )

})


testthat::test_that("Aldersklasse fungerer", {

  x <- data.frame(alder = c(-1, 10, 17, 18, 20,
                            59, 60, 61, 69, 70,
                            99, 100, NA, NA, NA))

  x_out <- ablanor::utlede_aldersklasse(x)

  # sjekk at de som skal bli NA blir det:
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$alder < 18 |
                      .data$alder > 99 |
                      is.na(.data$alder)) %>%
      dplyr::pull(.data$aldersklasse) %>%
      is.na()))

  # Sjekk at øvre/nedre grense for aldersklasse er er riktig
  testthat::expect_equal(
    x_out %>%
      dplyr::filter(.data$aldersklasse == "60-69") %>%
      dplyr::pull(.data$alder) %>%
      min(),
    60)

  testthat::expect_equal(
    x_out %>%
      dplyr::filter(.data$aldersklasse == "60-69") %>%
      dplyr::pull(.data$alder) %>%
      max(),
    69)

  # sjekk feilmelding dersom var har feil format
  testthat::expect_error(
    ablanor::utlede_aldersklasse(data.frame(alder = "a")))

  testthat::expect_error(
    ablanor::utlede_aldersklasse(data.frame(alder = "1")))

})




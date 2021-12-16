#Test utlede alder ----
testthat::test_that("utlede_alder() works", {

  x <- data.frame(
    birth_date = as.Date(c(rep("1951-12-30", 4), NA,
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
      dplyr::pull(.data$alder),
    c(69, 69, 70, -50))

  # Forventer feilmedling dersom en av variablene mangler i datasettet
  testthat::expect_error(
    ablanor::utlede_alder(df = data.frame(tull1 = 1, tull2 = 2)))

  # Forventer feilmelding dersom ikke dato-variabler
  testthat::expect_error(
    ablanor::utlede_alder(df = data.frame(birth_date = "1951-12-30",
                                          dato_pros = "2021-12-30")))


})


#Test utlede_alder_75 ----
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

# Test aldersklasse ----
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




# Test BMImanual ----
testthat::test_that("Utlede BMI klasse fungerer", {


  df <- data.frame(
    vekt = c(80, 70, 100, 50, 90, NA, 80),
    hoyde = c(150, 160, 170, 180, 190, 160, NA))

  df_out <- ablanor::utlede_bmi(df)

  testthat::expect_true(all(
    names(df_out) %in% c("hoyde", "vekt", "bmi_manual")
  ))

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(!is.na(.data$bmi_manual)) %>%
      dplyr::pull(bmi_manual),
    c(35.56, 27.34, 34.60, 15.43, 24.93)
  )



})
# test BMI- klasse ----
testthat::test_that("Utlede BMI klasse fungerer", {


  df <- data.frame(bmi_manual = c(NA, seq(from = 15, to = 41, by = 0.1), 220))
  df_out <- ablanor::utlede_bmi_klasse(df)


  testthat::expect_true(all(
    names(df_out) %in% c("bmi_manual", "bmi_klasse", "bmi_over35")
  ))


  #  Forventer min og maks for disse klassene:
  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_klasse == "Undervekt") %>%
      dplyr::pull(.data$bmi_manual) %>%
      min(),
    15.0)

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_klasse == "Undervekt") %>%
      dplyr::pull(.data$bmi_manual) %>%
      max(),
    18.4)

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_klasse == "Fedme grad I") %>%
      dplyr::pull(.data$bmi_manual) %>%
      min(),
    30.0)

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_klasse == "Fedme grad I") %>%
      dplyr::pull(.data$bmi_manual) %>%
      max(),
    34.9)

  # Forventer klasse ugyldig dersom BMI har svært høy verdi
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$bmi_manual >= 100) %>%
      dplyr::pull(.data$bmi_klasse) == "ugyldig"))

  # Forventer klasse NA dersom BMI mangler
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$bmi_manual)) %>%
      dplyr::pull(.data$bmi_klasse) %>%
      is.na()))

  # forventer min og max verdi for bmi_over35
  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_over35 == "BMI >=35") %>%
      dplyr::pull(.data$bmi_manual) %>%
      min(),
    35.0)

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(.data$bmi_over35 == "BMI <35") %>%
      dplyr::pull(.data$bmi_manual) %>%
      max(),
    34.9)


  # forventer feilmelding dersom bmi mangler
  testthat::expect_error(
    ablanor::utlede_bmi_klasse(df = data.frame(tull = 1:5))
  )
})



# Test tidsvariabler ----

testthat::test_that("Utlede tidsvariabler fungerer", {

  df <- data.frame(
    dato_pros = as.Date(c("2021-10-05",
                          "2020-11-30",
                          "1998-01-16",
                          "2020-07-07",
                          NA_character_,
                          "2021-15-60"),
                        format = "%Y-%m-%d"))
  df_out <- ablanor::utlede_tidsvariabler(df)

  testthat::expect_equal(
    names(df_out),
    c("dato_pros", "aar", "maaned_nr", "maaned")
  )

  testthat::expect_true(
    df_out %>%
      dplyr::filter(is.na(.data$dato_pros)) %>%
      nrow() == 2)

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$dato_pros)) %>%
      dplyr::pull(.data$maaned) %>%
      is.na()))

  testthat::expect_equal(
    df_out %>%
      dplyr::filter(!is.na(.data$dato_pros)) %>%
      dplyr::pull(.data$maaned) %>%
      as.character(),
    c("2021-10", "2020-11", "1998-01", "2020-07"))

  testthat::expect_error(
    ablanor::utlede_tidsvariabler(df = data.frame(dette_er_feil_navn = 1)))
})






# Test AFLI - kategori ----
testthat::test_that("utlede_kateg_afli_aryt_i48 fungerer", {

  df <- data.frame(
    forlopstype = c(NA, 2, rep(1, 8)),
    aryt_i48_0 = c(0, 1, NA, 0, 0, 0, 0, 0, 1, 1),
    aryt_i48_1 =  c(0, 0, 0, NA, 0, 1, 1, 1, 0, 0),
    aryt_i48_1_underkat = c(NA, NA, NA, NA, NA, 1, 1, 2, NA, NA)
  )


  df_out <- ablanor::utlede_kateg_afli_aryt_i48(df)

  testthat::expect_equal(
    names(df_out),
    c("forlopstype", "aryt_i48_0", "aryt_i48_1", "aryt_i48_1_underkat",
      "kategori_afli_aryt_i48"))


  # Forventede verdier:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 1,
                    .data$aryt_i48_0 == 1) %>%
      dplyr::pull(.data$kategori_afli_aryt_i48) ==
      "AFLI-ICD 48.0 Paroksymal atrieflimmer"
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 1,
                    .data$aryt_i48_1 == 1,
                    .data$aryt_i48_1_underkat == 1) %>%
      dplyr::pull(.data$kategori_afli_aryt_i48) ==
      "AFLI-ICD 48.1 Persisterende atrieflimmer"
  ))


  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 1,
                    .data$aryt_i48_1 == 1,
                    .data$aryt_i48_1_underkat == 2) %>%
      dplyr::pull(.data$kategori_afli_aryt_i48) ==
      "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer"
  ))


  # Forventer NA her:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(is.na(.data$forlopstype == 1) |
                      .data$forlopstype != 1) %>%
      dplyr::pull(.data$kategori_afli_aryt_i48)  %>%
      is.na()
  ))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$forlopstype == 1 &
          (.data$aryt_i48_0 == 0 | is.na(.data$aryt_i48_0)) &
          (.data$aryt_i48_1 == 0 | is.na(.data$aryt_i48_1))) %>%
      dplyr::pull(.data$kategori_afli_aryt_i48)  %>%
      is.na()
  ))

  # Forventer 5 som er NA
  testthat::expect_equal(
    df_out %>%
      dplyr::filter(is.na(.data$kategori_afli_aryt_i48)) %>%
      nrow(),
    5)

  # Forventer feilmelding
  testthat::expect_error(
    ablanor::utlede_kateg_afli_aryt_i48(df = data.frame(tester_feil = NA))
  )





})


# Test VT Kardiomyopati ----
testthat::test_that("utlede kardiomyopati fungerer", {



  df <- data.frame(
    forlopstype = c(NA, 1, rep(2, 8)),
    kardiomyopati =  c(NA, NA, 9, 0, rep(1, 6)),
    type_kardiomyopati = c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 99)
  )


  df_out <- ablanor::utlede_kardiomyopati(df)


  # Forventede variabel-navn
  testthat::expect_equal(
    names(df_out),
    c("forlopstype", "kardiomyopati", "type_kardiomyopati",
      "kategori_vt_kardiomyopati"))

  # Forventer NA her
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype != 2 | is.na(.data$forlopstype)) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) %>%
      is.na())
  )

  # Forventer verdier:
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 2,
                    .data$kardiomyopati == 0) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) ==
      "Uten kardiomyopati"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 2,
                    .data$kardiomyopati == 1,
                    .data$type_kardiomyopati == 1) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) ==
      "Iskemisk KM (ICM)"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 2,
                    .data$kardiomyopati == 1,
                    .data$type_kardiomyopati == 2) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) ==
      "Dilatert KM (DCM)"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 2,
                    .data$kardiomyopati == 1,
                    !(.data$type_kardiomyopati %in% 1:2)) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) ==
      "Annen KM"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype == 2,
                    .data$kardiomyopati == 9) %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) ==
      "Ukjent om kardiomyopati"))


  testthat::expect_equal(
    df_out %>%
      dplyr::pull(.data$kategori_vt_kardiomyopati) %>%
      as.character(),

    c(NA_character_, NA_character_, "Ukjent om kardiomyopati",
      "Uten kardiomyopati", "Iskemisk KM (ICM)", "Dilatert KM (DCM)",
      "Annen KM", "Annen KM", "Annen KM", "Annen KM"))
})


# Test AFLI hjertesvikt redusert EF ----
testthat::test_that("AFLI hjertesvik redusert EF", {

  df <- data.frame(
    forlopstype = c(NA, 2, rep(1, 8)),
    hjertesvikt = c(NA, 1, NA, 0, 0, 9, 9, 1, 1, 1),
    ejekfrak = c(NA, 9, 1, 9, 1, 1, 3, 1, 2, 3))

  df_out <- ablanor::utlede_hjertesvikt_redusert_ef(df)


  # Forventer navn
  testthat::expect_equal(
    names(df_out),
    c("forlopstype", "hjertesvikt", "ejekfrak", "kategori_afli_hjsvikt_ef")
  )

  # Forventede verdier
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype ==  1 &
                      (.data$hjertesvikt %in% 1 |
                         .data$ejekfrak %in% 2:3)) %>%
      dplyr::pull(.data$kategori_afli_hjsvikt_ef) ==
      "AFLI-Hjertesvikt og/eller redusert EF"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype ==  1 &
                      !(.data$hjertesvikt %in% 1 |
                          .data$ejekfrak %in% 2:3)) %>%
      dplyr::pull(.data$kategori_afli_hjsvikt_ef) ==
      "AFLI-Verken hjertesvikt eller redusert EF"))

  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(.data$forlopstype !=  1 |
                      is.na(.data$forlopstype)) %>%
      dplyr::pull(.data$kategori_afli_hjsvikt_ef) %>%
      as.character() %>%
      is.na()))

  testthat::expect_equal(
    df_out %>%
      dplyr::pull(.data$kategori_afli_hjsvikt_ef) %>%
      as.character(),

    c(NA_character_, NA_character_,
      rep("AFLI-Verken hjertesvikt eller redusert EF", 4),
      rep("AFLI-Hjertesvikt og/eller redusert EF", 4)))


})

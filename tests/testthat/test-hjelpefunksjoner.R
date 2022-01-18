
# TEst forlopstype kortnavn ----
test_that("Kontroll forløpstype kortnavn", {

  x <- data.frame(forlopstype = rep(1:4, 2))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "AFLI", "forlopstype"] == 1))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "VT", "forlopstype"] == 2))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "SVT", "forlopstype"] == 3))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "EFU", "forlopstype"] == 4))

  x <- data.frame(forlopstype = c(0, 5))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(all(is.na(x_kb$forlopstype_tekst)))

  x <- data.frame(forlopstype = c(rep(1:4, 2), NA))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(is.na(x_kb$forlopstype_tekst)[9])


  x <- data.frame(forlopstype = c(1, 2, 3, 4, "Total"),
                  n = c(2, 7, 5, 1, 15))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = TRUE)
  expect_true(x_kb$forlopstype_tekst[5] == "Totalt")


})

test_that("Kontroll forløpstype kortnavn for langtnavn", {

  x <- data.frame(forlopstype = rep(1:4, 2))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x,
                                                 total = FALSE,
                                                 langtnavn = TRUE)
  expect_true(all(
    x_kb %>%
      dplyr::filter(.data$forlopstype == 1) %>%
      dplyr::pull(.data$forlopstype_tekst) ==
      "Atrieflimmer/atypisk flutter (AFLI)"))

  expect_true(all(
    x_kb %>%
      dplyr::filter(.data$forlopstype == 2) %>%
      dplyr::pull(.data$forlopstype_tekst) ==
      "Ventrikkeltakykardi (VT)"))


  expect_true(all(
    x_kb %>%
      dplyr::filter(.data$forlopstype == 3) %>%
      dplyr::pull(.data$forlopstype_tekst) ==
      "Supraventrikulær takykardi (SVT)"))

  expect_true(all(
    x_kb %>%
      dplyr::filter(.data$forlopstype == 4) %>%
      dplyr::pull(.data$forlopstype_tekst) ==
      "Elektrofysiologisk undersøkelse (EFU)"))



  x <- data.frame(forlopstype = c(1, 2, 3, 4, "Total"),
                  n = c(2, 7, 5, 1, 15))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x,
                                                 total = TRUE,
                                                 langtnavn = TRUE)
  expect_true(x_kb$forlopstype_tekst[5] == "Totalt")


})

# test string pad ----
test_that("String pad works", {
  expect_equal(ablanor::string_pad(string_vector = c("Ja", "Nei", "Kanskje")),
               c("  Ja   ", "  Nei  ", "Kanskje"))
})







# Test make_sorters
testthat::test_that("Make_sorters works", {


  df <- data.frame(kjonn = factor(x = c("M", "K", "M", "M"),
                                  levels = c("M", "K"),
                                  ordered = TRUE),
                   alder = 50:53,
                   suksess = factor(x = c(9, 1, 0, NA),
                                    levels = c(1, 0, 9),
                                    labels = c("Ja", "Nei", "Ukjent"),
                                    ordered = TRUE),
                   kommentar = c("Cola til folket !",
                                 "Aha, alfabetisk",
                                 "Dersom ikke faktor",
                                 "Bæh, sortering"))

  testthat::expect_equal(
    ablanor::make_sorters(df),
    paste0(
      "function(attr) ",
      "{\nvar sortAs = $.pivotUtilities.sortAs;\n",
      "if (attr == \"kjonn\") { return sortAs([\"M\" , \"K\"]); }\n",
      "if (attr == \"suksess\") { return sortAs(",
      "[\"Ja\" , \"Nei\" , \"Ukjent\"]); }\n}"
    ))

  testthat::expect_equal(
    NULL,
    ablanor::make_sorters(df = data.frame()),
  )

  testthat::expect_equal(
    NULL,
    ablanor::make_sorters(df = df %>% dplyr::select(.data$kommentar)),
  )


})

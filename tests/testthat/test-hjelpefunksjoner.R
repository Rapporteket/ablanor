test_that("Kontroll forløpstype kortnavn", {

  x <- data.frame(forlopstype = rep(1:4, 2))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "AFLI", "forlopstype"] ==1))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "VT", "forlopstype"] == 2))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "SVT", "forlopstype"] == 3))
  expect_true(all(x_kb[x_kb$forlopstype_tekst == "EFU", "forlopstype"] == 4))

  x <- data.frame(forlopstype = c(0,5))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(all(is.na(x_kb$forlopstype_tekst)))

  x <- data.frame(forlopstype = c(rep(1:4, 2), NA))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = FALSE)
  expect_true(is.na(x_kb$forlopstype_tekst)[9])


  x <- data.frame(forlopstype = c(1, 2, 3, 4,"Total"),
                  n = c(2, 7, 5, 1, 15))
  x_kb <- ablanor::legg_til_forlopstype_kortnavn(x, total = TRUE)
  expect_true(x_kb$forlopstype_tekst[5] == "Totalt")


})

test_that("String pad works", {
  expect_equal(ablanor::string_pad(string_vector = c("Ja", "Nei", "Kanskje")),
               c("  Ja   ", "  Nei  ", "Kanskje"))
})

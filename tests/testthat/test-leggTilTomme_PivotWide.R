test_that("Expansion works", {
  x <- data.frame(maaned = rep(c("jan", "feb", "mars", "april"), 2),
                  legend = factor(x = c(rep("en",4) ,rep("to",4)),
                              levels = c("en", "to", "tre")),
                  value = paste0("1/", 1:8))

  expect_equal(as.character(
      ablanor::leggTilTomme_PivotWide(x, remove_NAcols = FALSE)[1,4]),
      " -- ")

  expect_equal(colnames(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = FALSE)),
    c("maaned", "en", "to", "tre"))


  x <- data.frame(maaned = rep(c("jan", "feb", "mars", "april", NA), 2),
                  legend = factor(x = c(rep("en",5),NA ,rep("to",4)),
                                  levels = c("en", "to", "tre", NA),
                                  exclude = NULL),
                  value = c(paste0("1/", 1:9), NA))

  expect_true("NA" %in%
                colnames(ablanor::leggTilTomme_PivotWide(
                  x, remove_NAcols = FALSE)))

  expect_true(! "NA" %in%
                colnames(
                  ablanor::leggTilTomme_PivotWide(x, remove_NAcols = TRUE)))
  expect_true(NA_character_ %in%
                (ablanor::leggTilTomme_PivotWide(
                  x, remove_NAcols = FALSE)$maaned))


  x <- data.frame(maaned = factor(x = rep(c("feb", "april", "mars", "jan", NA),
                                          2),
                                  levels = c("jan", "feb", "mars",
                                             "april", "mai", NA),
                                  exclude = NULL),
                  legend = factor(x = c(rep("tre",5),NA ,rep("to",4)),
                                  levels = c("en", "to", "tre", NA),
                                  exclude = NULL),
                  value = c(paste0("1/", 1:9), NA))

  expect_equal(colnames(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = TRUE)),
    c("maaned", "en", "to", "tre"))

  expect_true(is.na(as.character(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = TRUE)$maaned[6])))
})

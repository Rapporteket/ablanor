test_that("Expansion works", {

  x <- data.frame(maaned = rep(c("jan", "feb", "mars", "april"), 2),
                  legend = factor(x = c(rep("en", 4), rep("to", 4)),
                                  levels = c("en", "to", "tre")),
                  value = paste0("1/", 1:8))

  expect_true(all(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols = FALSE) %>%
      dplyr::pull(tre) %>%
      as.character() == " -- "))

  expect_equal(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = FALSE) %>%
      names(),
    c("maaned", "en", "to", "tre")
  )


  x <- data.frame(maaned = rep(c("jan", "feb", "mars", "april", NA), 2),
                  legend = factor(x = c(rep("en", 5), NA, rep("to", 4)),
                                  levels = c("en", "to", "tre", NA),
                                  exclude = NULL),
                  value = c(paste0("1/", 1:9), NA))

  expect_true(ablanor::leggTilTomme_PivotWide(x, remove_NAcols = FALSE) %>%
                dplyr::select("NA") %>%
                ncol() == 1
  )

  expect_equal(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols = TRUE) %>% names(),
    c("maaned", "en", "to", "tre")
  )

  expect_true(ablanor::leggTilTomme_PivotWide(x, remove_NAcols = FALSE) %>%
                dplyr::filter(is.na(maaned)) %>%
                nrow() ==1
  )


  x <- data.frame(maaned = factor(x = rep(c("feb", "april", "mars", "jan", NA),
                                          2),
                                  levels = c("jan", "feb", "mars",
                                             "april", "mai", NA),
                                  exclude = NULL),
                  legend = factor(x = c(rep("tre", 5), NA, rep("to", 4)),
                                  levels = c("en", "to", "tre", NA),
                                  exclude = NULL),
                  value = c(paste0("1/", 1:9), NA))

  expect_equal(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = TRUE) %>%
      names(),
    c("maaned", "en", "to", "tre")
  )

  expect_equal(
    ablanor::leggTilTomme_PivotWide(x, remove_NAcols  = TRUE) %>%
      dim(),
    c(6, 4)
  )

})

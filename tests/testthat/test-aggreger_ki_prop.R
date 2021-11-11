testthat::test_that("Tester aggreger ki_prop", {

  x <- data.frame(ki_krit_teller = c(T, F, T, F, F,  NA),
                  ki_krit_nevner = c(T, T, T, T, F,  F),
                  grpVar = rep(c("a", "b"), 3))


  # Uten grupperingsvariabel:
  x_out <- ablanor::aggreger_ki_prop(x, forenklet = TRUE)

  testthat::expect_true(x_out$est == 0.5)
  testthat::expect_true(x_out$ki_teller == 2)
  testthat::expect_true(x_out$ki_nevner == 4)
  testthat::expect_equal(
    x_out %>% names(),
    c("est", "ki_teller", "ki_nevner", "konfint_nedre", "konfint_ovre"))


  testthat::expect_equal(x %>%
                           dplyr::group_by(.data$grpVar) %>%
                           ablanor::aggreger_ki_prop(., forenklet = TRUE) %>%
                           dim(),
                         c(2, 6)
  )



  # Med grupperingsvariabel
  testthat::expect_true(
    "data.frame" %in% class(x %>%
                              dplyr::group_by(.data$grpVar) %>%
                              ablanor::aggreger_ki_prop(., forenklet = TRUE)))

  x_out <- x %>%
    dplyr::group_by(.data$grpVar) %>%
    ablanor::aggreger_ki_prop(., forenklet = TRUE)


  expect_equal(x_out$est, c(1, 0))
  expect_equal(x_out$ki_teller, c(2, 0))
  expect_equal(x_out$ki_nevner, c(2, 2))
  expect_equal(x_out$grpVar, c("a", "b"))

})

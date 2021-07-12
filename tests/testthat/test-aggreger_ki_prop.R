test_that("Tester aggreger ki_prop", {
  testthat::skip("Test needs fixing!")
  x <- data.frame(ki_krit_teller = c(T, F, T, F, F,  NA),
                  ki_krit_nevner = c(T, T, T, T, F,  F),
                  grpVar = rep(c("a", "b"),3))
  expect_true(aggreger_ki_prop(x, forenklet = TRUE)$est == 0.5)
  expect_true(aggreger_ki_prop(x, forenklet = TRUE)$ki_teller == 2)
  expect_true(aggreger_ki_prop(x, forenklet = TRUE)$ki_nevner == 4)
  expect_equal(names(aggreger_ki_prop(x, forenklet = TRUE)),
               c("est", "ki_teller", "ki_nevner",
                 "konfint_nedre", "konfint_ovre"))


  expect_equal(dim(x %>%
                     dplyr::group_by(grpVar) %>%
                     aggreger_ki_prop(., forenklet = TRUE)),
               c(2,6))


  expect_true("data.frame" %in% class(x %>%
                                        dplyr::group_by(grpVar) %>%
                                        aggreger_ki_prop(., forenklet = TRUE)))


  x_out <- x %>%
    dplyr::group_by(grpVar) %>%
    aggreger_ki_prop(., forenklet = TRUE)


 expect_equal(x_out$est, c(1,0))
 expect_equal(x_out$ki_teller, c(2,0))
 expect_equal(x_out$ki_nevner, c(2,2))
 expect_equal(x_out$grpVar, c("a", "b"))

})

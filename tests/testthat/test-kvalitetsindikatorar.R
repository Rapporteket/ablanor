test_that("Teste kvalitestindikatorer - Ki_komplikajsoner", {
  x <- data.frame(komp_janei = c(0,0,0,1,0,1, 2, NA))

  expect_equal(dim(ki_komplikasjonar(x)),
               c(8,3))
  expect_equal(colnames(ki_komplikasjonar(x)),
               c("komp_janei", "ki_krit_teller", "ki_krit_nevner"))
  expect_true(is.na(ki_komplikasjonar(x)$ki_krit_teller[8]))
  expect_true(sum(ki_komplikasjonar(x)$ki_krit_teller, na.rm = TRUE) == 2)
  expect_true(sum(ki_komplikasjonar(x)$ki_krit_nevner, na.rm = TRUE) == 6)
  expect_true(class(ki_komplikasjonar(x)) == "data.frame")

})

test_that("Function returns data frame when non-valid setId", {
  expect_true("NULL" %in% class(getPivotDataSet(setId = "testSomeRandomName")))
})

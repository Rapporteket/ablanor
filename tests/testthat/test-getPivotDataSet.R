test_that("Function returns data frame when non-valid setId", {
  expect_true("NULL" %in% class(getPivotDataSet(setId = "testSomeRandomName")))
})

test_that("error is provided on impossible combo of reshId and allData", {
  expect_error(
    getPivotDataSet(
      setId = "pros_patient", registryName = "ablanor", session = list(),
      singleRow = TRUE, reshId = NULL, allData = FALSE
    )
  )
})

# Test that require database infrastructure may be performed in test-getData

test_that("getKodebokData works", {
  kb <- ablanor::getKodebokData()

  expect_true(all(
                c("fysisk_feltnavn",
                  "listeverdier",
                  "listetekst",
                  "type") %in% names(kb)))

  expect_true(
    kb %>% is.data.frame()
  )
})

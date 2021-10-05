test_that("getKodebokData works", {
  kb <- ablanor::getKodebokData()

  expect_true(all(
    kb %>% names() %in%
                c("fysisk_feltnavn",
                  "listeverdier",
                  "listetekst",
                  "type")))

  expect_true(
    kb %>% is.data.frame()
  )
})

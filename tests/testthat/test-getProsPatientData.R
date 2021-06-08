test_that("Test innlesing prosedyre-pasient data", {

  # @ todo : format ut er dataframe
  #  inneholder utledete variablr
  # kontroll på utledete variabler
})

test_that("When 'pros_patient' data is imported, test new created variables", {

  # DISSE TESTENE FUNGERER IKKE - LAGE TEST-DATASETT ? ELLER LAGE EGNE
  # FUNKSJONER utlede_alder_75, utlede_bmi_35, osv ?
  # x <- getProsPatientData(registryName = registryName,
  #                         singleRow = FALSE,
  #                         tekstVars = FALSE)
  # expect_true(max(x[x$alder_75 == "<75", "alder"]) < 75)
  # expect_true(min(x[x$alder_75 == ">=75", "alder"]) >= 75)
  #
  # expect_true(max(x[x$bmi_over35 == "BMI <35", "bmi_manual"]) < 35)
  # expect_true(min(x[x$bmi_over35 == "BMI >=35", "bmi_manual"]) >= 35)
  #
  # expect_true(all(is.na(x[x$forlopstype != 2, "kategori_vt_kardiomyopati"])))
  # expect_true(all(!is.na(x[x$forlopstype == 2, "kategori_vt_kardiomyopati"])))
  #
  # expect_true(all(is.na(x[x$forlopstype != 1, "kategori_afli_hjsviktEF"])))
  # expect_true(all(!is.na(x[x$forlopstype == 1, "kategori_afli_hjsviktEF"])))
})

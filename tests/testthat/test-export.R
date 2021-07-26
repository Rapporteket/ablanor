test_that("error when dont't know what", {
  expect_error(getGithub("harrypotter", "wand"))
})

test_that("contributors are provided", {
  expect_equal(class(getGithub("contributors", "rapbase")), "character")
})

test_that("key can be provided", {
  expect_true(grepl("ssh-rsa", getGithub("keys", "areedv")))
})

test_that("a abbreviated named list can be provided from key(s)", {
  testKey <- "ssh-rsa averylongstring"
  testName <- "ssh-rsa ...ngstring"
  expect_equal(names(selectListPubkey(testKey)), testName)

})

# rest of test on db export will have to be performed where a db is present

# tests only performed on Windows where mysqldump and gzip is not present
if (.Platform$OS.type == "windows" && Sys.which("mysqldump") == "") {
  expect_error(exportDb("reg", session = NULL))
}
if (.Platform$OS.type == "windows" && Sys.which("gzip") == "") {
  expect_error(exportDb("reg", session = NULL))
}

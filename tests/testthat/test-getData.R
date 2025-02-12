# For these tests to work locally make sure an instance of mysql server is
# available and that the necessary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# When run at Github Actions build servers [USER] must be set to 'actions' and
# with an empty password (as also assumed in the above localhost example).
# See also .github/workflows/R-CMD-check.yml

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# preserve initial state
config_path <- Sys.getenv("R_RAP_CONFIG_PATH")


test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             host = Sys.getenv("MYSQL_HOST"),
                             user = Sys.getenv("MYSQL_USER"),
                             password = Sys.getenv("MYSQL_PASSWORD"),
                             bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testDb;")
  RMariaDB::dbDisconnect(con)
}

# make temporary config
test_config <- paste0(
  "testReg:",
  "\n  host : ", Sys.getenv("MYSQL_HOST"),
  "\n  name : testDb",
  "\n  user : ", Sys.getenv("MYSQL_USER"),
  "\n  pass : ", Sys.getenv("MYSQL_PASSWORD"),
  "\n  disp : ephemaralUnitTesting\n"
)
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "dbConfig.yml"))
writeLines(test_config, cf)
close(cf)

# make queries for creating tables
fc <- file(system.file("testDb.sql", package = "ablanor"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

test_that("relevant test database and tables can be made", {
  check_db()
  con <- rapbase::rapOpenDbConnection("data")$con
  for (i in seq_len(length(queries))) {
    expect_equal(class(RMariaDB::dbExecute(con, queries[i])), "integer")

  }
  rapbase::rapCloseDbConnection(con)
})

# onto main testing
test_that("hospital name can be read from db", {
  check_db()
  con <- rapbase::rapOpenDbConnection("data")$con
  query <- paste("INSERT INTO friendlycentre SET ID=1, CENTRESHORTNAME='s1',",
                 "FRIENDLYNAME='friendly1';")
  RMariaDB::dbExecute(con, query)
  expect_equal(class(getHospitalName(1)), "character")
  expect_equal(getHospitalName(1), "friendly1")
  expect_equal(getHospitalName(1, shortName = TRUE), "s1")
  expect_warning(getHospitalName(2))
  rapbase::rapCloseDbConnection(con)
})

test_that("name-id mapping can be obtained", {
  check_db()
  con <- rapbase::rapOpenDbConnection("data")$con
  expect_equal(class(getNameReshId()), "data.frame")
  expect_equal(class(getNameReshId(asNamedList = TRUE)),
               "list")
  rapbase::rapCloseDbConnection(con)
})

test_that("tables can be dumped", {
  check_db()
  con <- rapbase::rapOpenDbConnection("data")$con
  expect_equal(class(
    getDataDump("basereg", Sys.Date(), Sys.Date(), userRole = "SC", reshID = NULL)
  ), "data.frame")
  expect_equal(class(
    getDataDump("friendlycentre", Sys.Date(), Sys.Date(),userRole = "SC", reshID = NULL)
  ), "data.frame")
  expect_equal(class(
    getDataDump("mce", Sys.Date(), Sys.Date(), userRole = "SC", reshID = NULL)
  ), "data.frame")
  expect_equal(class(
    getDataDump("patientlist", Sys.Date(), Sys.Date(), userRole = "SC", reshID = NULL)
  ), "data.frame")
  expect_equal(class(
    getDataDump("pros", Sys.Date(), Sys.Date(), userRole = "SC", reshID = NULL)
  ), "data.frame")
  expect_error(
    getDataDump("notATable", Sys.Date(), Sys.Date(), userRole = "SC", reshID = NULL)
  )
  rapbase::rapCloseDbConnection(con)
})

test_that("rand12 data can be read from db", {
  check_db()
  expect_equal(class(getRand12(singleRow = FALSE, userRole = "SC")), "list")
  expect_equal(class(getRand12(singleRow = TRUE, userRole = "SC")), "list")
})




# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection("data")$con
  RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)

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

withr::with_envvar(
  new = c(
    "R_RAP_CONFIG_PATH" = tempdir(),
    "FALK_EXTENDED_USER_RIGHTS" = "[{\"A\":80,\"R\":\"LC\",\"U\":1},{\"A\":80,\"R\":\"SC\",\"U\":2},{\"A\":81,\"R\":\"LC\",\"U\":2}]",
    "FALK_APP_ID" = "80"
  ),
  code = {
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
    base_config <- paste0(
      "r:",
      "\n  raplog :",
      "\n    archiveDir : logArchive",
      "\n    eolDays : 730",
      "\n    target: file",
      "\n    key: raplogTest\n",
      "\n",
      "\n  autoReport:",
      "\n    target: file",
      "\n    key: autoreport\n",
      "\n",
      "\n  testUser:",
      "\n    user : ttester",
      "\n    groups : ablanor",
      "\n    role : LC",
      "\n    resh_id : 1",
      "\n    email : ttester@reg.no",
      "\n    full_name : Tore Tester",
      "\n    phone : 0123456789\n"
    )
    test_config <- paste0(
      "ablanor:",
      "\n  host : ", Sys.getenv("MYSQL_HOST"),
      "\n  name : testDb",
      "\n  user : ", Sys.getenv("MYSQL_USER"),
      "\n  pass : ", Sys.getenv("MYSQL_PASSWORD"),
      "\n  disp : ephemaralUnitTesting\n"
    )

    cf <- file(file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml"))
    writeLines(base_config, cf)
    close(cf)
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

    test_that("an org can be added to db", {
      check_db()
      con <- rapbase::rapOpenDbConnection("data")$con
      query <- paste("INSERT INTO friendlycentre SET ID=1, CENTRESHORTNAME='s1',",
                     "FRIENDLYNAME='friendly1';")
      RMariaDB::dbExecute(con, query)
      expect_equal(class(getHospitalName(1)), "character")
      expect_equal(getHospitalName(1), "friendly1")
      expect_equal(getHospitalName(1, shortName = TRUE), "s1")
      rapbase::rapCloseDbConnection(con)
    })

    # rapbase modules are already tested. For now, just make sure the server run
    # by dummy class test of auto report list
    test_that("server can run", {
      check_db()
      shiny::testServer(app = app_server, {
        print("CLASS SUBREPORTS")
        print(class(subReports))
        print("######\n")
        expect_equal(class(subReports), "list")
      })
    })

    # remove test db
    if (is.null(check_db(is_test_that = FALSE))) {
      con <- rapbase::rapOpenDbConnection("data")$con
      RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
      rapbase::rapCloseDbConnection(con)
    }
  }
)

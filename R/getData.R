#' Load data for ablanor
#'
#' Functions for loading data needed by ablanor from a database or local files.
#' When local files are used the environmental variable
#' \emph{filbane_ablanor_test} must be set to the path of the file where data
#' are found
#'
#' \code{getNameReshId()} returns a mapping of organization name and id in the
#' form of columns named \emph{name} and \emph{id}. Optionally this function
#' can also return a list of named values (ids), \emph{e.g.} for use in shiny
#' selection lists.
#'
#' @param registryName Character string defining the registry name.
#' @param tableName Character string with name of database table
#' @param fromDate Character string of format YYYY-MM-DD with start date. Value
#' NULL if no filter on date.
#' @param toDate Character string of format YYYY-MM-DD with end date. Value
#' NULL if no filter on date.
#' @param asNamedList Logical whether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#' @param singleRow Logical if only one row from the table is to be provided.
#' Default value is FALSE.
#' @param reshId Integer dummy/placeholder organization id
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by \code{reshId}), "SC" has access to national data.
#' @param shortName boolean. Default value FALSE and "friendlyname" is returned.
#' If TRUE shortname is returned.
#' @param ... Optional arguments to be passed to the function.
#'
#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#' @name getData
#' @aliases getDataDump getNameReshId getHospitalName getRand12 getProsPatient
NULL

#' @rdname getData
#' @export
getDataDump <- function(registryName, tableName, fromDate, toDate,
                        reshId = NULL, userRole, ...) {
  . <- ""
  stopifnot(tableName %in% c("basereg",
                             "friendlycentre",
                             "mce",
                             "patientlist",
                             "pros",
                             "followup",
                             "rand12",
                             "pros_patient_followup",
                             "kodeboken"))

  if (!tableName %in% c("pros_patient_followup", "kodeboken")) {

    query <- paste("SELECT * FROM", tableName)
    condition <- ""

    if (tableName == "basereg") {
      condition <- paste0(" WHERE DATO_BAS >= '", fromDate,
                          "' AND DATO_BAS < '", toDate, "'")

    } else if (tableName == "mce") {
      condition <- paste0(" WHERE TSCREATED >= '", fromDate,
                          "' AND TSCREATED < '", toDate, "'")

    } else if (tableName == "patientlist") {
      condition <- paste0(" WHERE REGISTERED_DATE >= '", fromDate,
                          "' AND REGISTERED_DATE < '", toDate, "'")

    } else if (tableName == "pros") {
      condition <- paste0(" WHERE DATO_PROS >= '", fromDate,
                          "' AND DATO_PROS < '", toDate, "'")

    } else if (tableName == "followup") {

    } else if (tableName == "rand12") {
      condition <- paste0(" WHERE DATO_RAND12 >= '", fromDate,
                          "' AND DATO_RAND12 < '", toDate, "'")
    }

    if (userRole != "SC" & !tableName %in% "friendlycentre") {
      condition <- paste0(condition, " AND CENTREID = '", reshId, "'")
    }

    query <- paste0(query, condition, ";")

    if ("session" %in% names(list(...))) {
      #nocov start
      rapbase::repLogger(session = list(...)[["session"]],
                         msg = paste("AblaNor data dump:\n", query))
      #nocov end
    }

    rapbase::loadRegData(registryName, query)

  } else if (tableName == "pros_patient_followup") {
    dat <- ablanor::getProsPatientData(registryName = registryName,
                                       singleRow = FALSE,
                                       reshId = reshId,
                                       userRole = userRole,
                                       fromDate = fromDate,
                                       toDate = toDate)


    dat %>% ablanor::legg_til_sykehusnavn(df = ., short = FALSE)



  } else if (tableName == "kodeboken") {
    ablanor::getKodebokData()
  }
}


#' @rdname getData
#' @export
getNameReshId <- function(registryName, asNamedList = FALSE) {

  query <- "
SELECT
  CENTRESHORTNAME AS name,
  ID AS id
FROM
  friendlycentre
WHERE
  CENTRESHORTNAME NOT LIKE 'Test%'
GROUP BY
  CENTRESHORTNAME,
  ID;"

  res <- rapbase::loadRegData(registryName, query)

  if (asNamedList) {
    res <- stats::setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
}

#' @rdname getData
#' @export
getHospitalName <- function(registryName, reshId, shortName = FALSE) {

  if (shortName) {
    dbField <- "CENTRESHORTNAME"
  } else {
    dbField <- "FRIENDLYNAME"
  }

  query <- paste0("
SELECT
  ", dbField, "
FROM
  friendlycentre
WHERE
  ID = ", reshId, ";")

  name <- rapbase::loadRegData(registryName, query)[1, ]

  if (is.na(name)) {
    warning(paste("Resh ID", reshId, "did not match any names!"))
  }

  name
}


#' @rdname getData
#' @export
getRand12 <- function(registryName, singleRow,
                      reshId = NULL, userRole, ...) {


  condition <- ""
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }

  query_procedure <- paste0("SELECT MCEID, CENTREID FROM pros", condition)
  query_rand12 <- paste0("SELECT * FROM rand12", condition)

  if (singleRow) {
    msg_procedure <- "Query metadata for merged dataset, procedure"
    msg_rand12 <- "Query metadata for merged dataset, rand12"
    query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
    query_rand12 <- paste0(query_rand12, "\nLIMIT\n  1;")
  } else {
    msg_procedure <- "Query data for merged dataset, procedure"
    msg_rand12 <- "Query data for merged dataset, rand12"
    query_procedure <- paste0(query_procedure, ";")
    query_rand12 <- paste0(query_rand12, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_procedure)
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_rand12)
    d_rand12 <- rapbase::loadRegData(registryName, query_rand12)
    # nocov end
  } else {
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    d_rand12 <- rapbase::loadRegData(registryName, query_rand12)

  }

  list(pros = d_pros, rand12 = d_rand12)
}


#' @rdname getData
#' @export
getBasereg <- function(registryName, singleRow,
                       reshId = NULL, userRole, ...) {

  condition <- ""
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }

  query_procedure <- paste0("SELECT MCEID, CENTREID FROM pros", condition)
  query <- paste0("SELECT * FROM basereg", condition)

  if (singleRow) {
    msg_procedure <- "Query metadata for merged dataset, procedure"
    query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
    msg <- "Query metadata for basereg"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg_procedure <- "Query data for merged dataset, procedure"
    query_procedure <- paste0(query_procedure, ";")
    msg <- "Query data for basereg"
    query <- paste0(query, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_procedure)
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_basereg <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    d_basereg <- rapbase::loadRegData(registryName, query)
  }


  list(pros = d_pros,
       basereg = d_basereg)
}


#' @rdname getData
#' @export
getFollowup <- function(registryName, singleRow,
                       reshId = NULL, userRole, ...) {

  condition <- ""
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }

  query_procedure <- paste0("SELECT MCEID, CENTREID, DATO_PROS FROM pros", condition)
  query_mce <-  paste0("SELECT MCEID, PARENTMCEID, MCETYPE, CENTREID FROM mce", condition)
  query <- paste0("SELECT * FROM followup", condition)

  if (singleRow) {
    msg_procedure <- "Query metadata for merged dataset, procedure"
    query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
    msg_mce <- "Query metadata for merged dataset, mce"
    query_mce <- paste0(query_mce, "\nLIMIT\n  1;")
    msg <- "Query metadata for followup"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg_procedure <- "Query data for merged dataset, procedure"
    query_procedure <- paste0(query_procedure, ";")
    msg_mce <- "Query data for merged dataset, mce"
    query_mce <- paste0(query_mce, ";")
    msg <- "Query data for followup"
    query <- paste0(query, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_procedure)
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_mce)
    d_pros <- rapbase::loadRegData(registryName, query_mce)
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_basereg <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_pros <- rapbase::loadRegData(registryName, query_procedure)
    d_mce <- rapbase::loadRegData(registryName, query_mce)
    d_followup <- rapbase::loadRegData(registryName, query)
  }


  list(followup = d_followup,
       pros = d_pros,
       mce = d_mce)
}




#' @rdname getData
#' @export
getProsPatient <- function(registryName, singleRow,
                           reshId = NULL, userRole,
                           fromDate,
                           toDate, ...) {

  if (registryName == "test_ablanor_lokalt") {
    load(file = Sys.getenv("filbane_ablanor_test"), envir = parent.frame())

  } else {

    # SPØRRING ETTER PROSEDYRE-DATA. MED OG UTEN FILTER PÅ DATOER OG
    # LOKALE DATA
    # Med filter på dato, nasjonale tall
    if ((!is.null(fromDate) & !is.null(toDate)) & userRole == "SC") {
      condition_pros <- paste0(" WHERE DATO_PROS >= '", fromDate,
                               "' AND DATO_PROS < '", toDate, "'")

      # Med filter på dato, lokale tall
    } else if ((!is.null(fromDate) & !is.null(toDate)) & userRole != "SC") {
      condition_pros <- paste0(" WHERE DATO_PROS >= '", fromDate,
                               "' AND DATO_PROS < '", toDate, "'",
                               " AND CENTREID = '", reshId, "'")

      #Ingen filter på dato, nasjonale data
    } else if ((is.null(fromDate) | is.null(toDate)) & userRole == "SC") {
      condition_pros <- ""

      #Ingen filter på dato, lokale data
    } else if ((is.null(fromDate) | is.null(toDate)) & userRole != "SC") {
      condition_pros <- paste0(" WHERE CENTREID = '", reshId, "'")
    }
    query_procedure <- paste0("SELECT * FROM pros", condition_pros)


    # SPØRRING ETTER ANDRE TABELLER (BASEREG, PASIENTDATA OG FOLLOWUP)
    # FOR NASJONALE ELLER LOKALE TALL
    # Ingen filter på dato, da vi filtrerer på prosedyre-data
    condition <- ""
    if (userRole != "SC") {
      condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
    }

    query_basereg <- paste0("SELECT * FROM basereg", condition)
    query_followup <- paste0("SELECT * FROM followup")
    query_mce <- paste0(
      "SELECT MCEID, MCETYPE, PATIENT_ID, PARENTMCEID, STATUS FROM mce",
      condition)

    # Patient-list does not have variable 'CENTREID'
    query_patientlist <- paste0("SELECT * FROM patientlist ")




    if (singleRow) {
      msg_basereg <- "Query metadata for merged dataset, basereg"
      msg_procedure <- "Query metadata for merged dataset, procedure"
      msg_mce <- "Query metadata for merged dataset, mce"
      msg_patientlist <- "Query metadata for merged dataset, patientlist"
      msg_followup <- "Query metadata for merged dataset, followup"

      query_basereg <- paste0(query_basereg, "\nLIMIT\n  1;")
      query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
      query_mce <- paste0(query_mce, "\nLIMIT\n  1;")
      query_patientlist <- paste0(query_patientlist, "\nLIMIT\n  1;")
      query_followup <- paste0(query_followup, "\nLIMIT\n  1;")
    } else {
      msg_basereg <- "Query data for merged dataset, basereg"
      msg_procedure <- "Query data for merged dataset, procedure"
      msg_mce <- "Query data for merged dataset, mce"
      msg_patientlist <- "Query data for merged dataset, patientlist"
      msg_followup <- "Query metadata for merged dataset, followup"

      query_basereg <- paste0(query_basereg, ";")
      query_procedure <- paste0(query_procedure, ";")
      query_mce <- paste0(query_mce, ";")
      query_patientlist <- paste0(query_patientlist, ";")
      query_followup <- paste0(query_followup, ";")
    }

    # log db request if shiny app session object is provided
    if ("session" %in% names(list(...))) {
      # nocov start
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_basereg)
      d_basereg <- rapbase::loadRegData(registryName, query_basereg)
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_procedure)
      d_pros <- rapbase::loadRegData(registryName, query_procedure)
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_mce)
      d_mce <- rapbase::loadRegData(registryName, query_mce)
      rapbase::repLogger(session = list(...)[["session"]],
                         msg = msg_patientlist)
      d_patientlist <- rapbase::loadRegData(registryName, query_patientlist)
      rapbase::repLogger(session = list(...)[["session"]], msg = msg_followup)
      d_followup <- rapbase::loadRegData(registryName, query_followup)
      # nocov end
    } else {
      d_basereg <- rapbase::loadRegData(registryName, query_basereg)
      d_pros <- rapbase::loadRegData(registryName, query_procedure)
      d_mce <- rapbase::loadRegData(registryName, query_mce)
      d_patientlist <- rapbase::loadRegData(registryName, query_patientlist)
      d_followup <- rapbase::loadRegData(registryName, query_followup)
    }

    list(
      basereg = d_basereg,
      pros = d_pros,
      mce = d_mce,
      patientlist = d_patientlist,
      followup = d_followup
    )
  }
}



#' @rdname getData
#' @export

getLatestEntry <- function(registryName) {

  # Get date of newest registration (National data)
  query <- "SELECT max(DATO_PROS) AS date FROM pros;"
  rapbase::loadRegData(registryName, query = query)$date
}

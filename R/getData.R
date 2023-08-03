#' Ablanor SQL
#'
#' Get tables from database usig SQL. Notice, only tables when a
#' procedure-date exists!
#'
#' \code{getNameReshId()} returns a mapping of organization name and id in the
#' form of columns named \emph{name} and \emph{id}. Optionally this function
#' can also return a list of named values (ids), \emph{e.g.} for use in shiny
#' selection lists.
#'
#' @param registryName "ablanor"
#' @param singleRow bools. TRUE bare metadata, FALSE hele datasettet
#' @param reshId Integer organization id. From login settings.
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by reshId), "SC" has access to national data.
#' @param fromDate NULL default is 01-01-1900. If datadump or pivot table,
#' start date of calendar is used.
#' @param toDate NULL default is newest registration in Abalnor. If datadump or
#'  pivot table, end date of calendar is used.
#' @param asNamedList Logical whether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#'
#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#'
#' @name getDataAblanor
#' @aliases getPros
#' getBasereg
#' getRand12
#' getMce
#' getPatientlist
#' getFriendlycentre
#' getFollowup
#' getLatestEntry
#' getNameReshId
#' getHospitalName
NULL


#' @rdname getDataAblanor
#' @export
getPros <- function(registryName,
                    singleRow,
                    reshId = NULL,
                    userRole,
                    fromDate = NULL,
                    toDate = NULL, ...){


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }

  # SQL only in defined interval, with non-missing dates.
  condition <- paste0(" WHERE DATO_PROS >= '", fromDate,
                      "' AND DATO_PROS < '", toDate, "'",
                      "AND DATO_PROS IS NOT NULL")

  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " AND CENTREID = '", reshId, "'")
  }

  # Kun fullførte prosedyrer (med prosedyredato!)
  query <- paste0("SELECT * FROM pros ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for pros"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for pros"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_pros <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_pros <- rapbase::loadRegData(registryName, query)
  }

  list(d_pros = d_pros)
}


#' @rdname getDataAblanor
#' @export
getBasereg <- function(registryName,
                       singleRow,
                       reshId = NULL,
                       userRole,
                       fromDate = NULL,
                       toDate = NULL,...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }

  # SQL only in defined interval, with non-missing dates.
  condition <- paste0(" WHERE pros.DATO_PROS >= '", fromDate,
                      "' AND pros.DATO_PROS < '", toDate, "'",
                      "AND pros.DATO_PROS IS NOT NULL")

  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " AND pros.CENTREID = '", reshId, "'")
  }


  # Kun basereg-skjema for fullførte prosedyrer (med prosedyredato!)
  query <- paste0("
  SELECT basereg.*,
         pros.DATO_PROS
  FROM pros
  LEFT JOIN basereg  ON
        pros.MCEID = basereg.MCEID AND
        pros.CENTREID = basereg.CENTREID",
                  condition)

  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for basereg"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for basereg"
    query <- paste0(query, ";")
  }


  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_basereg <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_basereg <- rapbase::loadRegData(registryName, query)
  }


  list(d_basereg = d_basereg)
}




#' @rdname getDataAblanor
#' @export
getMce <- function(registryName,
                   singleRow,
                   reshId = NULL,
                   userRole,
                   fromDate = NULL,
                   toDate = NULL, ...){

  # SQL NOT possible for defined time-interval. Use ALL mce entries

  # SQL only in defined interval, with non-missing dates.
  condition <- ""

  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }

  # Kun fullførte prosedyrer (med prosedyredato!)
  query <- paste0("SELECT * FROM mce ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for mce"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for mce"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_mce <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_mce <- rapbase::loadRegData(registryName, query)
  }

  list(d_mce = d_mce)
}



#' @rdname getDataAblanor
#' @export
getPatientlist <- function(registryName,
                           singleRow,
                           reshId = NULL,
                           userRole,
                           fromDate = NULL,
                           toDate = NULL, ...){

  # SQL NOT possible for defined time-interval. Use ALL patientlist entries.
  # Patient registered only once, even though mulitple prodecures are possible

  # NO FILTER ON HOSPITAL. One patient can have registrations on mulitple
  # hospitals. Only for SC-role.

  # ALLE pasienter i registeret
  query <- "SELECT * FROM patientlist"


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for patientlist"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for patientlist"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_patientlist <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_patientlist <- rapbase::loadRegData(registryName, query)
  }

  list(d_patientlist = d_patientlist)
}



#' @rdname getDataAblanor
#' @export
getFriendlycentre <- function(registryName,
                              singleRow,
                              reshId = NULL,
                              userRole,
                              fromDate = NULL,
                              toDate = NULL, ...){}



#' @rdname getDataAblanor
#' @export
getRand12 <- function(registryName,
                      singleRow,
                      reshId = NULL,
                      userRole,
                      fromDate = NULL,
                      toDate = NULL, ...) {


  # SQL possible for defined time-interval:
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }

  # SQL only in defined interval, with non-missing dates.
  condition <- paste0(" WHERE pros.DATO_PROS >= '", fromDate,
                      "' AND pros.DATO_PROS < '", toDate, "'",
                      "AND pros.DATO_PROS IS NOT NULL")

  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " AND pros.CENTREID = '", reshId, "'")
  }


  # Kun rand12-skjema for fullførte prosedyrer (med prosedyredato!)
  query <- paste0("
  SELECT rand12.*,
         pros.DATO_PROS
  FROM pros
  LEFT JOIN rand12  ON
        pros.MCEID = rand12.MCEID AND
        pros.CENTREID = rand12.CENTREID",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for rand12"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for rand12"
    query <- paste0(query, ";")
  }




  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_rand12 <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_rand12 <- rapbase::loadRegData(registryName, query)
  }


  list(d_rand12 = d_rand12)

}



#' @rdname getDataAblanor
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
    d_mce <- rapbase::loadRegData(registryName, query_mce)
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_followup <- rapbase::loadRegData(registryName, query)
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




#' @rdname getDataAblanor
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



#' @rdname getDataAblanor
#' @export
getLatestEntry <- function(registryName) {

  # Get date of newest registration (National data)
  query <- "SELECT max(DATO_PROS) AS date FROM pros;"
  rapbase::loadRegData(registryName, query = query)$date
}

#' @rdname getDataAblanor
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

#' @rdname getDataAblanor
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



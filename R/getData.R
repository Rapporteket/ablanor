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
#' @param singleRow bools. TRUE only one row, for variable names. FALSE all
#' rows.
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
#' @aliases getBasereg
#' getPros
#' getMce
#' getRand12
#' getFollowup
#' getGkv
#' getPatientlist
#' getFriendlycentre
#' getMcepatientdata
#' getBaseregPros
#' getLatestEntry
#' getNameReshId
#' getHospitalName
NULL


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
getMce <- function(registryName,
                   singleRow,
                   reshId = NULL,
                   userRole,
                   fromDate = NULL,
                   toDate = NULL, ...){

  # Use ALL mce entries
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
getRand12 <- function(registryName,
                      singleRow,
                      reshId = NULL,
                      userRole,
                      fromDate = NULL,
                      toDate = NULL, ...) {


  # Use ALL rand12 entries.
  condition <- ""
  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }


  query <- paste0("SELECT * FROM rand12",
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
getFollowup <- function(registryName,
                        singleRow,
                        reshId = NULL,
                        userRole,
                        fromDate = NULL,
                        toDate = NULL, ...) {

  # use all followup entries
  condition <- ""

  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }

  # Kun fullførte prosedyrer (med prosedyredato!)
  query <- paste0("SELECT * FROM followup ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for followup"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for followup"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_followup <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_followup <- rapbase::loadRegData(registryName, query)
  }

  list(d_followup = d_followup)
}

#' @rdname getDataAblanor
#' @export
getGkv <- function(registryName,
                   singleRow,
                   reshId = NULL,
                   userRole,
                   fromDate = NULL,
                   toDate = NULL, ...) {


  # SQL possible for defined time-interval:
  condition <- ""
  # national or local hospital
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }


  # Kun rand12-skjema for fullførte prosedyrer (med prosedyredato!)
  query <- paste0("SELECT * FROM gkv ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for gkv"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for gkv"
    query <- paste0(query, ";")
  }




  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_gkv <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_gkv <- rapbase::loadRegData(registryName, query)
  }


  list(d_gkv = d_gkv)

}



#' @rdname getDataAblanor
#' @export
getFriendlycentre <- function(registryName,
                              singleRow,
                              reshId = NULL,
                              userRole,
                              fromDate = NULL,
                              toDate = NULL, ...){

  query <- "SELECT * FROM friendlycentre"

  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for friendlycentre"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for friendlycentre"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_friendlycentre <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_friendlycentre <- rapbase::loadRegData(registryName, query)
  }

  list(d_friendlycentre = d_friendlycentre)
}




#' @rdname getDataAblanor
#' @export
getMcepatientdata <- function(registryName,
                              singleRow,
                              reshId = NULL,
                              userRole,
                              fromDate,
                              toDate, ...) {

  # Use ALL mce_patient_data entries
  query <- "SELECT * FROM mce_patient_data "

  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for mce_patient_data"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for mce_patient_data"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_mce_patient_data <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_mce_patient_data <- rapbase::loadRegData(registryName, query)
  }

  list(d_mce_patient_data = d_mce_patient_data)
}



#' @rdname getDataAblanor
#' @export
getPatientlist <- function(registryName,
                           singleRow,
                           reshId = NULL,
                           userRole,
                           fromDate = NULL,
                           toDate = NULL, ...){

  # Use ALL patientlist entries.
  # Patient registered only once, even though multiple prodecures/hospitals
  # are possible

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
getBaseregPros <- function(registryName,
                           singleRow,
                           reshId = NULL,
                           userRole,
                           fromDate = NULL,
                           toDate = NULL, ...) {

  d_pros <- ablanor::getPros(registryName = registryName,
                             singleRow = singleRow,
                             reshId = reshId,
                             userRole = userRole,
                             fromDate = fromDate,
                             toDate = toDate)

  d_basereg <- ablanor::getBasereg(registryName = registryName,
                                   singleRow = singleRow,
                                   reshId =reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)


  condition <- ""
  if (userRole != "SC") {
    condition <- paste0(condition, " AND CENTREID = '", reshId, "'")
  }
  query_mce <- paste0(
    "SELECT
      MCEID, PATIENT_ID
      FROM mce
      WHERE MCETYPE >=1
      AND MCETYPE <= 4 ", # uten eprom/followup/rand12/gkv
    condition)


  query_patientlist <- paste0(
    "SELECT
      ID, BIRTH_DATE, GENDER, DECEASED, DECEASED_DATE, SSN_TYPE, SSNSUBTYPE
      FROM patientlist")


  query_mcepatientdata <- paste0(
    "SELECT
      PID, MCEID, ZIPCODE
      FROM mce_patient_data")




  if (singleRow) {
    msg_mce <- "Query metadata for merged dataset, mce"
    msg_patientlist <- "Query metadata for merged dataset, patientlist"
    msg_mcepatientdata <- "Query metadata for merged dataset, mcepatientdata"

    query_mce <- paste0(query_mce, "\nLIMIT\n  1;")
    query_patientlist <- paste0(query_patientlist, "\nLIMIT\n  1;")
    query_mcepatientdata <- paste0(query_mcepatientdata, "\nLIMIT\n  1;")
  } else {
    msg_mce <- "Query data for merged dataset, mce"
    msg_patientlist <- "Query data for merged dataset, patientlist"
    msg_mcepatientdata <- "Query data for merged dataset, mcepatientdata"

    query_mce <- paste0(query_mce, ";")
    query_patientlist <- paste0(query_patientlist, ";")
    query_mcepatientdata <- paste0(query_mcepatientdata, ";")

  }

  # log db request if shiny app session object is provided
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = msg_mce)
    d_mce <- rapbase::loadRegData(registryName, query_mce)

    rapbase::repLogger(session = list(...)[["session"]],
                       msg = msg_patientlist)
    d_patientlist <- rapbase::loadRegData(registryName, query_patientlist)

    rapbase::repLogger(session = list(...)[["session"]],
                       msg = msg_mcepatientdata)
    d_mcepatientdata <- rapbase::loadRegData(registryName, query_mcepatientdata)


    # nocov end
  } else {
    d_mce <- rapbase::loadRegData(registryName, query_mce)
    d_patientlist <- rapbase::loadRegData(registryName, query_patientlist)
    d_mcepatientdata <- rapbase::loadRegData(registryName, query_mcepatientdata)
  }

  list(
    basereg = d_basereg$d_basereg,
    pros = d_pros$d_pros,
    mce = d_mce,
    patientlist = d_patientlist,
    mcepatientdata = d_mcepatientdata)

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



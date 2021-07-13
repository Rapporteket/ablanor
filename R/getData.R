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
#' @param asNamedList Logical wether to return a list of named values or not.
#' Default is FALSE in which case a data frame containing name and id is
#' returned.
#' @param singleRow Logical if only one row from the table is to be provided.
#' Default value is FALSE.
#' @param reshId Integer organization id.
#' @param ... Optional arguments to be passed to the function.
#'
#' @return Data frame or (when multiple data sets are returned) a list of data
#' frames containing registry data. In case of \code{getNameReshId()} data may
#' also be returned as a named list of values (see Details).
#' @name getData
#' @aliases getNameReshId getHospitalName getRand12 getProsPatient
NULL

#' @rdname getData
#' @export
getNameReshId <- function(registryName, asNamedList = FALSE) {

  query <- "
SELECT
  CENTRESHORTNAME AS name,
  ID AS id
FROM
  friendlycentre
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
getHospitalName <- function(registryName, reshId) {

  query <- paste0("
SELECT
  CENTRESHORTNAME
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
getRand12 <- function(registryName, singleRow, ...) {
  if (registryName == "test_ablanor_lokalt") {
    # LASTE INN DATA LOKALT
    load(file = Sys.getenv("filbane_ablanor_test"), envir = parent.frame())

  } else {
    query_procedure <- paste0("
SELECT
  *
FROM
  pros"
    )

    query_rand12 <- paste0("
SELECT
  *
FROM
  rand12"
    )

    if(singleRow) {
      msg_procedure <- "Query metadata for merged dataset, procedure"
      msg_rand12<- "Query metadata for merged dataset, rand12"
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
  }

  list(pros = d_pros, rand12 = d_rand12)
}

#' @rdname getData
#' @export
getProsPatient <- function(registryName, singleRow, ...) {

  if (registryName == "test_ablanor_lokalt") {
    load(file = Sys.getenv("filbane_ablanor_test"), envir = parent.frame())
  } else {
    query_basereg <- paste0("
SELECT
  *
FROM
  basereg"
    )

    query_procedure <- paste0("
SELECT
  *
FROM
  pros"
    )

    query_mce <- paste0("
SELECT
  MCEID,
  PATIENT_ID,
  STATUS
FROM
  mce"
    )

    query_patientlist <- paste0("
SELECT
  *
FROM
  patientlist"
    )

    if(singleRow) {
      msg_basereg <- "Query metadata for merged dataset, basereg"
      msg_procedure <- "Query metadata for merged dataset, procedure"
      msg_mce <- "Query metadata for merged dataset, mce"
      msg_patientlist <- "Query metadata for merged dataset, patientlist"

      query_basereg <- paste0(query_basereg, "\nLIMIT\n  1;")
      query_procedure <- paste0(query_procedure, "\nLIMIT\n  1;")
      query_mce <- paste0(query_mce, "\nLIMIT\n  1;")
      query_patientlist <- paste0(query_patientlist, "\nLIMIT\n  1;")
    } else {
      msg_basereg <- "Query data for merged dataset, basereg"
      msg_procedure <- "Query data for merged dataset, procedure"
      msg_mce <- "Query data for merged dataset, mce"
      msg_patientlist <- "Query data for merged dataset, patientlist"

      query_basereg <- paste0(query_basereg, ";")
      query_procedure <- paste0(query_procedure, ";")
      query_mce <- paste0(query_mce, ";")
      query_patientlist <- paste0(query_patientlist, ";")
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
      # nocov end
    } else {
      d_basereg <- rapbase::loadRegData(registryName, query_basereg)
      d_pros <- rapbase::loadRegData(registryName, query_procedure)
      d_mce <- rapbase::loadRegData(registryName, query_mce)
      d_patientlist <- rapbase::loadRegData(registryName, query_patientlist)
    }

    list(
      basereg = d_basereg,
      pros = d_pros,
      mce =d_mce,
      patientlist = d_patientlist
    )
  }
}

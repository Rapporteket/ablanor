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
#' @param shortName boolean. Default value FALSE and "friendlyname" is returned.
#' If TRUE shortname is returned.
#' @param newNames boolean. TRUE uses "sykehusnavn" as defined in
#' 'legg_til_sykehusnavn()'. Default value is FALSE, uses "sykehusnavn" from
#' table Friendlycentre.
#' @param ... Optional arguments to be passed to the function.
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
#' getFollowupBasis
#' getFollowupOneYr
#' getFollowupFiveYr
#' getGkv
#' getProms
#' getHendelse
#' getPatientlist
#' getFriendlycentre
#' getMcepatientdata
#' getBaseregPros
#' getBaseregProsFollowup1
#' getBaseregProsFollowup0
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
                      "' AND pros.DATO_PROS <= '", toDate, "'",
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
                      "' AND DATO_PROS <= '", toDate, "'",
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
getFollowupBasis <- function(registryName,
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
  query <- paste0("SELECT * FROM basisfollowup ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for basisfollowup"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for basisfollowup"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_followupBasis <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_followupBasis <- rapbase::loadRegData(registryName, query)
  }

  list(d_followupBasis = d_followupBasis)
}






#' @rdname getDataAblanor
#' @export
getFollowupOneYr <- function(registryName,
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
    msg <- "Query single row data for 1yr followup"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for 1yr followup"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_followup1 <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_followup1 <- rapbase::loadRegData(registryName, query)
  }

  list(d_followup1 = d_followup1)
}





#' @rdname getDataAblanor
#' @export
getFollowupFiveYr <- function(registryName,
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
  query <- paste0("SELECT * FROM fiveyearfollowup ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for 5yr followup"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for 5yr followup"
    query <- paste0(query, ";")
  }

  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_followup5 <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_followup5 <- rapbase::loadRegData(registryName, query)
  }

  list(d_followup5 = d_followup5)
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
getProms <- function(registryName,
                     singleRow,
                     reshId = NULL,
                     userRole,
                     fromDate = NULL,
                     toDate = NULL, ...) {



   # NOTE TO MYSELF: CENTREID is EMPTY FOR OLD > 2023-11 REGISTRATIONS
  # NOTE: DO NOT USE DATE
  # condition <- ""
  # # national or local hospital
  # if (userRole != "SC") {
  #   condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  # }
  # query <- paste0("SELECT * FROM proms",
  #                 condition)

  query <- "SELECT * FROM proms"

  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for proms"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for proms"
    query <- paste0(query, ";")
  }


  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_proms <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_proms <- rapbase::loadRegData(registryName, query)
  }

  list(d_proms = d_proms)

}





#' @rdname getDataAblanor
#' @export
getHendelse <- function(registryName,
                        singleRow,
                        reshId = NULL,
                        userRole,
                        fromDate = NULL,
                        toDate = NULL, ...) {


  condition <- ""
  if (userRole != "SC") {
    condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  }


  query <- paste0("SELECT * FROM adhoc ",
                  condition)


  # En eller alle rader:
  if (singleRow) {
    msg <- "Query single row data for hendelse"
    query <- paste0(query, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for hendelse"
    query <- paste0(query, ";")
  }




  # ENDELIG SQL SPØRRING
  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_gkv <- rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    d_hendelse <- rapbase::loadRegData(registryName, query)
  }


  list(d_hendelse = d_hendelse)

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


  # NOTE TO MYSELF: CENTREID is added in new version!! Attention
  # when merged with other files
  # NOTE: DO NOT USE DATE
  # condition <- ""
  # # national or local hospital
  # if (userRole != "SC") {
  #   condition <- paste0(condition, " WHERE CENTREID = '", reshId, "'")
  # }
  # query <- paste0("SELECT * FROM patientlist",
  #                 condition)


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
      ID, CENTREID, BIRTH_DATE, GENDER,
      DECEASED, DECEASED_DATE, SSN_TYPE, SSNSUBTYPE
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
getBaseregProsFollowup1 <- function(registryName,
                                    singleRow,
                                    reshId = NULL,
                                    userRole,
                                    fromDate = NULL,
                                    toDate = NULL, ...){

  # PROS + BASEREG sammen
  # proms,
  # Mce (type = 9), patientid
  # patientlist
  # followup
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }
  condition <- paste0(" WHERE pros.DATO_PROS >= '", fromDate,
                      "' AND pros.DATO_PROS <= '", toDate, "'",
                      " AND pros.DATO_PROS IS NOT NULL")


  condition_followup <- ""

  if (userRole != "SC") {
    condition_followup <- paste0(" AND mce.CENTREID = '", reshId, "'")
    condition <- paste0(condition, " AND pros.CENTREID = '", reshId, "'")
  }



  # BASEREG + PROSEDYRE + PASIENTID + PASIENTINFO
  # (kun dersom prosedyredato finnes)
  query_basePros <- paste0(
    "SELECT pros.MCEID,
            pros.CENTREID,
            pros.FORLOPSTYPE,
            pros.DATO_PROS,

            basereg.HOYDE,
            basereg.VEKT,
            basereg.HYPERTONI,
            basereg.DIABETES,
            basereg.HJERTESVIKT,
            basereg.TIA_SLAG,
            basereg.KARSYKDOM,
            basereg.HJERTEFEIL,
            basereg.OSAS_KOLS,
            basereg.KARDIOMYOPATI,
            basereg.PACEMAKER,
            basereg.EJEKFRAK,
            basereg.DEBUT_ARYT_AAR,
            basereg.EHRA_SYMPT,

            mce.PATIENT_ID,
            mce.MCETYPE,
            mce.HAS_FOLLOWUP,

            patientlist.ID,
            patientlist.BIRTH_DATE,
            patientlist.GENDER,
            patientlist.DECEASED,
            patientlist.DECEASED_DATE,
            patientlist.SSN_TYPE,
            patientlist.SSNSUBTYPE

    FROM pros
    LEFT JOIN basereg ON
         pros.MCEID = basereg.MCEID AND
         pros.CENTREID = basereg.CENTREID
    LEFT JOIN mce ON
         pros.MCEID = mce.MCEID AND
         pros.CENTREID = mce.CENTREID
    LEFT JOIN patientlist ON
         mce.PATIENT_ID = patientlist.ID AND
         mce.CENTREID = patientlist.CENTREID"
    ,
    condition,
    " AND pros.FORLOPSTYPE IS NOT NULL ")



  query_followup <- paste0(
    " SELECT mce.MCEID,
             mce.CENTREID,
             mce.MCETYPE,
             mce.PATIENT_ID,
             mce.PARENTMCEID,
             mce.TSCREATED,

             followup.DATO_FOLLOWUP,
             followup.COMPLETE,
             followup.INCOMPLETE_REASON,
             followup.Q1,
             followup.Q2,
             followup.Q3,
             followup.Q4,
             followup.Q5,
             followup.Q5_BURN_FREEZE,
             followup.Q5_PACEMAKER,
             followup.Q5_ELECTROCONVERSION,
             followup.Q5_OTHER,
             followup.Q5_OTHER_SPECIFY,
             followup.Q6,
             followup.Q6_REGULAR_EKG,
             followup.Q6_24_HOUR_EKG,
             followup.Q6_PACEMAKER,
             followup.Q6_PULSE_WATCH,
             followup.Q6_OTHER,
             followup.Q6_OTHER_SPECIFY,
             followup.Q7,
             followup.Q7_STROKE,
             followup.Q7_BLOCK,
             followup.Q7_OPERATION,
             followup.Q7_PACEMAKER,
             followup.Q7_OTHER,
             followup.Q7_OTHER_SPECIFY,
             followup.STATUS
    FROM mce
    LEFT JOIN followup ON
      mce.MCEID = followup.MCEID
    WHERE mce.MCETYPE = 9 ",
    condition_followup)

  query_proms <- "SELECT MCEID,
                         REGISTRATION_TYPE,
                         TSSENDT,
                         EXPIRY_DATE,
                         REMINDER_DATE,
                         STATUS,
                         FORM_ORDER_STATUS_ERROR_CODE
                  FROM proms
                  WHERE REGISTRATION_TYPE = 'Followup' "





  if (singleRow) {
    msg <- "Query single row data for 1-year followup"
    query_followup <- paste0(query_followup, "\nLIMIT\n  1;")
    query_basePros <- paste0(query_basePros, "\nLIMIT\n  1;")
    query_proms <- paste0(query_proms, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for 1-year followup"
    query_followup <- paste0(query_followup, ";")
    query_basePros <- paste0(query_basePros, ";")
    query_proms <- paste0(query_proms, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
    # nocov end
  } else {
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
  }


  list(d_baseregPat = d_baseregPat,
       d_followup = d_followup,
       d_proms = d_proms)

}






#' @rdname getDataAblanor
#' @export
getBaseregProsFollowup0 <- function(registryName,
                                    singleRow,
                                    reshId = NULL,
                                    userRole,
                                    fromDate = NULL,
                                    toDate = NULL, ...){

  # PROS + BASEREG sammen
  # proms,
  # Mce (type = 7), patientid
  # patientlist
  # followup
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }
  condition <- paste0(" WHERE pros.DATO_PROS >= '", fromDate,
                      "' AND pros.DATO_PROS <= '", toDate, "'",
                      " AND pros.DATO_PROS IS NOT NULL")


  condition_followup <- ""

  if (userRole != "SC") {
    condition_followup <- paste0(" AND mce.CENTREID = '", reshId, "'")
    condition <- paste0(condition, " AND pros.CENTREID = '", reshId, "'")
  }



  # BASEREG + PROSEDYRE + PASIENTID + PASIENTINFO
  # (kun dersom prosedyredato finnes)
  query_basePros <- paste0(
    "SELECT pros.MCEID,
            pros.CENTREID,
            pros.FORLOPSTYPE,
            pros.DATO_PROS,

            basereg.HOYDE,
            basereg.VEKT,
            basereg.HYPERTONI,
            basereg.DIABETES,
            basereg.HJERTESVIKT,
            basereg.TIA_SLAG,
            basereg.KARSYKDOM,
            basereg.HJERTEFEIL,
            basereg.OSAS_KOLS,
            basereg.KARDIOMYOPATI,
            basereg.PACEMAKER,
            basereg.EJEKFRAK,
            basereg.DEBUT_ARYT_AAR,
            basereg.EHRA_SYMPT,

            mce.PATIENT_ID,
            mce.MCETYPE,
            mce.HAS_BASISFOLLOWUP,

            patientlist.ID,
            patientlist.BIRTH_DATE,
            patientlist.GENDER,
            patientlist.DECEASED,
            patientlist.DECEASED_DATE,
            patientlist.SSN_TYPE,
            patientlist.SSNSUBTYPE

    FROM pros
    LEFT JOIN basereg ON
         pros.MCEID = basereg.MCEID AND
         pros.CENTREID = basereg.CENTREID
    LEFT JOIN mce ON
         pros.MCEID = mce.MCEID AND
         pros.CENTREID = mce.CENTREID
    LEFT JOIN patientlist ON
         mce.PATIENT_ID = patientlist.ID AND
         mce.CENTREID = patientlist.CENTREID"
    ,
    condition,
    " AND pros.FORLOPSTYPE IS NOT NULL ")



  query_followup <- paste0(
    " SELECT mce.MCEID,
             mce.CENTREID,
             mce.MCETYPE,
             mce.PATIENT_ID,
             mce.PARENTMCEID,
             mce.TSCREATED,

             basisfollowup.DATO_FOLLOWUP,
             basisfollowup.COMPLETE,
             basisfollowup.INCOMPLETE_REASON,
             basisfollowup.Q1,
             basisfollowup.Q2,
             basisfollowup.Q3,
             basisfollowup.Q4,
             basisfollowup.Q5,
             basisfollowup.Q5_BURN_FREEZE,
             basisfollowup.Q5_PACEMAKER,
             basisfollowup.Q5_ELECTROCONVERSION,
             basisfollowup.Q5_OTHER,
             basisfollowup.Q5_OTHER_SPECIFY,
             basisfollowup.Q6,
             basisfollowup.Q6_REGULAR_EKG,
             basisfollowup.Q6_24_HOUR_EKG,
             basisfollowup.Q6_PACEMAKER,
             basisfollowup.Q6_PULSE_WATCH,
             basisfollowup.Q6_OTHER,
             basisfollowup.Q6_OTHER_SPECIFY,
             basisfollowup.STATUS
    FROM mce
    LEFT JOIN basisfollowup ON
      mce.MCEID = basisfollowup.MCEID
    WHERE mce.MCETYPE = 7 ",
    condition_followup)

  query_proms <- "SELECT MCEID,
                         REGISTRATION_TYPE,
                         TSSENDT,
                         EXPIRY_DATE,
                         REMINDER_DATE,
                         STATUS,
                         FORM_ORDER_STATUS_ERROR_CODE
                  FROM proms
                  WHERE REGISTRATION_TYPE = 'Basisfollowup' "

  query_gkv <- "SELECT MCEID,
                       DATO_GKV,
                       GKV_1,
                       GKV_2,
                       GKV_3,
                       GKV_4,
                       GKV_5,
                       GKV_6,
                       GKV_7,
                       GKV_8,
                       GKV_9,
                       GKV_10,
                       GKV_11,
                       GKV_12
               FROM gkv
               WHERE COMPLETE = 1 AND FORM_COMPLETED_VIA_PROMS = 1"


  query_rand12 <- "SELECT MCEID,
                          FOLLOWUP_PARENT_TYPE,
                          DATO_RAND12,
                          RAND_1,
                          RAND_2A,
                          RAND_2B,
                          RAND_3A,
                          RAND_3B,
                          RAND_4A,
                          RAND_4B,
                          RAND_5,
                          RAND_6A,
                          RAND_6B,
                          RAND_6C,
                          RAND_7
                   FROM rand12
                   WHERE COMPLETE = 1
                   AND (FOLLOWUP_PARENT_TYPE = 1
                     OR FOLLOWUP_PARENT_TYPE = 2
                     OR FOLLOWUP_PARENT_TYPE = 3
                     OR FOLLOWUP_PARENT_TYPE = 4
                     OR FOLLOWUP_PARENT_TYPE = 7)"


  if (singleRow) {
    msg <- "Query single row data for basis followup"
    query_followup <- paste0(query_followup, "\nLIMIT\n  1;")
    query_basePros <- paste0(query_basePros, "\nLIMIT\n  1;")
    query_proms <- paste0(query_proms, "\nLIMIT\n  1;")
    query_gkv <- paste0(query_gkv, "\nLIMIT\n  1;")
    query_rand12 <- paste0(query_rand12, "\nLIMIT\n  1;")

  } else {
    msg <- "Query data for basis followup"
    query_followup <- paste0(query_followup, ";")
    query_basePros <- paste0(query_basePros, ";")
    query_proms <- paste0(query_proms, ";")
    query_gkv <- paste0(query_gkv, ";")
    query_rand12 <- paste0(query_rand12, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
    d_gkv <- rapbase::loadRegData(registryName , query_gkv)
    d_rand12 <- rapbase::loadRegData(registryName , query_rand12)
    # nocov end
  } else {
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
    d_gkv <- rapbase::loadRegData(registryName , query_gkv)
    d_rand12 <- rapbase::loadRegData(registryName , query_rand12)
  }


  list(d_baseregPat = d_baseregPat,
       d_followup = d_followup,
       d_proms = d_proms,
       d_gkv = d_gkv,
       d_rand12 = d_rand12)

}




#' @rdname getDataAblanor
#' @export
getBaseregProsFollowup5 <- function(registryName,
                                    singleRow,
                                    reshId = NULL,
                                    userRole,
                                    fromDate = NULL,
                                    toDate = NULL, ...){

  # PROS + BASEREG sammen
  # proms,
  # Mce (type = 10), patientid
  # patientlist
  # followup
  if (is.null(fromDate)) {
    fromDate <- as.Date("1900-01-01")
  }
  if (is.null(toDate)) {
    toDate <- ablanor::getLatestEntry(registryName)
  }
  condition <- paste0(" WHERE pros.DATO_PROS >= '", fromDate,
                      "' AND pros.DATO_PROS <= '", toDate, "'",
                      " AND pros.DATO_PROS IS NOT NULL")


  condition_followup <- ""

  if (userRole != "SC") {
    condition_followup <- paste0(" AND mce.CENTREID = '", reshId, "'")
    condition <- paste0(condition, " AND pros.CENTREID = '", reshId, "'")
  }



  # BASEREG + PROSEDYRE + PASIENTID + PASIENTINFO
  # (kun dersom prosedyredato finnes)
  query_basePros <- paste0(
    "SELECT pros.MCEID,
            pros.CENTREID,
            pros.FORLOPSTYPE,
            pros.DATO_PROS,

            basereg.HOYDE,
            basereg.VEKT,
            basereg.HYPERTONI,
            basereg.DIABETES,
            basereg.HJERTESVIKT,
            basereg.TIA_SLAG,
            basereg.KARSYKDOM,
            basereg.HJERTEFEIL,
            basereg.OSAS_KOLS,
            basereg.KARDIOMYOPATI,
            basereg.PACEMAKER,
            basereg.EJEKFRAK,
            basereg.DEBUT_ARYT_AAR,
            basereg.EHRA_SYMPT,

            mce.PATIENT_ID,
            mce.MCETYPE,
            mce.HAS_FOLLOWUP,

            patientlist.ID,
            patientlist.BIRTH_DATE,
            patientlist.GENDER,
            patientlist.DECEASED,
            patientlist.DECEASED_DATE,
            patientlist.SSN_TYPE,
            patientlist.SSNSUBTYPE

    FROM pros
    LEFT JOIN basereg ON
         pros.MCEID = basereg.MCEID AND
         pros.CENTREID = basereg.CENTREID
    LEFT JOIN mce ON
         pros.MCEID = mce.MCEID AND
         pros.CENTREID = mce.CENTREID
    LEFT JOIN patientlist ON
         mce.PATIENT_ID = patientlist.ID AND
         mce.CENTREID = patientlist.CENTREID"
    ,
    condition,
    " AND pros.FORLOPSTYPE IS NOT NULL ")



  query_followup <- paste0(
    " SELECT mce.MCEID,
             mce.CENTREID,
             mce.MCETYPE,
             mce.PATIENT_ID,
             mce.PARENTMCEID,
             mce.TSCREATED,

             fiveyearfollowup.DATO_FOLLOWUP,
             fiveyearfollowup.COMPLETE,
             fiveyearfollowup.INCOMPLETE_REASON,
             fiveyearfollowup.Q1,
             fiveyearfollowup.Q2,
             fiveyearfollowup.Q3,
             fiveyearfollowup.Q4,
             fiveyearfollowup.Q5,
             fiveyearfollowup.Q5_BURN_FREEZE,
             fiveyearfollowup.Q5_PACEMAKER,
             fiveyearfollowup.Q5_ELECTROCONVERSION,
             fiveyearfollowup.Q5_OTHER,
             fiveyearfollowup.Q5_OTHER_SPECIFY,
             fiveyearfollowup.Q6,
             fiveyearfollowup.Q6_REGULAR_EKG,
             fiveyearfollowup.Q6_24_HOUR_EKG,
             fiveyearfollowup.Q6_PACEMAKER,
             fiveyearfollowup.Q6_PULSE_WATCH,
             fiveyearfollowup.Q6_OTHER,
             fiveyearfollowup.Q6_OTHER_SPECIFY,
             fiveyearfollowup.Q7,
             fiveyearfollowup.Q7_STROKE,
             fiveyearfollowup.Q7_BLOCK,
             fiveyearfollowup.Q7_OPERATION,
             fiveyearfollowup.Q7_PACEMAKER,
             fiveyearfollowup.Q7_OTHER,
             fiveyearfollowup.Q7_OTHER_SPECIFY,
             fiveyearfollowup.STATUS
    FROM mce
    LEFT JOIN fiveyearfollowup ON
      mce.MCEID = fiveyearfollowup.MCEID
    WHERE mce.MCETYPE = 10 ",
    condition_followup)

  query_proms <- "SELECT MCEID,
                         REGISTRATION_TYPE,
                         TSSENDT,
                         EXPIRY_DATE,
                         REMINDER_DATE,
                         STATUS,
                         FORM_ORDER_STATUS_ERROR_CODE
                  FROM proms
                  WHERE REGISTRATION_TYPE = 'Fiveyearfollowup' "





  if (singleRow) {
    msg <- "Query single row data for 5-year followup"
    query_followup <- paste0(query_followup, "\nLIMIT\n  1;")
    query_basePros <- paste0(query_basePros, "\nLIMIT\n  1;")
    query_proms <- paste0(query_proms, "\nLIMIT\n  1;")
  } else {
    msg <- "Query data for 5-year followup"
    query_followup <- paste0(query_followup, ";")
    query_basePros <- paste0(query_basePros, ";")
    query_proms <- paste0(query_proms, ";")
  }

  if ("session" %in% names(list(...))) {
    # nocov start
    rapbase::repLogger(session = list(...)[["session"]], msg = msg)
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup5 <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
    # nocov end
  } else {
    d_baseregPat <- rapbase::loadRegData(registryName, query_basePros)
    d_followup5 <- rapbase::loadRegData(registryName , query_followup)
    d_proms <- rapbase::loadRegData(registryName , query_proms)
  }


  list(d_baseregPat = d_baseregPat,
       d_followup = d_followup5,
       d_proms = d_proms)

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
getNameReshId <- function(registryName, asNamedList = FALSE, shortNames = FALSE, newNames = FALSE) {

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

  if(newNames){
    res %<>%
      dplyr::mutate(centreid = id) %>%
      ablanor::legg_til_sykehusnavn(., short = shortNames) %>%
      dplyr::select(id, sykehusnavn) %>%
      dplyr::rename("name" = "sykehusnavn") %>%
      dplyr::filter(!is.na(name))
  }

  if (asNamedList) {
    res <- stats::setNames(res$id, res$name)
    res <- as.list(res)
  }

  res
}

#' @rdname getDataAblanor
#' @export
getHospitalName <- function(registryName, reshId, shortName = FALSE, newNames = FALSE) {

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

  if(newNames) {
    name <- ablanor::legg_til_sykehusnavn(
        df = data.frame(centreid = reshId),
        short = shortName) %>%
      dplyr::pull(sykehusnavn)
    } else {
      name <- rapbase::loadRegData(registryName, query)[1, ]
    }


  if (is.na(name)) {
    warning(paste("Resh ID", reshId, "did not match any names!"))
  }

  name
}



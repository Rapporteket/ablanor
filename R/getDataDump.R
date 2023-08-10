#' Get datadump ablanor
#'
#' Use getDataAbalnor function to get selected table.
#'
#' @param registryName "ablanor"
#' @param tableName Selected table for download
#' @param fromDate default 01-01-1900. Start date in calendar if
#' tableName is pros or basereg, argument ignored for other tableNames.
#' @param toDate default is newest registration. End date i calendar
#' if tableName is pros or basereg, argument ignored for other tableNames.
#' @param reshId from session information
#' @param userRole "SC" gets National data, "LC" get local hospital's data.
#' Some tables are only available for SC users.
#' @param ...
#'
#' @return data.frame
#' @export
getDataDump <- function(registryName,
                        tableName,
                        fromDate,
                        toDate,
                        reshId = NULL,
                        userRole, ...) {
  . <- ""
  stopifnot(tableName %in% c("basereg",
                             "pros",
                             "mce",
                             "rand12",
                             "followupbasis",
                             "followup1",
                             "followup5",
                             "gkv",
                             "hendelse",
                             "proms",
                             "kodeboken",
                             "friendlycentre",
                             "mce_patient_data",
                             "patientlist"))


  if (tableName == "basereg") {
    tab_list <- ablanor::getBasereg(registryName = registryName,
                                    singleRow = FALSE,
                                    reshId = reshId,
                                    userRole = userRole,
                                    fromDate = fromDate,
                                    toDate = toDate)
    dat <- tab_list$d_basereg
  }

  if (tableName == "pros") {
    tab_list <- ablanor::getPros(registryName = registryName,
                                 singleRow = FALSE,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = fromDate,
                                 toDate = toDate)
    dat <- tab_list$d_pros
  }

  if (tableName == "mce") {
    tab_list <- ablanor::getMce(registryName = registryName,
                                singleRow = FALSE,
                                reshId = reshId,
                                userRole = userRole,
                                fromDate = fromDate,
                                toDate = toDate)
    dat <- tab_list$d_mce
  }

  if (tableName == "rand12") {
    tab_list <- ablanor::getRand12(registryName = registryName,
                                   singleRow = FALSE,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)
    dat <- tab_list$d_rand12
  }

  if (tableName == "followupbasis") {
    tab_list <- ablanor::getFollowupBasis(registryName = registryName,
                                          singleRow = FALSE,
                                          reshId = reshId,
                                          userRole = userRole,
                                          fromDate = fromDate,
                                          toDate = toDate)
    dat <- tab_list$d_followupBasis
  }

  if (tableName == "followup1") {
    tab_list <- ablanor::getFollowupOneYr(registryName = registryName,
                                          singleRow = FALSE,
                                          reshId = reshId,
                                          userRole = userRole,
                                          fromDate = fromDate,
                                          toDate = toDate)
    dat <- tab_list$d_followup1
  }


  if (tableName == "followup5") {
    tab_list <- ablanor::getFollowupFiveYr(registryName = registryName,
                                           singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = fromDate,
                                           toDate = toDate)
    dat <- tab_list$d_followup5
  }

  if (tableName == "gkv") {
    tab_list <- ablanor::getGkv(registryName = registryName,
                                singleRow = FALSE,
                                reshId = reshId,
                                userRole = userRole,
                                fromDate = fromDate,
                                toDate = toDate)
    dat <- tab_list$d_gkv
  }

  if (tableName == "hendelse") {
    tab_list <- ablanor::getHendelse(registryName = registryName,
                                     singleRow = FALSE,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate)
    dat <- tab_list$d_hendelse
  }

  if (tableName == "friendlycentre") {
    tab_list <- ablanor::getFriendlycentre(registryName = registryName,
                                           singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = fromDate,
                                           toDate = toDate)
    dat <- tab_list$d_friendlycentre
  }

  if (tableName == "mce_patient_data") {
    tab_list <- ablanor::getMcepatientdata(registryName = registryName,
                                           singleRow = FALSE,
                                           reshId = reshId,
                                           userRole = userRole,
                                           fromDate = fromDate,
                                           toDate = toDate)
    dat <- tab_list$d_mce_patient_data
  }

  if (tableName == "patientlist") {
    tab_list <- ablanor::getPatientlist(registryName = registryName,
                                        singleRow = FALSE,
                                        reshId = reshId,
                                        userRole = userRole,
                                        fromDate = fromDate,
                                        toDate = toDate)
    dat <- tab_list$d_patientlist
  }

  if (tableName == "proms") {
    tab_list <- ablanor::getProms(registryName = registryName,
                                  singleRow = FALSE,
                                  reshId = reshId,
                                  userRole = userRole,
                                  fromDate = fromDate,
                                  toDate = toDate)
    dat <- tab_list$d_proms
  }


  if (tableName == "kodeboken") {
    dat <- ablanor::getKodebokData()
  }

  return(dat)
}


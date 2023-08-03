#' Get datadump ablanor
#'
#' Use getDataAbalnor function to get selected table.
#'
#' @param registryName "ablanor"
#' @param tableName Selected table for download
#' @param fromDate default 01-01-1900. Otherwise start date i calendar.
#' @param toDate default is newest registration. Otherwise end date i calendar.
#' @param reshId from session information
#' @param userRole "SC" gets National data, "LC" get local hospital's data.
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
                             "friendlycentre",
                             "mce",
                             "rand12",
                             "patientlist",
                             "followup",
                             "gkv",
                             "pros_patient_followup",
                             "kodeboken"))


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


  if (tableName == "rand12") {
    tab_list <- ablanor::getRand12(registryName = registryName,
                                   singleRow = FALSE,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)
    dat <- tab_list$d_rand12
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

  if (tableName == "patientlist") {
    tab_list <- ablanor::getPatientlist(registryName = registryName,
                                        singleRow = FALSE,
                                        reshId = reshId,
                                        userRole = userRole,
                                        fromDate = fromDate,
                                        toDate = toDate)
    dat <- tab_list$d_patientlist
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




  if (tableName == "pros_patient_followup") {
    dat <- ablanor::getProsPatientData(registryName = registryName,
                                       singleRow = FALSE,
                                       reshId = reshId,
                                       userRole = userRole,
                                       fromDate = fromDate,
                                       toDate = toDate)
    dat %<>% ablanor::legg_til_sykehusnavn(df = ., short = FALSE)

  }
  if (tableName == "kodeboken") {
    dat <- ablanor::getKodebokData()
  }

  return(dat)
}


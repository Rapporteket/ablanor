
#' Get selected data set for Pivot Table  (Utforsker)
#'
#' Some tables are returned as they are, with only raw data. Other tables are
#' treated (importation of variables from other tables, generation of new
#' variables) before the data frame is returned.
#'
#' @param setId String providing name of data set to be returned. One of the
#' values "pros_patient" containing procedure and patient data or "rand12"
#' containing data from rand12 questionnaire.
#' @param registryName String providing registry name.
#' @param session List shiny session object.
#' @param singleRow Logical defining if only one row is to be returned.
#'
#' @return data frame
#' @export
#' @examples
#' \dontrun{d_ablanor <- getPivotDataSet(setId = "pros_patient")
#' }
getPivotDataSet <- function(setId = "",
                            registryName,
                            session,
                            singleRow = FALSE) {

  validSetId <- c("pros_patient", "rand12")

  if (setId %in% validSetId) {

    if (setId == "rand12") {
      dat <- getRand12Data(registryName = registryName,
                           singleRow = singleRow)
      # , tekstVars = TRUE) @fixme
    }
    if (setId == "pros_patient") {
      dat <- getProsPatientData(registryName = registryName,
                                singleRow = singleRow)
      # , tekstVars = TRUE) @fixme
    }

  } else {
    dat <- NULL
  }
  dat
}
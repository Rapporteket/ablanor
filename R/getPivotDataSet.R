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
#' @param reshId Integer organization id
#' @param userRole String dummy/placeholder role. "LC" has access only
#' to local data (defined by reshId), "SC" has access to national data.
#'
#' @return data frame
#' @export
#' @examples
#' \dontrun{d_ablanor <- getPivotDataSet(setId = "pros_patient")
#' }
getPivotDataSet <- function(setId = "",
                            registryName,
                            session,
                            singleRow = FALSE,
                            reshId = NULL,
                            userRole,
                            fromDate = NULL,
                            doDate = NULL) {
  . <- ""

  validSetId <- c("pros_patient", "rand12", "basereg", "followup", "mce", "gkv")

  if (setId %in% validSetId) {

    if (userRole != "SC") {
      stopifnot(!is.null(reshId))
    }

    if (setId == "rand12") {
      dat <- ablanor::getRand12Data(registryName = registryName,
                                    singleRow = singleRow,
                                    session = session,
                                    reshId = reshId,
                                    userRole = userRole)
    }


    if (setId == "basereg") {
      dat <- ablanor::getBaseregData(registryName = registryName,
                                     singleRow = singleRow,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = doDate)
    }



    if (setId == "pros_patient") {
      dat <- ablanor::getProsPatientData(registryName = registryName,
                                         singleRow = singleRow,
                                         session = session,
                                         reshId = reshId,
                                         userRole = userRole,
                                         fromDate = NULL,
                                         toDate = NULL)
    }


    if (setId == "followup") {
      dat <- ablanor::getFollowupData(registryName = registryName,
                                      singleRow = singleRow,
                                      session = session,
                                      reshId = reshId,
                                      userRole = userRole)
    }




    if(singleRow == FALSE){
      # Erstatte listeverdi med listetekst og ja/nei for avkrysningsboks
      kb <- ablanor::getKodebokData() %>%
        dplyr::select(.data$fysisk_feltnavn,
                      .data$listeverdier,
                      .data$listetekst,
                      .data$type)

      dat %<>% ablanor::kodebok_fyll_listetekstvar(df = .,
                                                   kb = kb,
                                                   suffiks = "_tekst") %>%
        ablanor::kodebok_fyll_avkrysningsboks(df = .,
                                              kb = kb,
                                              suffiks = "_tekst") %>%
        ablanor::kodebok_beholde_bare_listetekstvar(
          df = .,
          kb = kb,
          suffiks = "_tekst",
          fjerne_suffiks_fra_navn = TRUE)
    }

    dat %<>% ablanor::legg_til_sykehusnavn(df = ., short = FALSE)

  } else {
    dat <- NULL
  }


  dat
}

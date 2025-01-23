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
                            toDate = NULL) {
  . <- ""

  validSetId <- c("basereg",
                  "pros",
                  "mce",
                  "rand12",
                  "followupbasis",
                  "followup1",
                  "followup5",
                  "gkv",
                  "proms",
                  "basereg_pros_indik",
                  "basereg_pros_hendelse",
                  "pros_pat_followup0",
                  "pros_pat_followup1",
                  "pros_pat_followup5")


  if (setId %in% validSetId) {

    if (userRole != "SC") {
      stopifnot(!is.null(reshId))
    }


    # BASISSKJEMA RÅDATA
    if (setId == "basereg") {
      dat <- ablanor::getBaseregData(registryName = registryName,
                                     singleRow = singleRow,
                                     session = session,
                                     reshId = reshId,
                                     userRole = userRole,
                                     fromDate = fromDate,
                                     toDate = toDate)
    }

    # PROSEDYRESKJEMA RÅDATA
    if (setId == "pros") {
      dat <- ablanor::getProsData(registryName = registryName,
                                  singleRow = singleRow,
                                  session = session,
                                  reshId = reshId,
                                  userRole = userRole,
                                  fromDate = fromDate,
                                  toDate = toDate)
    }

    # FORLØPSOVERSIKT RÅDATA
    if (setId == "mce") {
      dat <- ablanor::getMceData(registryName = registryName,
                                 singleRow = singleRow,
                                 session = session,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = NULL,
                                 toDate = NULL)
    }
    # RAND 12: BASIS, 1 OG 5 ÅR. RÅDATA
    if (setId == "rand12") {
      dat <- ablanor::getRand12Data(registryName = registryName,
                                    singleRow = singleRow,
                                    session = session,
                                    reshId = reshId,
                                    userRole = userRole,
                                    fromDate = fromDate,
                                    toDate = toDate)
    }


    # FOLLOWUP BASIS RÅDATA
    if (setId == "followupbasis") {
      dat <- ablanor::getFollowupBasisData(registryName = registryName,
                                           singleRow = singleRow,
                                           session = session,
                                           reshId = reshId,
                                           userRole = userRole)
    }


    # FOLLOWUP 1 ÅR RÅDATA
    if (setId == "followup1") {
      dat <- ablanor::getFollowupOneYrData(registryName = registryName,
                                           singleRow = singleRow,
                                           session = session,
                                           reshId = reshId,
                                           userRole = userRole)
    }

    # FOLLOWUP 5 ÅR RÅDATA
    if (setId == "followup5") {
      dat <- ablanor::getFollowupFiveYrData(registryName = registryName,
                                            singleRow = singleRow,
                                            session = session,
                                            reshId = reshId,
                                            userRole = userRole)
    }


    # GKV RÅDATA (prom basis)
    if (setId == "gkv") {
      dat <- ablanor::getGkvData(registryName = registryName,
                                 singleRow = singleRow,
                                 session = session,
                                 reshId = reshId,
                                 userRole = userRole,
                                 fromDate = fromDate,
                                 toDate = toDate)
    }

    # PROMS STATUS - RÅDATA
    if (setId == "proms") {
      dat <- ablanor::getPromsData(registryName = registryName,
                                   singleRow = singleRow,
                                   session = session,
                                   reshId = reshId,
                                   userRole = userRole,
                                   fromDate = fromDate,
                                   toDate = toDate)
    }



    # BASIS-PROSEDYRE- KVALITETSINDIKATORER
    if (setId == "basereg_pros_indik") {
      dat <- ablanor::getBaseregProsData(registryName = registryName,
                                         singleRow = singleRow,
                                         session = session,
                                         reshId = reshId,
                                         userRole = userRole,
                                         fromDate = fromDate,
                                         toDate = toDate)
    }

    # BASIS-PROSEDYRE-HENDELSE
    if (setId == "basereg_pros_hendelse") {
      dat <- ablanor::getBaseregProsHendelseData(registryName = registryName,
                                                 singleRow = singleRow,
                                                 session = session,
                                                 reshId = reshId,
                                                 userRole = userRole,
                                                 fromDate = fromDate,
                                                 toDate = toDate)
    }


    if (setId == "pros_pat_followup1") {
      dat <- ablanor::getBaseregProsFollowup1Data(registryName = registryName,
                                                  singleRow = singleRow,
                                                  session = session,
                                                  reshId = reshId,
                                                  userRole = userRole,
                                                  fromDate = fromDate,
                                                  toDate = toDate)
    }

    if (setId == "pros_pat_followup0") {
      dat <- ablanor::getBaseregProsFollowup0Data(registryName = registryName,
                                                  singleRow = singleRow,
                                                  session = session,
                                                  reshId = reshId,
                                                  userRole = userRole,
                                                  fromDate = fromDate,
                                                  toDate = toDate)
    }


    if (setId == "pros_pat_followup5") {
      dat <- ablanor::getBaseregProsFollowup5Data(registryName = registryName,
                                                  singleRow = singleRow,
                                                  session = session,
                                                  reshId = reshId,
                                                  userRole = userRole,
                                                  fromDate = fromDate,
                                                  toDate = toDate)
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

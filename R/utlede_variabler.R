#' Add variable age (alder)
#'
#' Age in years at time of procedure. Calculated from variables
#' \code{birth_date} and \code{dato_pros}.
#'
#' @param df data.frame with AblaNor data. Must contain variables
#' \code{birth_date} and \code{dato_pros}
#'
#' @return Returns \code{df} with one new variable: alder (= age in years)
#' @export
#'
#' @examples x <- data.frame(
#'    birth_date = as.Date(rep("1951-12-30",3), format = "%Y-%m-%d"),
#'    dato_pros = as.Date(c("2021-05-29", "2021-12-29", "2021-12-30"),
#'    format = "%Y-%m-%d"))
#' ablanor::utlede_alder(x)
utlede_alder <- function(df) {

  stopifnot(all(c("birth_date", "dato_pros") %in% names(df)))
  stopifnot(df %>% dplyr::pull(.data$birth_date) %>% lubridate::is.Date())
  stopifnot(df %>% dplyr::pull(.data$dato_pros) %>% lubridate::is.Date())

  df %>%
    dplyr::mutate(
      alder = lubridate::as.period(
        x = lubridate::interval(start = .data$birth_date,
                                end = .data$dato_pros),
        unit = "years")$year)
}

#' Add binary variable alder_75
#'
#' Create binary variable \code{alder_75} from \code{alder}.
#'
#' @param df ablanor data.frame, must contain variable \code{alder}. See also
#' \link[ablanor]{utlede_alder}.
#'
#' @return Returns \code{df} with one new variable: alder_75
#' @export
#'
#' @examples
#' x <- data.frame(alder = 73:76)
#' ablanor::utlede_alder_75(x)
utlede_alder_75 <- function(df) {
  stopifnot("alder" %in% names(df))

  df %>%
    dplyr::mutate(alder_75 = ifelse(.data$alder >= 75,
                                    yes = ">=75",
                                    no = "<75")) %>%
    dplyr::relocate(.data$alder_75, .after = .data$alder)
}



#' Add variable aldersklasse
#'
#' Create variable \code{aldersklasse} from \code{alder}.
#'
#' @param df ablanor data.frame, must contain variable \code{alder}. See also
#' \link[ablanor]{utlede_alder}.
#'
#' @return Returns \code{df} with one new variable: aldersklasse
#' @export
#'
#' @examples
#' x <- data.frame(alder = 73:76)
#' ablanor::utlede_aldersklasse(x)
utlede_aldersklasse <- function(df) {
  stopifnot("alder" %in% names(df))

  df %>%
    dplyr::mutate(
      aldersklasse = cut(
        tidyr::replace_na(.data$alder, replace = 0),
        breaks = c(18, 49, 59, 69, 79, 89, 99),
        include.lowest = TRUE,
        labels = c("18-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
        ordered_result = TRUE)) %>%
    dplyr::relocate(.data$aldersklasse, .after = .data$alder)
}




#' Add variables bmi_klasse and bmi_over35
#'
#' Add categories of Body Mass Index (BMI = weight in kilograms divided by
#' the square of height in meters).
#'
#' @param df data.frame with ablanor-data. Must contain variable
#' \code{bmi}.
#'
#' @return Returns \code{df} with 2 new columns: \code{bmi_klasse}
#' and \code{bmi_over35}
#' @export
#'
#' @examples
#' df <- data.frame(bmi = c(15, 15.2, 19, 25, 26.7, 32.1, 41.0, NA))
utlede_bmi_klasse <- function(df) {

  stopifnot("bmi" %in% names(df))

  df %>%
    dplyr::mutate(
      bmi_klasse =
        factor(
          x = dplyr::case_when(
            .data$bmi <= 16 ~ "Alvorlig undervekt",
            .data$bmi > 16 & .data$bmi <= 17 ~ "Moderat undervekt",
            .data$bmi > 17 & .data$bmi <= 18.5 ~ "Mild undervekt",
            .data$bmi > 18.5 & .data$bmi < 25 ~ "Normal",
            .data$bmi >= 25 & .data$bmi < 30 ~ "Overvekt",
            .data$bmi >= 30 & .data$bmi < 35 ~ "Moderat fedme, klasse I",
            .data$bmi >= 35 & .data$bmi < 40 ~ "Fedme, klasse II",
            .data$bmi >= 40 & .data$bmi < 100 ~ "Fedme, klasse III",
            .data$bmi >= 100 ~ "ugyldig",
            TRUE ~ NA_character_),

          levels = c("Alvorlig undervekt",
                     "Moderat undervekt",
                     "Mild undervekt",
                     "Normal",
                     "Overvekt",
                     "Moderat fedme, klasse I",
                     "Fedme, klasse II",
                     "Fedme, klasse III",
                     "ugyldig"),
          ordered = TRUE),

      bmi_over35 = dplyr::if_else(.data$bmi >= 35,
                                  true = "BMI >=35",
                                  false = "BMI <35",
                                  missing = NA_character_)
    )

}




#' Add tidsvariabler
#'
#' Add variables \code{aar}, \code{maaned_nr} (numerical month) and
#' \code{maaned} (year + month) in data set based in \code{dato_pros}.
#'
#' @param df data.frame with ablanor-data. Must contain \code{dato_pros}.
#'
#' @return Returns \code{df} with 3 new columns
#' @export
#'
#' @examples  df <- data.frame(
#' dato_pros = as.Date(c("2021-10-05", "1998-01-16", "2020-07-07"),
#'                     format = "%Y-%m-%d"))
#' ablanor::utlede_tidsvariabler(df)
utlede_tidsvariabler <- function(df) {

  stopifnot("dato_pros" %in% names(df))


  df %>%
    dplyr::mutate(
      aar = as.ordered(lubridate::year(.data$dato_pros)),
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     lubridate::month(.data$dato_pros))),
      maaned = ifelse(test = is.na(.data$aar) | is.na(.data$maaned_nr),
                      yes = NA,
                      no = paste0(.data$aar, "-", .data$maaned_nr)))


}





#' Kategori AFLI arytmi i48
#'
#' Add variable \code{kategori_afli_aryt_i48} for AFLI-procedures, based on
#' ICD codes.
#'
#' @param df data.frame with Ablanor-data. Must contain variables
#' \code{forlopstype}, \code{aryt_i48_0}, \code{aryt_i48_1} and
#' \code{aryt_i48_1_underkat}.
#'
#' @return returns \code{df} with one new column.
#' @export
#'
#' @examples
#' df <- data.frame(
#'    forlopstype = rep(1, 4),
#'    aryt_i48_0 = c(0, 1, 0, 0),
#'    aryt_i48_1 =  c(0, 0, 1, 2),
#'    aryt_i48_1_underkat = c(NA, NA, 1, 2))
#' ablanor::utlede_kateg_afli_aryt_i48(df)
utlede_kateg_afli_aryt_i48 <- function(df) {

  stopifnot(c("forlopstype",
              "aryt_i48_0",
              "aryt_i48_1",
              "aryt_i48_1_underkat") %in% names(df))

  df %>%
    dplyr::mutate(
      kategori_afli_aryt_i48 = factor(
        x = dplyr::case_when(

          .data$forlopstype == 1 & .data$aryt_i48_0 == TRUE ~
            "AFLI-ICD 48.0 Paroksymal atrieflimmer",

          .data$forlopstype == 1 &
            .data$aryt_i48_1 == TRUE &
            .data$aryt_i48_1_underkat == 1 ~
            "AFLI-ICD 48.1 Persisterende atrieflimmer",

          .data$forlopstype == 1 &
            .data$aryt_i48_1 == TRUE &
            .data$aryt_i48_1_underkat == 2 ~
            "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer"),

        levels = c("AFLI-ICD 48.0 Paroksymal atrieflimmer",
                   "AFLI-ICD 48.1 Persisterende atrieflimmer",
                   "AFLI-ICD 48.1 Langtidspersisterende atrieflimmer"),
        ordered = TRUE))
}






#' VT Kardiomyopati
#'
#' Add variable \code{kategori_vt_kardiomyopati} for VT-procedures,
#' based on variables \code{kardiomyopati} and \code{type_kardiomyopati}.
#'
#' @param df data.frame with AblaNor data. Must contain \code{forlopstype},
#' \code{kardiomyopati}, \code{type_kardiomyopati}.
#'
#' @return Returns \code{df} with 1 new column.
#' @export
#'
#' @examples
#' df <- data.frame(
#'    forlopstype = c(NA, 1, rep(2,8)),
#'    kardiomyopati =  c(NA, NA, 9, 0, rep(1, 6)),
#'    type_kardiomyopati = c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 99))
#' ablanor::utlede_kardiomyopati(df)
utlede_kardiomyopati <- function(df) {

  stopifnot(c("forlopstype",
              "kardiomyopati",
              "type_kardiomyopati") %in% names(df))

  df %>%
    dplyr::mutate(
      kategori_vt_kardiomyopati = factor(

        x = dplyr::case_when(

          .data$forlopstype == 2 &
            .data$kardiomyopati == 0 ~ "Uten kardiomyopati",

          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            .data$type_kardiomyopati == 1 ~ "Iskemisk KM (ICM)",

          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            .data$type_kardiomyopati == 2 ~ "Dilatert KM (DCM)",

          .data$forlopstype == 2 &
            .data$kardiomyopati == 1 &
            !(.data$type_kardiomyopati %in% 1:2) ~ "Annen KM",

          .data$forlopstype == 2 &
            .data$kardiomyopati == 9 ~ "Ukjent om kardiomyopati"),

        levels = c("Uten kardiomyopati",
                   "Iskemisk KM (ICM)",
                   "Dilatert KM (DCM)",
                   "Annen KM",
                   "Ukjent om kardiomyopati"),
        ordered = TRUE))


}





#' Hjertesvikt og/eller redusert EF
#'
#' Add variable \code{kategori_afli_hjsvikt_ef} for AFLI-procedyres, value
#' based on variables \code{hjertesvikt} and \code{ejekfrak}.
#'
#' @param df data.frame with Ablanor-data. Must contain variables
#' \code{forlopstype}, \code{hjertesvikt} and \code{ejekfrak}.
#'
#' @return Returns \code{df} with 1 new column
#' @export
#'
#' @examples
#'   df <- data.frame(
#'     forlopstype = c(NA, 2, rep(1, 8)),
#'     hjertesvikt = c(NA, 1, NA, 0, 0, 9, 9, 1, 1, 1),
#'     ejekfrak = c(NA, 9, 1, 9, 1, 1, 3, 1, 2, 3))
#'  ablanor::utlede_hjertesvikt_redusert_ef(df)
utlede_hjertesvikt_redusert_ef <- function(df) {

  stopifnot(c("forlopstype",
              "hjertesvikt",
              "ejekfrak") %in% names(df))

  df %>%
    dplyr::mutate(
      kategori_afli_hjsvikt_ef = factor(

        x = dplyr::case_when(
          .data$forlopstype ==  1 &
            (.data$hjertesvikt %in% 1 | .data$ejekfrak %in% 2:3)
          ~ "AFLI-Hjertesvikt og/eller redusert EF",

          .data$forlopstype ==  1 &
            !(.data$hjertesvikt %in% 1 | .data$ejekfrak %in% 2:3)
          ~ "AFLI-Verken hjertesvikt eller redusert EF",

          TRUE ~ NA_character_),

        levels = c("AFLI-Verken hjertesvikt eller redusert EF",
                   "AFLI-Hjertesvikt og/eller redusert EF"),

        ordered = TRUE))
}

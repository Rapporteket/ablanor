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




#' Add varibale bmi_klasse
#'
#' Add categories of Body Mass Index (BMI = weight in kilograms divided by
#' the square of height in meters).
#'
#' @param df data.frame with abalnor-data. Must contain variable
#' \code{bmi}.
#'
#' @return Returns \code{df} with one new column: \code{bmi_klasse}.
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
            .data$bmi >= 40 & .data$bmi < 45 ~ "Fedme, klasse III",
            .data$bmi >= 45 ~ "ugyldig",
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
          ordered = TRUE))

}



#' Add variable age (alder)
#'
#' Age in years at time of procedure. Calculated from variables
#' \code{birth_date} and \code{dato_pros}.
#'
#' @param df data.frame with AblaNor data. Must contain variables
#' \code{birth_date} and \code{dato_pros}
#' @return Returns \code{df} with one new variable: alder (= age in years)
#' @export
#' @examples x <- data.frame(
#'    birth_date = as.Date(rep("1951-12-30",3), format = "%Y-%m-%d"),
#'    dato_pros = as.Date(c("2021-05-29", "2021-12-29", "2021-12-30"),
#'    format = "%Y-%m-%d"))
#' ablanor::utlede_alder(x)
utlede_alder <- function(df) {

  stopifnot(all(c("birth_date", "dato_pros") %in% names(df)))
  stopifnot(df %>% dplyr::pull(birth_date) %>% lubridate::is.Date())
  stopifnot(df %>% dplyr::pull(dato_pros) %>% lubridate::is.Date())

  df %>%
    dplyr::mutate(
      alder = lubridate::as.period(
        x = lubridate::interval(start = birth_date, end = dato_pros),
        unit = "years")$year) %>%
    dplyr::relocate(alder, .after = birth_date)

}

#' Add binary variable alder_75
#'
#' Create binary variable \code{alder_75} from \code{alder}.
#' @param df ablanor data.frame, must contain variable \code{alder}. See also
#' \link[ablanor]{utlede_alder}.
#' @return Returns \code{df} with one new variable: alder_75
#' @export
#' @examples
#' x <- data.frame(alder = 73:76)
#' ablanor::utlede_alder_75(x)
utlede_alder_75 <- function(df) {

  stopifnot("alder" %in% names(df))
  df %>%
    dplyr::mutate(alder_75 = ifelse(alder >= 75,
                                    yes = ">=75",
                                    no = "<75")) %>%
    dplyr::relocate(alder_75, .after = alder)
}



#' Add variable aldersklasse
#'
#' Create variable \code{aldersklasse} from \code{alder}.
#' @param df ablanor data.frame, must contain variable \code{alder}. See also
#' \link[ablanor]{utlede_alder}.
#' @return Returns \code{df} with one new variable: aldersklasse
#' @export
#' @examples
#' x <- data.frame(alder = 73:76)
#' ablanor::utlede_aldersklasse(x)
utlede_aldersklasse <- function(df) {

  stopifnot("alder" %in% names(df))
  df %>%
    dplyr::mutate(
      aldersklasse = cut(
        tidyr::replace_na(alder, replace = 0),
        breaks = c(18, 49, 59, 69, 79, 89, 99),
        include.lowest = TRUE,
        labels = c("18-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
        ordered_result = TRUE)) %>%
    dplyr::relocate(aldersklasse, .after = alder)
}

#' Add variable alder_65_75
#'
#' Create variable \code{alder_65_75} from \code{alder}.
#' @param df ablanor data.frame, must contain variable \code{alder}. See also
#' \link[ablanor]{utlede_alder}.
#' @return Returns \code{df} with one new variable: alder_65_75
#' #' @details
#' \itemize{
#' \item{"<65"}
#' \item{"[65-75>"}
#' \item{">=75"}
#' \item{NA}
#' }
#' @export
#' @examples
#' x <- data.frame(alder = c(NA, 60:79))
#' ablanor::utlede_alder_65_75(x)
utlede_alder_65_75 <- function(df) {

  stopifnot("alder" %in% names(df))
  df %>%
    dplyr::mutate(alder_65_75 = dplyr::case_when(
      alder >= 75 ~ ">=75",
      alder >= 65 & alder <75 ~ "[65-75>",
      alder < 65 ~ "<65",
      is.na(alder) ~ NA_character_,
      TRUE ~ NA_character_)) %>%
    dplyr::relocate(alder_65_75, .after = alder)
}

#' Add variable bmi_manual
#' Add manually calculatet value of Body Mass Index
#'  (BMI = weight in kilograms divided by the square of height in meters).
#'
#' @param df data.frame with ablanor-data. Must contain variables
#' \code{hoyde} and \code{vekt}.
#' @return Returns \code{df} with 1 new column: \code{bmi_manual}
#' @export
#' @examples
#' df <- data.frame(hoyde = c(150, 160, 170), vekt = c(95, 85, 65))
utlede_bmi <- function(df) {

  stopifnot(c("hoyde", "vekt") %in% names(df))
  df %>%
    dplyr::mutate(
      bmi_manual = round(x = vekt / ((hoyde / 100) * (hoyde / 100)),
                         digits = 2)) %>%
    dplyr::relocate(bmi_manual, .after = vekt)

}

#' Add variables bmi_klasse and bmi_over35
#'
#' Add categories of Body Mass Index (BMI = weight in kilograms divided by
#' the square of height in meters).
#' @param df data.frame with ablanor-data. Must contain variable
#' \code{bmi}.
#' @return Returns \code{df} with 2 new columns: \code{bmi_klasse}
#' and \code{bmi_over35}
#' @export
#' @examples
#' df <- data.frame(bmi_manual = c(15, 15.2, 19, 25, 26.7, 32.1, 41.0, NA))
utlede_bmi_klasse <- function(df) {

  stopifnot(c("bmi_manual") %in% names(df))
  df %>%
    dplyr::mutate(
      bmi_klasse = factor(
        x = dplyr::case_when(
          bmi_manual < 18.5 ~ "Undervekt",
          bmi_manual >= 18.5 & bmi_manual < 25 ~ "Normalvekt",
          bmi_manual >= 25 & bmi_manual < 30 ~ "Overvekt",
          bmi_manual >= 30 & bmi_manual < 35 ~ "Fedme grad I",
          bmi_manual >= 35 & bmi_manual < 40 ~ "Fedme grad II",
          bmi_manual >= 40 & bmi_manual < 100 ~ "Fedme grad III",
          bmi_manual >= 100 ~ "ugyldig",
          TRUE ~ NA_character_),
        levels = c("Undervekt",
                   "Normalvekt",
                   "Overvekt",
                   "Fedme grad I",
                   "Fedme grad II",
                   "Fedme grad III",
                   "ugyldig"),
        ordered = TRUE),

      bmi_over35 = dplyr::if_else(bmi_manual >= 35,
                                  true = "BMI >=35",
                                  false = "BMI <35",
                                  missing = NA_character_))%>%
    dplyr::relocate(bmi_over35, .after = bmi_manual) %>%
    dplyr::relocate(bmi_klasse, .after = bmi_manual)
}




#' Add tidsvariabler
#'
#' Add variables \code{aar}, \code{maaned_nr} (numerical month) and
#' \code{maaned} (year + month) in data set based in \code{dato_pros}.
#' @param df data.frame with ablanor-data. Must contain \code{dato_pros}.
#' @return Returns \code{df} with 3 new columns
#' @export
#' @examples  df <- data.frame(
#' dato_pros = as.Date(c("2021-10-05", "1998-01-16", "2020-07-07"),
#'                     format = "%Y-%m-%d"))
#' ablanor::utlede_tidsvariabler(df)
utlede_tidsvariabler <- function(df) {

  stopifnot("dato_pros" %in% names(df))
  df %>%
    dplyr::mutate(
      aar_prosedyre = as.ordered(lubridate::year(dato_pros)),
      maaned_nr_prosedyre = as.ordered(sprintf(fmt = "%02d",
                                               lubridate::month(dato_pros))),
      maaned_prosedyre = ifelse(
        test = is.na(aar_prosedyre) | is.na(maaned_nr_prosedyre),
        yes = NA,
        no = paste0(aar_prosedyre, "-", maaned_nr_prosedyre))) %>%
    dplyr::relocate(c("aar_prosedyre",
                      "maaned_prosedyre",
                      "maaned_nr_prosedyre"), .after = dato_pros)

}





#' Kategori AFLI arytmi i48
#'
#' Add variable \code{kategori_afli_aryt_i48} for AFLI-procedures, based on
#' ICD codes.
#' @param df data.frame with Ablanor-data. Must contain variables
#' \code{forlopstype}, \code{aryt_i48_0}, \code{aryt_i48_1} and
#' \code{aryt_i48_1_underkat}.
#' @return returns \code{df} with one new column.
#' @export
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

          forlopstype == 1 & aryt_i48_0 == TRUE ~
            "AFLI-ICD 48.0 Paroksymal atrieflimmer",

          forlopstype == 1 &
            aryt_i48_1 == TRUE &
            aryt_i48_1_underkat == 1 ~
            "AFLI-ICD 48.1 Persisterende atrieflimmer",

          forlopstype == 1 &
            aryt_i48_1 == TRUE &
            aryt_i48_1_underkat == 2 ~
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
#' @param df data.frame with AblaNor data. Must contain \code{forlopstype},
#' \code{kardiomyopati}, \code{type_kardiomyopati}.
#' @return Returns \code{df} with 1 new column.
#' @export
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

          forlopstype == 2 &
            kardiomyopati == 0 ~ "Uten kardiomyopati",

          forlopstype == 2 &
            kardiomyopati == 1 &
            type_kardiomyopati == 1 ~ "Iskemisk KM (ICM)",

          forlopstype == 2 &
            kardiomyopati == 1 &
            type_kardiomyopati == 2 ~ "Dilatert KM (DCM)",

          forlopstype == 2 &
            kardiomyopati == 1 &
            !(type_kardiomyopati %in% 1:2) ~ "Annen KM",

          forlopstype == 2 &
            kardiomyopati == 9 ~ "Ukjent om kardiomyopati"),

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
          forlopstype ==  1 &
            (hjertesvikt %in% 1 | ejekfrak %in% 2:3)
          ~ "AFLI-Hjertesvikt og/eller redusert EF",

          forlopstype ==  1 &
            !(hjertesvikt %in% 1 | ejekfrak %in% 2:3)
          ~ "AFLI-Verken hjertesvikt eller redusert EF",

          TRUE ~ NA_character_),

        levels = c("AFLI-Verken hjertesvikt eller redusert EF",
                   "AFLI-Hjertesvikt og/eller redusert EF"),

        ordered = TRUE))
}


#' CHA2DS2VASc
#' Add variable \code{CHA2DS2VASc} score from 0 to 9.
#' @param df data.frame with Ablanor-data. Must contain variables
#' \code{hjertesvikt}, \code{hypertoni} and \code{diabetes},
#' \code{tia_slag}, \code{alder} and \code{gender}.
#' @return Returns \code{df} with 1 new column: CHA2DS2VASc
#' @details
#' \itemize{
#' \item{C- hjertesvikt:}{ score +1}
#' \item{H- hypertoni:}{ score +1}
#' \item{A2- alder(>=75):}{ score +2}#'
#' \item{D- diabetes:}{ score +1}
#' \item{S2- tia_slag:}{ score +2}
#' \item{V- karsykdom:}{ score +1}
#' \item{A2- alder(>=65; <75):}{ score +1}
#' \item{Sc- gender(female):}{ score +1}
#' }
#' @export
#' @examples
#'  x <- data.frame(
#'     hjertesvikt = c(NA, 2, rep(1, 8)),
#'     hypertoni = c(NA, 1, NA, 0, 0, 9, 9, 1, 1, 1),
#'     karsykdom = c(0, 1, 1, 1, 1, 0, 0, NA, NA, 9),
#'     diabetes = c(NA, 9, 1, 9, 1, 1, NA, 1, 0, 9),
#'     tia_slag = c(NA, 1, 1, 9, 1, 1, 0, 1, 0, 0),
#'     alder = c(18, 25, 64, 65, 70, 74, 75, 76, 80, 105),
#'     gender = c(rep(1, 4), NA, NA, rep(2, 4)))
#'     x %>% ablanor::utlede_alder_65_75() %>% ablanor::utlede_CHA2DS2VASc()
utlede_CHA2DS2VASc <- function(df) {

  stopifnot(c("hjertesvikt",
              "hypertoni",
              "karsykdom",
              "diabetes",
              "tia_slag",
              "alder",
              "gender") %in% names(df))
  df %>%
    dplyr::mutate(
      # Alderspoeng
      CHA2DS2VASc = dplyr::case_when(is.na(alder) ~ 0,
                                     alder < 65 ~ 0,
                                     alder >= 65 & alder < 75 ~ 1,
                                     alder >= 75 ~ 2,
                                     TRUE ~ NA)) %>%
    # Alle Andre variabler
    dplyr::mutate(CHA2DS2VASc = ifelse(test = hjertesvikt %in% 1,
                                       yes = CHA2DS2VASc + 1,
                                       no = CHA2DS2VASc)) %>%

    dplyr::mutate(CHA2DS2VASc = ifelse(test = hypertoni %in% 1,
                                       yes = CHA2DS2VASc + 1,
                                       no = CHA2DS2VASc)) %>%

    dplyr::mutate(CHA2DS2VASc = ifelse(test = karsykdom  %in% 1,
                                       yes = CHA2DS2VASc + 1,
                                       no = CHA2DS2VASc)) %>%

    dplyr::mutate(CHA2DS2VASc = ifelse(test = diabetes %in% 1,
                                       yes = CHA2DS2VASc + 1,
                                       no = CHA2DS2VASc)) %>%

    dplyr::mutate(CHA2DS2VASc = ifelse(test = tia_slag %in% 1,
                                       yes = CHA2DS2VASc + 2,
                                       no = CHA2DS2VASc)) %>%

    dplyr::mutate(CHA2DS2VASc = ifelse(test = gender  %in% 2,
                                       yes = CHA2DS2VASc + 1,
                                       no = CHA2DS2VASc))
}

#' CHA2DS2VA
#' Add variable \code{CHA2DS2VA} score from 0 to 8.
#' #'
#' @param df data.frame with Ablanor-data. Must contain variables
#' \code{hjertesvikt}, \code{hypertoni} and \code{diabetes},
#' \code{tia_slag} and \code{alder}.
#' @details
#' \itemize{
#' \item{C- hjertesvikt:}{ score +1}
#' \item{H- hypertoni:}{ score +1}
#' \item{A2- alder(>=75):}{ score +2}#'
#' \item{D- diabetes:}{ score +1}
#' \item{S2- tia_slag:}{ score +2}
#' \item{V- karsykdom:}{ score +1}
#' \item{A2- alder(>=65; <75):}{ score +1}
#' }
#' @return Returns \code{df} with 1 new column: CHA2DS2VASc
#' @export
#' @examples
#'   x <- data.frame(
#'     hjertesvikt = c(NA, 2, rep(1, 8)),
#'     hypertoni = c(NA, 1, NA, 0, 0, 9, 9, 1, 1, 1),
#'     karsykdom = c(0, 1, 1, 1, 1, 0, 0, NA, NA, 9),
#'     diabetes = c(NA, 9, 1, 9, 1, 1, NA, 1, 0, 9),
#'     tia_slag = c(NA, 1, 1, 9, 1, 1, 0, 1, 0, 0),
#'     alder = c(18, 25, 64, 65, 70, 74, 75, 76, 80, 105),
#'     gender = c(rep(1, 4), NA, NA, rep(2, 4)))
#'  ablanor::utlede_CHA2DS2VA(x)
utlede_CHA2DS2VA <- function(df) {

  stopifnot(c("hjertesvikt",
              "hypertoni",
              "karsykdom",
              "diabetes",
              "tia_slag",
              "alder") %in% names(df))
  df %>%
    dplyr::mutate(
      # Alderspoeng
      CHA2DS2VA = dplyr::case_when(is.na(alder) ~ 0,
                                   alder < 65 ~ 0,
                                   alder >= 65 & alder < 75 ~ 1,
                                   alder >= 75 ~ 2,
                                   TRUE ~ NA)) %>%
    # Alle Andre variabler
    dplyr::mutate(CHA2DS2VA = ifelse(test = hjertesvikt %in% 1,
                                     yes = CHA2DS2VA + 1,
                                     no = CHA2DS2VA)) %>%

    dplyr::mutate(CHA2DS2VA = ifelse(test = hypertoni %in% 1,
                                     yes = CHA2DS2VA + 1,
                                     no = CHA2DS2VA)) %>%

    dplyr::mutate(CHA2DS2VA = ifelse(test = karsykdom  %in% 1,
                                     yes = CHA2DS2VA + 1,
                                     no = CHA2DS2VA)) %>%

    dplyr::mutate(CHA2DS2VA = ifelse(test = diabetes %in% 1,
                                     yes = CHA2DS2VA + 1,
                                     no = CHA2DS2VA)) %>%

    dplyr::mutate(CHA2DS2VA = ifelse(test = tia_slag %in% 1,
                                     yes = CHA2DS2VA + 2,
                                     no = CHA2DS2VA))
}




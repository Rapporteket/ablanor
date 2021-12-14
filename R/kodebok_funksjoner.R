#' AblaNor kodebok functions
#'
#' Functions for adding and removing \code{listeverdi}- and \code{listetekst}-
#' variables in \emph{AblaNor}-data. Functions are:
#' \itemize{
#' \item kodebok_sjekk_foer_leggtil() checks compatibility between \code{df}
#' and \code{kb} for a given variable \code{verdi_variabel} and says whether
#' corresponding \code{tekst_variabel} can be added in \code{df}.
#'\item kodebok_sjekk_foer_fjerning() checks compatibility between \code{df}
#' and \code{kb} for a given variable \code{tekst_variabel} and says and whether
#' corresponding \code{verdi_variabel} can be removed from \code{df}.
#' \item kodebok_fyll_listetekstvar() adds some/all \emph{listetekst}-variables
#' in \code{df}.
#' \item kodebok_beholde_bare_listetekstvar() removes some/all
#' \emph{listeverdi}-variables from \code{df}.
#' }
#'
#' @param df data.frame containing variables from AblaNor e.g. \code{d_ablanor}.
#' @param kb data.frame with \code{kodeboken}. Must contain variables
#' \code{type}, \code{fysisk_feltnavn}, \code{listeverdier} and
#' \code{listetekst}. One row for each level of all variables.
#' @param ... If only selected \emph{listetekst}- or \emph{listeverdi}-
#' variables are to be added or removed in \code{df}, the selected variables
#' should be listed here, separated by commas.
#' @param suffiks string value that contains suffix of new \emph{listetekst}-
#' variables to be added to or \emph{listeverdi}-variables to be removed from
#' \code{df}. Default value is \emph{"_tekst"}.
#' @param koder is used to check compatibility between \code{df} and \code{kb}
#' for one variable. \code{koder} contains selected rows from \code{kb}.
#' @param verdi_variabel is used to check compatibility between \code{df} and
#'  \code{kb} for one variable. \code{verdi_variabel} is the name of variable
#'  with numerical values (\emph{listeverdier}).
#' @param tekst_variabel is used to check compatibility between \code{df} and
#'  \code{kb} for one variable. \code{tekst_variabel} is the name of variable
#'  with text values (\emph{listetekst}).
#' @param fjerne_suffiks_fra_navn boolean. If TRUE the \emph{listetekst}-
#' variables are renamed and suffix is removed from variable names. If FALSE,
#' variable-names are not changed and will contain suffix.
#' @param type String. Contains the \code{type} of variable as defined in
#' \code{kb} \emph{Listevariabel} or \emph{Avkrysninsboks}
#'
#'
#' @name kodebok_funksjoner
#' @aliases
#' kodebok_sjekk_foer_leggtil
#' kodebok_sjekk_foer_fjerning
#' kodebok_fyll_listetekstvar
#' kodebok_beholde_bare_listetekstvar
#'
#' @examples
#'  df <- data.frame(var1 = c(1:5, 1:5),
#'                   var2 = rep(c(0,1), 5))
#'  kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5), "var2", "var2"),
#'                   type = rep("Listevariabel", 7),
#'                   listeverdier = c(1:5, 0, 1),
#'                   listetekst = c(letters[1:5], "nei", "ja"))
#'
#'  ablanor::kodebok_sjekk_foer_leggtil(
#'                          df = df,
#'                          tekst_variabel = "var2_tekst",
#'                          verdi_variabel = "var2",
#'                          koder = kb %>%
#'                            dplyr::filter(.data$fysisk_feltnavn == "var2"))
#'
#' ablanor::kodebok_fyll_listetekstvar(df = df,
#'                                     kb = kb,
#'                                     suffiks = "_tekst")
#' df <- data.frame(var1 = c(1:5),
#'                  var1_tekst = letters[1:5],
#'                  var3 = c(0, 0, 1, 1, 1),
#'                  var3_tekst = c("nei", "nei", "ja", "ja", "ja"))
#'
#' kb <- data.frame(fysisk_feltnavn = c(rep("var1", 5), "var3", "var3"),
#'                  type = rep("Listevariabel", 7),
#'                  listeverdier = c(1:5, 0, 1),
#'                  listetekst = c(letters[1:5],"nei", "ja"))
#' ablanor::kodebok_sjekk_foer_fjerning(df = df,
#'                                      kb,
#'                                      verdi_variabel = "var3",
#'                                      tekst_variabel = "var3_tekst",
#'                                      type = "Listevariabel")
#' ablanor::kodebok_beholde_bare_listetekstvar(
#'                            df = df,
#'                            kb = kb,
#'                            suffiks = "_tekst",
#'                            fjerne_suffiks_fra_navn = TRUE)
NULL

#' @rdname kodebok_funksjoner
#' @export
kodebok_sjekk_foer_leggtil <- function(df,
                                       verdi_variabel,
                                       tekst_variabel,
                                       koder) {

  resultat_sjekk <- TRUE

  # Dersom en variabel allerede finnes med det nye navnet:
  if (tekst_variabel %in% names(df)) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Dersom en variabel har et/fleire nivå som mangler i kodeboken
  manglar_i_kb <- setdiff(
    df[[verdi_variabel]][!is.na(df[[verdi_variabel]])],
    koder$listeverdier)

  if (length(manglar_i_kb) > 0) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Dersom klokeboka har duplikerte verdiar for ein variabel:
  if (any(koder %>%  dplyr::select(.data$listeverdier) %>% duplicated())) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Dersom klokeboka har duplikert tekst for ein variabel:
  if (any(koder %>%  dplyr::select(.data$listetekst) %>% duplicated())) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Dersom ein variabel som er gitt i input ikkje finnes i Klokeboka
  if (nrow(koder) == 0) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  return(resultat_sjekk)
}





#' @rdname kodebok_funksjoner
#' @export
kodebok_sjekk_foer_fjerning <- function(df, kb, verdi_variabel,
                                        tekst_variabel,
                                        type = "Listevariabel") {
  resultat_sjekk <- TRUE


  if (!type %in% c("Listevariabel", "Avkrysningsboks")) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Dersom en variabel ikke har tilhøyrande numerisk variabel,
  # kan ikke fjerne variabel + return med en gang.
  if (!verdi_variabel %in% names(df)) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  # Idem dersom ein variabel ikkje har listetekst-variabel
  if (!tekst_variabel %in% names(df)) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }



  # Dersom ikkje samsvar mellom dei to variablane (dei er uavhengige)
  if (type == "Listevariabel") {

    df_sjekk <- data.frame(
      obserververte_verdier = df[[tekst_variabel]],

      forventede_verdier = kodebok_fyll_listetekstvar(
        df = df %>% dplyr::select(tidyselect::all_of(verdi_variabel)),
        kb = kb,
        suffiks = "_tekst") %>%
        dplyr::pull(tekst_variabel))

  } else if (type == "Avkrysningsboks") {

    df_sjekk <- data.frame(
      obserververte_verdier = df[[tekst_variabel]],

      forventede_verdier = kodebok_fyll_avkrysningsboks(
        df = df %>% dplyr::select(tidyselect::all_of(verdi_variabel)),
        kb = kb,
        suffiks = "_tekst") %>%
        dplyr::pull(tekst_variabel))
  }


  # Dersom ikke samsvar:
  if (df_sjekk %>%
      dplyr::filter(!is.na(.data$obserververte_verdier) &
                    !is.na(.data$forventede_verdier)) %>%
      dplyr::filter(.data$obserververte_verdier !=
                    .data$forventede_verdier) %>%
      nrow() > 0) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  #Dersom bare en er NA:
  if (df_sjekk %>%
      dplyr::filter((is.na(.data$obserververte_verdier) &
                     !is.na(.data$forventede_verdier)) |
                    (is.na(.data$obserververte_verdier) &
                     !is.na(.data$forventede_verdier))) %>%
      nrow() > 0) {
    resultat_sjekk <- FALSE
    return(resultat_sjekk)
  }

  return(resultat_sjekk)
}

#' @rdname kodebok_funksjoner
#' @export
kodebok_fyll_avkrysningsboks <- function(df, kb, ..., suffiks = "_tekst") {

  . <- ""
  # Kontrollere at klokeboken inneholder obligatoriske variabler
  # TODO : Kan denne teste flyttes ut til getKlokebok?
  stopifnot(all(c("fysisk_feltnavn",
                  "listeverdier",
                  "listetekst",
                  "type") %in% names(kb)))

  # Ta videre kun avkrysningsboks-variabler
  kb %<>%
    dplyr::filter(.data$type == "Avkrysningsboks")


  # Dersom en/flere variabler er gitt som input i funksjonen, legges bare
  # tekstvariabler til for disse.
  arg <- rlang::quos(...)
  verdi_variabel_d <- rlang::quos_auto_name(arg) %>% names()

  if (length(verdi_variabel_d) == 0) {
    # Dersom ingen variabler er angitt i funksjonen, brukes alle variabler
    # som er felles mellom kb og datasettet.
    verdi_variabel_d <-  intersect(names(df), kb$fysisk_feltnavn)
  }



  # For hver av variablene, sjekk om ny kan legges til og eventuelt gjøre dette

  koder <- data.frame(listeverdier = c(0, 1),
                      listetekst = c("nei", "ja"))
  for (i in seq_along(verdi_variabel_d)) {
    verdi_variabel <- verdi_variabel_d[i]
    tekst_variabel <- paste0(verdi_variabel, suffiks)

    sjekk_kb <- ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      verdi_variabel = verdi_variabel,
      tekst_variabel = tekst_variabel,
      koder = koder)

    #Dersom alle sjekker ok, legges ein ny variabel til i datasettet:
    if (sjekk_kb) {
      df %<>%
        dplyr::mutate("{tekst_variabel}" := factor(
          x = .data[[verdi_variabel]],
          levels = koder$listeverdier,
          labels = koder$listetekst,
          ordered = TRUE)) %>%
        dplyr::relocate(., tekst_variabel, .after = verdi_variabel)
    }

  }
  df

}





#' @rdname kodebok_funksjoner
#' @export
kodebok_fyll_listetekstvar <- function(df, kb, ..., suffiks = "_tekst") {


  . <- ""
  # Kontrollere at klokeboken inneholder obligatoriske variabler
  # TODO : Kan denne teste flyttes ut til getKlokebok?
  stopifnot(all(c("fysisk_feltnavn",
                  "listeverdier",
                  "listetekst",
                  "type") %in% names(kb)))

  # Ta videre kun listevariabler
  kb %<>%
    dplyr::filter(.data$type == "Listevariabel")


  # Dersom en/flere variabler er gitt som input i funksjonen, legges bare
  # tekstvariabler til for disse.
  arg <- rlang::quos(...)
  verdi_variabel_d <- rlang::quos_auto_name(arg) %>% names()

  if (length(verdi_variabel_d) == 0) {
    # Dersom ingen variabler er angitt i funksjonen, brukes alle variabler
    # som er felles mellom kb og datasettet.
    verdi_variabel_d <-  intersect(names(df),
                                   kb$fysisk_feltnavn)
  }



  # For hver av variablene, sjekk om ny kan legges til og eventuelt gjøre dette
  for (i in seq_along(verdi_variabel_d)) {
    verdi_variabel <- verdi_variabel_d[i]
    koder <- kb %>%
      dplyr::filter(.data$fysisk_feltnavn %in% verdi_variabel_d[i])
    tekst_variabel <- paste0(verdi_variabel, suffiks)

    sjekk_kb <- ablanor::kodebok_sjekk_foer_leggtil(
      df = df,
      verdi_variabel = verdi_variabel,
      tekst_variabel = tekst_variabel,
      koder = koder)

    #Dersom alle sjekker ok, legges ein ny variabel til i datasettet:
    if (sjekk_kb) {
      df %<>%
        dplyr::mutate("{tekst_variabel}" := factor(
          x = .data[[verdi_variabel]],
          levels = koder$listeverdier,
          labels = koder$listetekst,
          ordered = TRUE)) %>%
        dplyr::relocate(., tekst_variabel, .after = verdi_variabel)
    }

  }
  df

}


#' @rdname kodebok_funksjoner
#' @export
kodebok_beholde_bare_listetekstvar <- function(df,
                                               kb, ...,
                                               suffiks = "_tekst",
                                               fjerne_suffiks_fra_navn = TRUE) {

  . <- ""
  # Dersom en/flere variabler er gitt som input i funksjonen, fjernes bare
  # listeverdi-variabler for disse.
  arg <- rlang::quos(...)
  alle_med_suffiks <- rlang::quos_auto_name(arg) %>% names()

  if (length(alle_med_suffiks) == 0) {
    # Dersom ingen variabler er spesifisert, går vi gjennom
    # alle variabler med suffikset
    alle_med_suffiks <- df %>%
      dplyr::select(tidyselect::contains(suffiks)) %>%
      names() %>%
      gsub(x = ., pattern = suffiks, replacement = "") %>%
      unlist()
  }




  # for alle med suffiks, sjekk at fjerning er OK og gjøre dette
  for (i in seq_along(alle_med_suffiks)) {
    verdi_variabel <- alle_med_suffiks[i]
    tekst_variabel <- paste0(verdi_variabel, suffiks)
    type <- kb %>%
      dplyr::filter(.data$fysisk_feltnavn == verdi_variabel) %>%
      dplyr::pull(.data$type) %>%
      unique()

    sjekk_kb <- ablanor::kodebok_sjekk_foer_fjerning(
      df = df,
      kb = kb,
      verdi_variabel = verdi_variabel,
      tekst_variabel = tekst_variabel,
      type = type)

    #Dersom alle sjekker ok, fjernes numerisk variabel fra i datasettet:
    if (sjekk_kb & fjerne_suffiks_fra_navn) {
      df %<>%
        dplyr::select(-verdi_variabel) %>%
        dplyr::rename("{verdi_variabel}"  := tekst_variabel)
    }


    #Dersom alle sjekker ok, fjernes numerisk variabel fra i datasettet:
    if (sjekk_kb & !fjerne_suffiks_fra_navn) {
      df %<>%
        dplyr::select(-verdi_variabel)
    }

  }
  df
}

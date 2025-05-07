#' Regn ut Kvalitetsindikator - Andel
#' Funksjon for å regne ut kvalitetsindikatorer for andeler. Tar inn et
#' datasett på 101-format, og returnerer et estimert resultat
#' @param d_ki_ind Inndata på 101-format
#' @param alfa Verdi for å bestemme bredde på konfidensintervall,
#' standardverid er 0.05
#' @param forenklet bools. FALSE gir estimatet bare etter testing av d_ki_ind.
#' TRUE gir estimatet uten testing.
#'
#' @return Sammendrag av estimert proporsjon med konfidensintervall.
#' @export
#'
#' @examples
#' \dontrun{
#' aggreger_ki_prop(d_ki_ind, alfa = 0.05)
#' }
aggreger_ki_prop <- function(d_ki_ind, alfa = 0.05, forenklet = FALSE) {


  if (forenklet == FALSE) {
    if (!(is.data.frame(d_ki_ind) &&
          all(c("ki_krit_teller", "ki_krit_nevner") %in% names(d_ki_ind)))) {
      stop("Inndata maa vaere tibble/data.frame med kolonnene
           'ki_krit_teller' og 'ki_krit_nevner'")
    }
    if (!(is.logical(d_ki_ind$ki_krit_teller) &&
          is.logical(d_ki_ind$ki_krit_nevner))) {
      stop("Kriterievariablene maa vaere boolsk")
    }
    if (!all(d_ki_ind$ki_krit_nevner %in% c(T, F))) {
      stop("'ki_krit_nevner' maa vaere TRUE eller FALSE")
    }
    if (!all((d_ki_ind$ki_krit_teller %in% c(F, T, NA)) &
             ((d_ki_ind$ki_krit_teller %in% c(F, T) &
               d_ki_ind$ki_krit_nevner == T) |
              (d_ki_ind$ki_krit_teller %in% c(F, NA) &
               d_ki_ind$ki_krit_nevner == F)))) {
      stop("'ki_krit_teller' maa vaere TRUE eller FALSE hvis 'ki_krit_nevner' er
           TRUE, og FALSE eller NA hvis 'ki_krit_nevner' er FALSE")
    }
    if (any(lengths(attr(d_ki_ind, "groups")$.rows) == 0)) {
      warning("Det finnes grupper uten observasjoner i grupperingsvariabel")
    }
  }

  d_sammendrag <- d_ki_ind %>%
    dplyr::summarise(ki_teller = as.integer(sum(.data$ki_krit_teller,
                                                      na.rm = TRUE)),

                     ki_nevner = as.integer(sum(.data$ki_krit_nevner)),

                     est = .data$ki_teller / .data$ki_nevner) %>%
    dplyr::select(!!!dplyr::groups(d_ki_ind),
                  "est",
                  "ki_teller",
                  "ki_nevner")

  konfint <- binom::binom.wilson(d_sammendrag$ki_teller, d_sammendrag$ki_nevner,
                                conf.level = 1 - alfa)
  d_sammendrag$konfint_nedre <- konfint$lower
  d_sammendrag$konfint_ovre <- konfint$upper
  d_sammendrag %<>%
    dplyr::mutate_at(dplyr::vars(.data$est,
                                 .data$konfint_nedre,
                                 .data$konfint_ovre),
                     tidyr::replace_na,
                     replace = NA)
  d_sammendrag
}

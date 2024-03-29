#' Make standard table for rmarkdown reports
#'
#' Functions that will return tables used in reports.
#'
#' \code{mst()} creates RMarkdown code for creating standard tables.
#' \code{mst()} creates RMarkdown code for creating standard tables, when kable
#' is made already
#'
#' @param tab Data frame or matrix represetnting the table
#' @param col_names Character vector with column names. Defaults
#' \code{colnames(tab)}
#' @param type Character string defining output, either 'html' or 'latex'.
#' Default is 'latex'
#' @param cap Character string with table caption. Empty string by default
#' @param label Character string defining the label in case the table needs to
#' be referenced elsewhere in the overall document. For instance, setting this
#' to 'my_table' the corresponding inline rmarkdown reference to use is
#' \code{\\@ref(tab:my_table)}. Please note that for this to work for both
#' LaTex and HTML the bookdown document processing functions must be used,
#' \emph{i.e.} bookdown:pdf_document2() and bookdown::html_document2(),
#' respectively. Default value is \code{knitr::opts_current$get("label")}.
#' @param digs Numeric number of digits to use. = by default
#' @param align Character vector specifying column alignment in the LaTeX way,
#' \emph{e.g.} \code{c("l", "c", "r")} will align the first column to the left,
#' center the second and right-aling the last one. Default is NULL in which case
#' @param fs Numeric providing the font size. Only apply for LaTeX output.
#' Default value is 14
#' @param lsd Logical if table is to be scaled down. Only apply for LaTeX
#' output. FALSE by default
#' @param full_width Logical, if table should use full page width. FALSE by
#' default

#'
#' @return Character string containing RMarkdown table code or an R data object
#' @name makeStandardTable
#' @aliases mst
#' @examples
#' mst(tab = mtcars[1:10, ])
NULL


#' @rdname makeStandardTable
#' @export
mst <- function(tab, col_names = colnames(tab), type = "pdf", cap = "",
                label = "", digs = 0, align = NULL, fs = 14, lsd = FALSE) {

  if (type == "pdf") {
    if (lsd) {
      lo <- c("HOLD_position", "scale_down")
    } else {
      lo <- c("HOLD_position")
    }
    k <- knitr::kable(tab, format = "latex", col.names = col_names,
                      caption = cap,
                      label = label, digits = digs,
                      align = align, booktabs = TRUE) %>%
      kableExtra::kable_styling(latex_options = lo, font_size = fs)
  }

  if (type == "html") {
    k <- knitr::kable(tab, format = "html", col.names = col_names,
                      caption = cap,
                      label = label, digits = digs,
                      align = align) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE)
  }
  k
}


#' @rdname makeStandardTable
#' @export
mst_short <- function(tab, col_names = colnames(tab), type = "pdf", cap = "",
                label = "", digs = 0, align = NULL, fs = 14, lsd = FALSE,
                full_width = FALSE) {

  if (type == "pdf") {
    if (lsd) {
      lo <- c("HOLD_position", "scale_down")
    } else {
      lo <- c("HOLD_position")
    }
    k <- kableExtra::kable_styling(kable_input = tab,
                                   latex_options = lo,
                                   font_size = fs)
  }

  if (type == "html") {
    k <- kableExtra::kable_styling(kable_input = tab,
                                  bootstrap_options = c("striped",
                                                        "hover",
                                                        "condensed"),
                                  full_width = full_width)
  }
  k
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utlede_variabler.R
\name{utlede_hjertesvikt_redusert_ef}
\alias{utlede_hjertesvikt_redusert_ef}
\title{Hjertesvikt og/eller redusert EF}
\usage{
utlede_hjertesvikt_redusert_ef(df)
}
\arguments{
\item{df}{data.frame with Ablanor-data. Must contain variables
\code{forlopstype}, \code{hjertesvikt} and \code{ejekfrak}.}
}
\value{
Returns \code{df} with 1 new column
}
\description{
Add variable \code{kategori_afli_hjsvikt_ef} for AFLI-procedyres, value
based on variables \code{hjertesvikt} and \code{ejekfrak}.
}
\examples{
  df <- data.frame(
    forlopstype = c(NA, 2, rep(1, 8)),
    hjertesvikt = c(NA, 1, NA, 0, 0, 9, 9, 1, 1, 1),
    ejekfrak = c(NA, 9, 1, 9, 1, 1, 3, 1, 2, 3))
 ablanor::utlede_hjertesvikt_redusert_ef(df)
}

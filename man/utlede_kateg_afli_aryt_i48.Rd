% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utlede_variabler.R
\name{utlede_kateg_afli_aryt_i48}
\alias{utlede_kateg_afli_aryt_i48}
\title{Kategori AFLI arytmi i48}
\usage{
utlede_kateg_afli_aryt_i48(df)
}
\arguments{
\item{df}{data.frame with Ablanor-data. Must contain variables
\code{forlopstype}, \code{aryt_i48_0}, \code{aryt_i48_1} and
\code{aryt_i48_1_underkat}.}
}
\value{
returns \code{df} with one new column.
}
\description{
Add variable \code{kategori_afli_aryt_i48} for AFLI-procedures, based on
ICD codes.
}
\examples{
df <- data.frame(
   forlopstype = rep(1, 4),
   aryt_i48_0 = c(0, 1, 0, 0),
   aryt_i48_1 =  c(0, 0, 1, 2),
   aryt_i48_1_underkat = c(NA, NA, 1, 2))
ablanor::utlede_kateg_afli_aryt_i48(df)
}

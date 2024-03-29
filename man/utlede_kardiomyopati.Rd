% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utlede_variabler.R
\name{utlede_kardiomyopati}
\alias{utlede_kardiomyopati}
\title{VT Kardiomyopati}
\usage{
utlede_kardiomyopati(df)
}
\arguments{
\item{df}{data.frame with AblaNor data. Must contain \code{forlopstype},
\code{kardiomyopati}, \code{type_kardiomyopati}.}
}
\value{
Returns \code{df} with 1 new column.
}
\description{
Add variable \code{kategori_vt_kardiomyopati} for VT-procedures,
based on variables \code{kardiomyopati} and \code{type_kardiomyopati}.
}
\examples{
df <- data.frame(
   forlopstype = c(NA, 1, rep(2,8)),
   kardiomyopati =  c(NA, NA, 9, 0, rep(1, 6)),
   type_kardiomyopati = c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 99))
ablanor::utlede_kardiomyopati(df)
}

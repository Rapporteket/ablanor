% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utlede_variabler.R
\name{utlede_aldersklasse}
\alias{utlede_aldersklasse}
\title{Add variable aldersklasse}
\usage{
utlede_aldersklasse(df)
}
\arguments{
\item{df}{ablanor data.frame, must contain variable \code{alder}. See also
\link[ablanor]{utlede_alder}.}
}
\value{
Returns \code{df} with one new variable: aldersklasse
}
\description{
Create variable \code{aldersklasse} from \code{alder}.
}
\examples{
x <- data.frame(alder = 73:76)
ablanor::utlede_aldersklasse(x)
}

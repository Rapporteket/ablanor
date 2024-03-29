% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hjelpefunksjoner.R
\name{legg_til_forlopstype_kortnavn}
\alias{legg_til_forlopstype_kortnavn}
\title{Hent kortnavn for forløpstype}
\usage{
legg_til_forlopstype_kortnavn(df, total = FALSE, langtnavn = FALSE)
}
\arguments{
\item{df}{prosedyredatasett som inkluderer forlopstype}

\item{total}{bools, dersom FALSE har vi ikkje en rad med totalsum. Dersom
TRUE har vi en rad med totalsum.}

\item{langtnavn}{boolsk. Dersom TRUE bruker vi lang tekst (ikke forkortning)
#'}
}
\value{
Returnerer inndata med en ekstra kolonne 'forlopstype_tekst'
som er en faktor med kortnavn for de ulike forløpstypene.
}
\description{
Henter inn kortnavn for forløpstyper som registreres i
AblaNor. Disse er i hovedsak ment for bruk i figurer og tabeller for å
forbedre lesbarhet.
}
\examples{
\dontrun{
legg_til_forlopstype_kortnavn(df = d_ablanor, total = TRUE, langtnavn = FALSE)
}
}

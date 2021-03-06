% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggRegDataSQLV2V3.R
\name{RyggRegDataSQLV2V3}
\alias{RyggRegDataSQLV2V3}
\title{Henter data registrert for Degenerativ Rygg}
\usage{
RyggRegDataSQLV2V3(
  datoFra = "2007-01-01",
  datoTil = "2099-01-01",
  alleVarV3 = 1
)
}
\arguments{
\item{datoFra}{Tidligste operasjonsdato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste operasjonsdato i utvalget (vises alltid i figuren).}

\item{alleVarV3}{0: fjerner variabler som ikke er i bruk på Rapporteket (standard),
1: har med alle variabler fra V3}
}
\value{
RegData, dataramme med data f.o.m. 2007.
}
\description{
Henter data for Degenerativ Rygg og kobler samme versjon 2 og versjon 3
}

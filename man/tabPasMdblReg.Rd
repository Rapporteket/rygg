% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggTabeller.R
\name{tabPasMdblReg}
\alias{tabPasMdblReg}
\title{Finner pasienter med potensielt dobbeltregistrerte skjema}
\usage{
tabPasMdblReg(RegData, datoFra = "2019-03-01", tidsavvik = 30)
}
\arguments{
\item{RegData}{dataramme fra nakkeregisteret, tidligst fra 01-01-2019}

\item{tidssavik}{\itemize{
\item maks tidsavvik (dager) mellom to påfølgende registreringer som sjekkes
}}
}
\value{
mulig dobbeltregistrerte skjema
}
\description{
Finner pasienter med potensielt dobbeltregistrerte skjema
}

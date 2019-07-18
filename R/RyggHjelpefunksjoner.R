# Må det kanskje komme en overornet tittel her?
#---------------------------------------------

#' Hjelpefunksjoner. Group of functions page title
#'
#' Fil med div hjelpefunksjoner.Group of functions Description section
#'
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' @section Finne reinnleggelser After function section:
#' Despite its location, this actually comes after the function section.
#' Fil som inneholder hjelpefunksjoner.
#' FinnReinnleggelser beregner reinnleggelser fra DateAdmittedIntensive og DateDischargedIntensive
#' SorterOgNavngiTidsEnhet Legger til tidsenhetene Aar, Halvaar, Mnd og Kvartal
#'
#'
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
# @inheritParams NIRFigAndeler
#' @return Div hjelpefunksjoner
#' @name hjelpeFunksjoner
NULL
#' @rdname hjelpeFunksjoner
#' @export


#' @section Tilrettelegge tidsenhetvariabel:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @rdname hjelpeFunksjoner
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar', tab=0) {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                          +(RegData$Aar-min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      format.Date(seq(from=as.Date('2018-01-01'),
                      to=as.Date('2018-09-01'), by='month'), format = '%b%y')

      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #           sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       #Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       # Mnd = format.Date(seq(from=min(as.Date(RegData$InnDato), na.rm = T),
                       #                       to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%b%y'),
                       #Henter fullt månedsnavn og forkorter etterpå.
                       Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'),
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))

      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}
      #RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      #RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      #a <- factor(c(1:10,3,2,4,3,7,9,4), levels=1:11, labels = letters[1:11])
#table(a)

      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}
#' @section Lage tulledata (simulerte data)
# Probably better if all sections come first, uless have one section per function(?)
#' @rdname hjelpeFunksjoner
#' @export
lageTulleData <- function(RegData, varBort='', antSh=26, antObs=20000) {
      library(synthpop)
      library(dplyr)
      #ForlopsID <- RegData$ForlopsID
      RegData <- RegData[,-which(names(RegData) %in% varBort)]
      RegData <- RegData[sample(1:dim(RegData)[1], antObs, replace = T),]
      sykehus <- paste('Sykehus', LETTERS[1:antSh])
      fordelingPasienter <- sample(1:10,antSh, replace = TRUE)
      RegData$ShNavn <- sample(sykehus, prob=fordelingPasienter/sum(fordelingPasienter), size=dim(RegData)[1], replace = TRUE)
      RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn)
	  return(RegData)
}

#' @section Legge til indikator for intervensjon, pårørendeoppfølging
#' @rdname hjelpeFunksjoner
#' @export
leggTilIntervensjon <- function(RegData, #startDatoPre='2011-01-01', sluttDatoPre='2016-10-01',
                                 startDatoIntervensjon='2016-10-01', sluttDatoIntervensjon=Sys.Date()){
      RegData$Intervensjon <- 0
      RegData$Intervensjon[intersect(which(RegData$InnDato >= as.Date(startDatoIntervensjon)),
                                     which(RegData$InnDato <= as.Date(sluttDatoIntervensjon)))] <- 1
      return(RegData)
}


#' @section Automatisk linjebryting av lange tekstetiketter
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @rdname hjelpeFunksjoner
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}

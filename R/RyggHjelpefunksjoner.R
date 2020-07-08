#' Hjelpefunksjoner. Group of functions page title
#'
#' Fil med div hjelpefunksjoner.Group of functions Description section
#'
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#'
#'

#' Tilrettelegge tidsenhetvariabel:
#'
#' @param RegData registerdata
#' @param tidsenhet 'AAr', 'Halvaar', 'Kvartal' eller 'Mnd'
#' @param tab ? hmm...
#'
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
  # format.Date(seq(from=as.Date('2018-01-01'),
  #                 to=as.Date('2018-09-01'), by='month'), format = '%b%y')

  tidtxt <- switch(tidsenhet,
                   Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'),
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                   Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                   sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                   Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                   sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                   Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))

  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
  if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}
  RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)

  UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
  return(UtData)
}

#' Lage tulledata (simulerte data)
#'
#' @param RegData Ekte data som skal rotes til
#' @param varBort variable som finnes i begge filer
#' @param antSh antall sykehus
#' @param antObs antall observasjoner
#'
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

#' Automatisk linjebryting av lange tekstetiketter
#'
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @return automatisk linjebryting
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}


#' Generere data til Resultatportalen
#'
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtVar - beinsmLavPre, peropKompDura, sympVarighUtstr
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilResPort <- function(RegData = RegData, valgtVar, datoFra = '2011-01-01', aar=0,
                                    hovedkat=99, hastegrad=99, tidlOp='', filUt='dummy'){

#2019-09-11: hovedkategori er ikke definert! Inntil videre
  #   if (valgtVar=='symptVarighUtstr_pro') {
  #   valgtVar <- 'sympVarighUtstr'
  #   hovedkat <- 1}
  # if (valgtVar=='beinsmLavPre_pro') {
  #   valgtVar <- 'beinsmLavPre'
  #   hovedkat <- 1}
  # if (valgtVar=='kpInf3Mnd_pro') {
  #   valgtVar <- 'kpInf3Mnd'
  #   hovedkat <- 1}
  # if (valgtVar=='kpInf3Mnd_SS') {
  #   valgtVar <- 'kpInf3Mnd'
  #   hovedkat <- 8}
  # if (valgtVar=='peropKompDura_proPrimElek') {
  #   valgtVar <- 'peropKompDura'
  #   hovedkat <- 1
  #   tidlOp <- 4
  #   hastegrad <- 1}
  # if (valgtVar=='peropKompDura_SSPrimElek') {
  #   valgtVar <- 'peropKompDura'
  #   hovedkat <- 8}

  filUt <- paste0('RyggTilOff', ifelse(filUt=='dummy',  valgtVar, filUt), '.csv')
  RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  RyggUtvalg <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, aar=aar, hastegrad = hastegrad, tidlOp=tidlOp) #datoFra = datoFra) #, hovedkat=hovedkat) # #, datoTil=datoTil)
  RegData <- RyggUtvalg$RegData
  RyggTilOffvalgtVar <- RegData[,c('Aar', "ShNavn", "ReshId", "Variabel")]
  info <- c(RyggVarSpes$tittel, RyggUtvalg$utvalgTxt)
  RyggTilOffvalgtVar$info <- c(info, rep(NA, dim(RyggTilOffvalgtVar)[1]-length(info)))
  #write.table(RyggTilOffvalgtVar, file = paste0('A:/Resultatportalen/', filUt), sep = ';', row.names = F) #, fileEncoding = 'UTF-8')
  return(invisible(RyggTilOffvalgtVar))


}#' Beregn andel registrereinger som er ferdigstilt viss lang tid etter operasjon.
#'
#'Forsinkelse er strengt tatt registrering og ferdigstillelse etter UtskrivelseDato,
#' men mht hva som er for sent for å sende ut oppfølgingsskjema, regnes forsinkelse
#' som tid etter operasjon
#'
#' @param RegData dataramme med reshID, FirstTimeClosed og operasjonsdato
#' @param fraDato startdato for perioden en ønsker å se på
#' @param tilDato sluttdato for perioden en ønsker å se på
#' @param forsinkelse minste antall dager fra operasjon til ferdigstillelse
#' @param reshID Avdelingas reshID. Benyttes til å filtrere.
#' @return
#' @export
forsinketReg <- function(RegData, fraDato, tilDato, forsinkelse, reshID){
  RegData$Diff <- as.numeric(difftime(as.Date(RegData$MedForstLukket),
                                      RegData$OpDato ,units = 'days')) #UtskrivelseDato
  Data <- RegData[ , c('OpDato', 'MndAar', 'Diff', 'ReshId')]%>%
    dplyr::filter(ReshId == reshID & OpDato > fraDato & (OpDato < tilDato))
  paste0(sum(as.numeric(Data$Diff)>forsinkelse, na.rm = T), ' (',
         100*round(sum(as.numeric(Data$Diff)>forsinkelse, na.rm = T)/dim(Data)[1],1), '%)')
}



#' Funksjon som produserer rapporten som skal lastes ned av mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending, dvs. (\emph{ uten ".Rnw"})
#' @param reshID Brukerens reshid
#' @param filnavn brukes av downloadHandler
#' @param datoFra startdato
#' @param datoTil sluttdato
#' @return Filsti til pdf-rapporten.
#' @export
henteSamlerapporter <- function(filnavn, rnwFil, reshID=0,
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package='rygg'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
}


#' Funksjon som produserer rapporten som skal sendes til mottager.
#' (The actual call to this function is made through do.call and
#' has the effect of providing the parameters as class
#' \emph{list}. Verdier gis inn som listeparametre
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param reshID Aktuell reshid
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @return Full path of file produced
#' @export
abonnementRygg <- function(rnwFil, brukernavn='tullebukk', reshID=0,
                            datoFra=Sys.Date()-180, datoTil=Sys.Date()) {

  raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Rygg',
                    reshId = reshID[[1]], msg = "Abonnement: månedsrapport")

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='rygg'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile)

  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Rygg',
                    reshId = reshID[[1]], msg = paste("Sendt: ", utfil))
  return(utfil)
}

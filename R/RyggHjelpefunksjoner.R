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


#' Generere data til Resultatportalen/SKDE-viser
#'
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtVar - beinsmLavPre, peropKompDura, sympVarighUtstr, p.t. 10 kvalitetsind.
#' @param indID indikator-id, eks. 'ind1', 'ind2', osv.
#' @param ResPort 1-hvis data til resultatportalen (standard), 0-data til SKDE-viser
#' @inheritParams RyggUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilOffVisning <- function(RegData = RegData, valgtVar, datoFra = '2011-01-01', aar=0, ktr=0,
                           indID = 'indDummy', ResPort=1,
                           hovedkat=99, hastegrad=99, tidlOp='', lastNedFil=0, filUt='dummy'){


  filUt <- paste0('Rygg', ifelse(filUt=='dummy',  valgtVar, filUt), c('_SKDE', '_ResPort')[ResPort+1],'.csv')
  RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype = 'andelGrVar')
  RegData <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, aar=aar, hastegrad = hastegrad,
                              tidlOp=tidlOp, hovedkat=hovedkat)$RegData      #datoFra = datoFra) #) # #, datoTil=datoTil)

  if (ResPort == 1){
    #Variabler: Aar	ReshId	Teller Ind1	Nevner Ind1	  AarID	   Indikator
    #          2014	103469	  0	          1	       2014103469	  ind1
    RegDataUt <- RegData[,c('Aar', "ReshId", "ShNavn", "Variabel")]
    RegDataUt<- dplyr::rename(RegDataUt, Teller = Variabel)
    RegDataUt$AarID <- paste0(RegDataUt$Aar, RegDataUt$ReshId)
    RegDataUt$Indikator <- indID
    RegDataUt$Nevner <- 1
  }

  if (ResPort == 0){
    #Variabler: year, orgnr, var, denominator, ind_id
    RegDataUt <- RegData #[,c('Aar', "ReshId", "Variabel")]
    RegDataUt$ind_id <- indID
    RegDataUt$denominator <- 1
  # nytt navn = gammelt navn
    RegDataUt <- dplyr::rename(RegDataUt,
                           year = Aar,
                           var = Variabel)

  #Legge på orgID ("Sykehusviser")
  #ReshId	orgnr	RapporteketNavn	SKDEnavn
  nyID <- c('999976' = '974706490',	#Ahus	Ahus
            '107508' = '974518821',	#Aleris Bergen	Aleris Bergen
            '107240' = '879595762',	#Aleris Drammen	Aleris Drammen
            '999975' = '981541499',	#Aleris Oslo	Aleris Colosseum Nobel
            '999994' = '983896383',	#Aleris Stavanger	Aleris Colosseum Stavanger
            '100133' = '974631091',	#Arendal	Arendal
            '100968' = '974795361',	#Bodø	Bodø
            '103094' = '974705788',	#Bærum	Bærum
            '103618' = '974631326',	#Drammen	Drammen
            '111127' = '974631768',	#Elverum	Elverum
            '100415' = '974595214',	#Flekkefjord	Flekkefjord
            '100316' = '974744570',	#Førde	Førde
            '111150' = '974632535',	#Gjøvik	Gjøvik
            '999978' = '974724774',	#Haugesund	Haugesund
            '105588' = '974557746',	#Haukeland, nevrokir	Haukeland
            '111961' = '974557746',	#Haukeland, ort	Haukeland
            '4209772' = '887987122',	#Ibsensykehuset	Ibsensykehuset Porsgrunn
            '999900' = '920500234', #Kolibri Medical Group  Kolibri Sandnes
            '100407' = '974733013',	#Kristiansand	Kristiansand
            '111068' = '974746948',	#Kristiansund	Kristiansund
            '102949' = '874743372',	#Kysthospitalet Hagevik	Kysthospitalet i Hagevik
            '105798' = '974754118',	#Levanger	Levanger
            '111185' = '874632562',	#Lillehammer	Lillehammer
            '110633' = '974116588',	#Martina Hansens	Martina Hansens hospital
            '111065' = '974745569',	#Molde	Molde
            '105899' = '974753898',	#Namsos	Namsos
            '104279' = '972140295',	#NIMI	NIMI
            '999998' = '991835083',	#Oslofjordklinikken	Oslofjordklinikken Sandvika
            '999920' = '913758862',	#Oslofjordklinikken Vest	Oslofjordklinikken Sandnes
            '102224' = '974795515',	#Rana	Mo i Rana
            '103469' = '874716782',	#Rikshospitalet, nevrokir	Rikshospitalet
            '103240' = '874716782',	#Rikshospitalet, ort	Rikshospitalet
            '1491' = '974633191',	#Skien	Skien
            '105783' = '974749025',	#St.Olavs, nevrokir	St. Olavs
            '102467' = '974749025',	#St.Olavs, ort	St. Olavs
            '114288' = '974703300',	#Stavanger, nevrokir	Stavanger
            '105403' = '974703300',	#Stavanger, ort	Stavanger
            '601161' = '974795787',	#Tromsø	Tromsø
            '105153' = '974633574',	#Larvik	Tønsberg
            '109820' = '974589095',	#Ullevål, nevrokir	Ullevål
            '999995' = '974589095',	#Ullevål, ort	Ullevål
            '102484' = '974747545',	#Volda	Volda
            '110771' = '953164701',	#Volvat	Volvat
            '107981' = '974633655',	#Østfold	Askim
            '102483' = '974747138'	#Ålesund	Ålesund
  )
  RegDataUt$orgnr <- as.character(nyID[as.character(RegDataUt$ReshId)])
  RegDataUt <- RegDataUt[ ,c('year', 'orgnr', 'var', 'denominator', 'ind_id')]
    }
if (lastNedFil==1) {
  write.table(RegDataUt, file = filUt, sep = ';', row.names = F)} #, fileEncoding = 'UTF-8')}
  return(invisible(RegDataUt))
}



#' Beregn andel registrereinger som er ferdigstilt viss lang tid etter operasjon.
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

#Hjelpefunksjoner.

#' Kjør Shiny Application
#'
#' @param browser App åpner i browser
#' @param logAsJson Logg i json-format
#'
#' @return Et objekt som representerer Rygg-app'en
#' @export
kjorRyggApp <- function(browser = FALSE, logAsJson = FALSE) {

  if (logAsJson) {
    rapbase::loggerSetup()
  }
  app <- shiny::shinyApp(
    ui = rygg::ui_rygg,
    server = rygg::server_rygg,
    options = list(launch.browser = browser)
  )

  return(app)
}


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
#' @return registreringsforsinkelse
#' @export
forsinketReg <- function(RegData, fraDato, tilDato, forsinkelse, reshID=0){
  RegData$Diff <- as.numeric(difftime(as.Date(RegData$MedForstLukket),
                                      RegData$OpDato ,units = 'days')) #UtskrivelseDato
  Data <- RegData[ , c('OpDato', 'MndAar', 'Diff', 'ReshId')]%>%
    dplyr::filter(OpDato > fraDato & (OpDato < tilDato))

  if (reshID != 0) {Data <- dplyr::filter(Data, ReshId == reshID)}
  paste0(sum(as.numeric(Data$Diff)>forsinkelse, na.rm = T), ' (',
         round(100*sum(as.numeric(Data$Diff)>forsinkelse, na.rm = T)/dim(Data)[1],1), '%)')
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

  #raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Rygg',
  #                  reshId = reshID[[1]], msg = "Abonnement: månedsrapport")

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(reshID), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='rygg'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile)

  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  #raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Rygg',
  #                  reshId = reshID[[1]], msg = paste("Sendt: ", utfil))
  return(utfil)
}



#' Identifisere reoperasjoner
#' Legger på operasjonsnummer(OpNr), tid til neste operasjon (DagerNesteOp)
#' og identifiserer reoperasjoner innen 90 dager (Reop90dEtterOp)
#'
#' @param RegData
#'
#' @return Legger på operasjonsnummer, tid til neste og identifiserer reoperasjoner
#' @export

finnReoperasjoner <- function(RegData){

antPas <- length(names(table(RegData$PasientID)))
RegDataSort <-RegData[order(RegData$PasientID, RegData$OpDato), ]

N <- dim(RegData)[1]
RegDataSort$OpNr <- ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=seq_along)
indPasFlereOp <- which(RegDataSort$OpNr>1)
RegDataSort$DagerNesteOp <- NA
RegDataSort$DagerNesteOp[indPasFlereOp-1] <-
  difftime(as.POSIXlt(RegDataSort$OpDato[indPasFlereOp], tz= 'UTC', format="%Y-%m-%d"),
           as.POSIXlt(RegDataSort$OpDato[indPasFlereOp-1], tz= 'UTC', format="%Y-%m-%d"),
           units = 'days')
RegDataSort$Reop90dEtterOp <- 0
indReop <- which(RegDataSort$DagerNesteOp<90 | RegDataSort$NyRyggOpr3mnd==1 | RegDataSort$Reop90d==1)
RegDataSort$Reop90dEtterOp[indReop] <- 1

#Tar bort den gamle Reop90d-variabelen for å unngå misforståelse
#RegDataSort <- RegDataSort[ ,-which(names(RegDataSort) == 'Reop90d')]
#Ikke med lenger når henter data.

# RegDataSort <-
#   RegDataSort %>%
#   dplyr::mutate(Reop2 =
#            ifelse(RegDataSort$DagerNesteOp<90 | RegDataSort$NyRyggOpr3mnd==1 | RegDataSort$Reop90d,
#                   1,0))

# table(RegDataSort$Reop2)
return(invisible(RegDataSort))
}

#' Generere data til SKDEs interaktive nettsider
#' ODI er besvart ett år etter operasjon og resultatet vises for BESVARELSESÅR (Tenk over om skal filtrere på skjemadato eller ett år etter operasjonsdato.
#' Det siste er kanskje best basert på konsistens og mulighet for sammenligning med resultater som har utvalg på operasjonsdato.)
#' @param filUt tilnavn for utdatatabell. Hvis ikke angitt, lastes ikke fil ned
#' @param valgtVar - beinsmLavPre, peropKompDura, sympVarighUtstr, p.t. 10 kvalitetsind.
#' @param indID indikator-id, eks. 'ind1', 'ind2', osv.
#' @param slaaSmToAar 0:nei (standard), 1:ja. Slår sammen resultater for to og to år, glidende. Dvs. 2021 viser resultat fra alle operasjoner i
#' 2020 og 2021, mens 2020 viser for 2020 og 2019. De fleste indikatorer vises for to år (slaaSmToAar=1). I praksis dupliseres data for hvert år.
#' @inheritParams RyggUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilOffVisning <- function(RegData = RegData, valgtVar, aar=0, ktr=0,
                              indID = 'indDummy', slaaSmToAar=0,
                              hovedkat=99, hastegrad=99, tidlOp='', filUt='dummy'){

if (ktr==2) {
  #For 12-mnd.ktr. vil vi benytte det året pasientetn SVARTE
  #RegData$Aar <- as.numeric(substr(RegData$Utfdato12mnd, 1, 4))
  #NB: Utfdato finnes bare i V3. Mye feilregistrering/ekstremverdier i Utfdato12mnd
  RegData$Aar <-RegData$Aar+1
  }

  RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype = 'andelGrVar')
  RegData <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, aar=aar, hastegrad = hastegrad,
                           tidlOp=tidlOp, hovedkat=hovedkat)$RegData

  RegDataUt <- RegData[,c('Aar', "ReshId", "Variabel")]

  aarMed <- sort(unique(RegDataUt$Aar))
  antAar <- length(aarMed)
  if (slaaSmToAar==1 & antAar>1) { #duplisering av data
    RegDataDupl <- RegDataUt[RegDataUt$Aar %in% aarMed[1:(antAar-1)], ]
    RegDataDupl$Aar <- RegDataDupl$Aar+1
    #table(RegDataDupl$Aar)
    #table(RegDataUt$Aar)
    RegDataUt <- rbind(RegDataUt[-which(RegDataUt$Aar == aarMed[1]), ], RegDataDupl)
    #table(RegDataUt$Aar)
  }

  #Variabler: year, orgnr, var, denominator, ind_id
  RegDataUt$ind_id <- indID
  RegDataUt$denominator <- 1
  # nytt navn = gammelt navn
  RegDataUt <- dplyr::rename(RegDataUt,
                             year = Aar,
                             var = Variabel)

  #Legge på orgID ("Sykehusviser") 943545634
  #ReshId	orgnr	RapporteketNavn	SKDEnavn
  nyID <- c('999976' = '974706490',	#Ahus	Ahus
            '4211883' = '943545634', #Aleris Bodø
            '107508' = '943545634',	#Aleris Bergen	Aleris Bergen
            '107240' = '943545634',	#Aleris Drammen	Aleris Drammen
            '4211881' = '943545634', #Aleris Drammen
            '4211880' = '943545634', #Aleris Helse AS / Aleris Nesttun (ny 2023)
            '107511' = '943545634',  #Aleris Oslo
            '999975' = '943545634',	#Aleris Oslo	Aleris Colosseum Nobel
            '999994' = '943545634',	#Aleris Stavanger	Aleris Colosseum Stavanger
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
            '4217275' = '924547715', #Majorstuen Spesialistsenter AS
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


  #---Jevnlig sjekk av om vi har nye resh:
  # nye <- setdiff(unique(RegData$ReshId), names(nyID)) #length(unique(RegData$ReshId))
  # RegData$ShNavn[match(nye, RegData$ReshId)]
  # table(RegData[RegData$ReshId %in% nye, c('ShNavn', 'Aar')])

  RegDataUt$orgnr <- as.character(nyID[as.character(RegDataUt$ReshId)])
  RegDataUt <- RegDataUt[ ,c('year', 'orgnr', 'var', 'denominator', 'ind_id')]
  RegDataUt$context <- 'caregiver'

  if (filUt != 'dummy') {
    filUt <- paste0('Rygg_', filUt, '.csv')
    write.table(RegDataUt, file = filUt, sep = ';', row.names = F)} #, fileEncoding = 'UTF-8')}
  return(invisible(RegDataUt))
}


#' Lage entydig PID
#' Lager entydig PID basert på personnummer. Alle PID i V2 har ending suffiks V2. Pasienter i V3 får PID fra V2 hvis personnummeret
#' deres finnes i begge versjoner. NB: For at dette skal bli riktig er vi avhengig av kilde med oppdatert koblingsfil (PID-personnummer)
#' @param data tilnavn for utdatatabell. Hvis ikke angitt, lastes ikke fil ned
#' @param datoFra startdato for filtrering
#' @param datoTil sluttdato for filtrering
#' @param reshID hvilken resh det evt. skal filtreres på
#' @return Datafil med entydige PID
#' @export
#'
tilretteleggDataDumper <- function(RegData, datoFra='2000-01-01', datoTil=Sys.Date(), reshID=0, ...){

  #Koble på KryptertFnr fra forlopsoversikt via ForlopsID
  PIDtab <- rapbase::loadRegData(registryName="data", query='SELECT * FROM koblingstabell')
  RegData <- merge(RegData, PIDtab, by.x = 'PasientID', by.y = 'ID', all.x = T)

  #Legg til ledende 0 i V2
  indUten0 <- which(nchar(RegData$Personnummer)==10)
  if (length(indUten0)>0) {
    RegData$Personnummer[indUten0] <- paste0(0,RegData$Personnummer[indUten0])}

  #Entydig PID SSN-var fra V3/koblingstab, Personnummer-var fra V
  tidlPas <- match(RegData$SSN, RegData$Personnummer, nomatch = 0, incomparables = NA) #match(RegData$KryptertFnr, RegData$Personnummer, nomatch = 0, incomparables = NA)
  hvilkePas <- which(tidlPas>0)
  RegData$PID[hvilkePas] <- RegData$PID[tidlPas[hvilkePas]]

  #SSN i en variabel
  fraV3 <- which(is.na(RegData$Personnummer))
  RegData$Personnummer[fraV3] <- RegData$KryptertFnr[fraV3]

  RegData <- dplyr::filter(RegData,
                        as.Date(InnDato) >= datoFra, # datovalg[1],
                        as.Date(InnDato) <= datoTil) #datovalg[2])
  if (reshID != 0) {
    RegData <- RegData[which(as.numeric(RegData$ReshId) %in% as.numeric(reshID)), ]}


  return(RegData)
}


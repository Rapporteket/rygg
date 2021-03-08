#Generere filer og tall til årsrapport for 2020.
library(rygg)
library(xtable)

#Felles parametre:
startAar <- 2011
rappAar <- 2020
datoFra1aar <- paste0(rappAar,'-01-01')
datoFra2aar <- paste0(rappAar-1,'-01-01')
datoFra3aar <- paste0(rappAar-2,'-01-01')
datoTil12mnd <- paste0(rappAar-1,'-12-31')
datoFra <- as.Date(paste0(startAar,'-01-01'))
datoTil <- as.Date(paste0(rappAar,'-12-31'))

aar2 <- (rappAar-1):rappAar  #2015:2016
aar2_12mnd <- aar2-1
tidlAar <- rappAar-1
tidlAar2 <- (rappAar-3):(rappAar-2) #2013:2014
Ngrense <- 20
AKjust <- 0
ktr <- 2


RyggData <- RyggRegDataSQLV2V3()
RegData <- RyggPreprosess(RegData=RyggData)
Ntot07 <- dim(RegData)[1]

#Gjør utvalg/tilrettelegge årsfiler
RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)$RegData #RegData[which(RegData$InnDato>= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil)), ] #
RegData1aar <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
#RegData12mnd <- RegData[which(RegData$Aar < rappAar), ] #For å ikke få med de som har fått 12mnd-skjema i inneværende år.

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]
AntAvd <- length(unique(RegData$ShNavn))
setwd('/home/rstudio/rygg/Aarsrapp')

#RegData <- read.table('A:/Rygg/RyggV2V3_2020-09-22.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE,)

#MANGLER: Alle stabelfigurer. Eks. RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')
#NB: Sjekk om stabelfigurene er i bruk


#---------FIGURER, årsrapport 2020--------------

# Dekningsgrad for hvert sykehus, Se tidligere figurer. IKKE FÅTT DEKN.GRADSANALYSE FOR 2020 (1.mar 2021)
RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Rygg', outfile='DGrygg.pdf')
RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Nakke', outfile='DGnakke.pdf') #


RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
               hastegrad=1, outfile='VentetidBestOp_Sh.pdf')

#NY2021: Ventetid fra operasjon bestemt til opr.tidpk

RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidHenvTimePol', Ngrense = 20,
                    hastegrad=1, outfile='VentetidHenvTimePol_Sh.pdf')

RyggFigAndeler(RegData = RegData1aar, valgtVar='antibiotikaMedikament',
               preprosess = 0, outfile = 'AntibiotikaMedikament.pdf')


# Vent: PeropKompDuraTidSmlGr.pdf #linjediagram per år, ulike operasjonstyper og totalt


RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, hastegrad=1, ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarPro.pdf')
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarFusj.pdf')
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarSS.pdf')
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=10, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarDS.pdf')

RyggFigAndelerGrVar(RegData=RegData, valgtVar='Osw22', ktr=2,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                    hovedkat=1,  hastegrad=1, tidlOp=4,  outfile='Osw22Pro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='Osw22', ktr=2,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                    hovedkat=9,  hastegrad=1, tidlOp=4,  outfile='Osw22SS.pdf')

RyggFigAndelerGrVar(RegData=RegData, valgtVar='fornoydhet', ktr=ktr,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                    hovedkat=1,  hastegrad=1, tidlOp=4,  outfile='FornoydAvdPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='fornoydhet', ktr=ktr,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                    hovedkat=9,  hastegrad=1, tidlOp=4,  outfile='FornoydAvdSS.pdf')

RyggFigAndelerGrVar(RegData = RegData1aar, preprosess = 0, valgtVar='morsmal', outfile = 'Morsmal.pdf')

HoyUtdAvd <- RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='utd', Ngrense = 10,
                                 outfile='HoyUtdAvd.pdf')

UforetrygdPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='uforetrygdPre', datoFra=datoFra1aar,
                                     outfile='UforAvd.pdf')

RyggFigAndelerGrVar(RegData = RegData, valgtVar = 'degSponFusj', aar = (rappAar-4):rappAar,
                    outfile = 'DegSponFusj.pdf')
DegSponFusjSStid <- RyggFigAndelTid(RegData=RegData, valgtVar = 'degSponFusj', hovedkat=9,
                                    outfile = 'DegSponFusjSStid.pdf')


RyggFigGjsnBox(RegData=RegData, aar=startAar:(rappAar-1) ,tidsenhet = 'Aar', outfile='OswEndrTidDS.pdf',
               valgtVar='OswEndr', hovedkat=10, ktr=ktr)

Alder70Aar <- RyggFigAndelTid(RegData=RegData, datoFra = datoFra, valgtVar='alder70', preprosess = 0,
                              outfile='Alder70.pdf')

RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdPro.pdf',
                 valgtVar='liggedogn', hovedkat = 1, valgtMaal = 'Gjsn')
RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdSS.pdf',
                 valgtVar='liggedogn', hovedkat=9, valgtMaal = 'Gjsn')
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='trombProfyl', outfile='TrombProfyl.pdf')


RyggFigGjsnGrVar(RegData=RegData1aar, valgtVar='liggetidPostOp', outfile='LiggetidPostOpGjsn.pdf')

#------ KVALITETSINDIKATORER------------

dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=rappAar, tidlAar=tidlAar, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=aar2, outfile='SympVarighUtstrShPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, preprosess = 0,
                    Ngrense=20, aar=aar2, outfile='SympVarighUtstrShSS.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, outfile='SympVarighUtstrTidPro.pdf')


BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='smBePreLav', aar=aar2,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPrePro.pdf')
BeinsmLavPre <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='smBePreLav', aar=rappAar, tidlAar=tidlAar,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPreProAar.pdf')

#Infeksjoner ikke registrert i 2019
#3 kval.ind: Prolaps, Fusjon, SS
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                    Ngrense = 20, hovedkat = 1, outfile='KpInf3mndPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                    Ngrense = 20, hovedkat = 5, outfile='KpInf3mndFusj.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                   Ngrense = 20, hovedkat=9, outfile='KpInf3mndSS.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 20, hovedkat = 1, outfile='KpInf3mndProAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 20, hovedkat = 5, outfile='KpInf3mndFusjAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 20, hovedkat=9, outfile='KpInf3mndSSAar.pdf')

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarPro.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarFusj.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarSS.pdf')

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidDS.pdf', tidsenhet = 'Aar',
               valgtVar='OswEndr', hovedkat=10, ktr=ktr)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr20',  outfile='OswEndr20Pro.pdf',
                    aar=aar2_12mnd, hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSS.pdf', #OswEndr30pstSS.pdf
                    aar=aar2_12mnd, hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)


# Andel skjema som er registrert innen 12 uker etter at pasienten er uskrevet, registreringsforsinkelse per sykehus.
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='regForsinkelse', preprosess = 0,
                    outfile='RegForsinkelse_Sh.pdf') #RegForsinkelse_Sh.pdf

#------------------------------------------------------------------------------------
#-----------Filer til Resultatportalen -----------------------
#------------------------------------------------------------------------------------
rm(list=ls())
#NKRdata1 <- read.table('A:/Rygg/Versjon2/NKR2019-01-16.csv', sep=';', header=T) #, encoding = 'UTF-8')
#NKRdata <- read.table('A:/Rygg/Versjon2/NKR2019-09.csv', sep=';', header=T) #, encoding = 'UTF-8')
#RyggData <- RyggPreprosess(RegData=NKRdata)
#datoFra = '2011-01-01'

library(rygg)
library(magrittr)
RyggData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())

rappAar <- 2020
valgteAar <- 2011:rappAar


DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='sympVarighUtstr', aar=valgteAar,
                                 hovedkat=1,
                                 ResPort=0, indID = 'nkr_rygg_varighet_bensmerter', filUt = 'ind1_Varighet_bensmerter')

#--Bensmerter mindre eller lik 3 på numerisk smerteskala
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='smBePreLav', aar=valgteAar, hovedkat=1,
                                ResPort=0, indID = 'nkr_rygg_lav_bensmerte_prolaps', filUt = 'ind2_lav_bensmerte_prolaps')

#--Sårinfeksjon, dyp og overfladisk
DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar, hovedkat=1,
                             ResPort=0, indID = 'nkr_rygg_saarinfeksjon_prolaps', filUt = 'ind3_Saarinfeksjon_prolaps')

DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar, hovedkat=9,
                             ResPort=0, indID = 'nkr_rygg_saarinfeksjon_stenose', filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=1, tidlOp=4, hastegrad=1,
                                ResPort=0, indID = 'nkr_rygg_durarift_prolaps', filUt = 'ind5_Durarift prolaps')
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=9, tidlOp=4, hastegrad=1,
                                ResPort=0, indID = 'nkr_rygg_durarift_stenose', filUt = 'ind6_Durarift_stenose')

# IKKE? valgtVar='peropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,

#Ventetid, operasjon bestemt til utført
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='ventetidSpesOp', aar=valgteAar,
                                hovedkat=9, tidlOp=4, hastegrad=1,
                                ResPort=0, indID = 'nkr_rygg_ventetid_kirurgi', filUt = 'ind8_VentetidOperasjon')

#-------Oswestry------
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr20', aar=valgteAar,
                                hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2,
                                ResPort=0, indID = 'nkr_rygg_ODI20p12mnd_prolaps', filUt = 'ind9_OswEndr20poengPro')
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr30pst', aar=valgteAar,
                                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2,
                                ResPort=0, indID = 'nkr_rygg_ODI30pst12mnd_stenose', filUt = 'ind10_OswEndr30pstPro')


#Alle sykehus og resh:
ShResh <- unique(RyggData[c('ReshId', 'ShNavn')])
write.table(ShResh, file = 'RyggShResh.csv', sep = ';', row.names = F)


#---Nøkkelinformasjon, Resultatportalen------
#---- R Y G G

RyggData <- RyggPreprosess(
  RegData=RyggRegDataSQLV2V3())
RyggData <- RyggUtvalgEnh(RegData = RyggData, datoFra = '2019-01-01', datoTil = '2019-12-31')$RegData
Dum <- RyggUtvalgEnh(RegData = RyggData, aar=2019)$RegData

FornoydData <- RyggVarTilrettelegg(RegData = RyggData,
                                   valgtVar = 'fornoydhet', ktr = 1, figurtype = 'andelGrVar')$RegData
BedreData <- RyggVarTilrettelegg(RegData = RyggData,
                                   valgtVar = 'nytte', ktr = 1, figurtype = 'andelGrVar')$RegData
VerreData <- RyggVarTilrettelegg(RegData = RyggData,
                                 valgtVar = 'verre', ktr = 1, figurtype = 'andelGrVar')$RegData
VentetidKirData <- RyggVarTilrettelegg(RegData = RyggData,
                                 valgtVar = 'ventetidSpesOp', ktr = 1, figurtype = 'andelGrVar')$RegData

NokkeltallRygg <- rbind(
  'Antall avdelinger' = length(unique((RyggData$ShNavn))),
  'Antall operasjoner' = dim(RyggData)[1],
  'Svart på oppfølging, 3 mnd.' = mean(RyggData$Ferdigstilt1b3mnd==1, na.rm=T),
  'Andel over 70 år'	= mean(RyggData$Alder>=70, na.rm=T),
  'Gjennomsnittsalder' = mean(RyggData$Alder, na.rm=T),
  'Andel kvinner' = 1-mean(RyggData$ErMann, na.rm=T),
  'Fornøyd med behandlingen, 3 mnd. etter' = mean(FornoydData$Variabel),
  'Helt restituert/mye bedre, 3 mnd. etter' = mean(BedreData$Variabel),
  'Verre 3 mnd. etter' = mean(VerreData$Variabel),
  'Ventet <3 mnd fra operasjon bestemt til kirurgi utført' = mean(VentetidKirData$Variabel)
)

tabNokkeltallRygg <- cbind(row.names(NokkeltallRygg),NokkeltallRygg)
write.table(tabNokkeltallRygg, file = 'NokkeltallRygg.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )



# 2011:2018-tall
antSh <- colSums(table(as.character(RyggData$ShNavn),RyggData$Aar)>0)
antOp <- table(RyggData$Aar)

#Andel som svarer på oppfølging 3 og 12 mnd.
ind <- RyggData$Ferdigstilt1b3mnd %in% 0:1
#andelSvart3mnd <- tapply(RyggData$Ferdigstilt1b3mnd,RyggData$Aar, FUN='mean', na.rm=T)
#andelSvart12mnd <- tapply(RyggData$Ferdigstilt1b12mnd,RyggData$Aar, FUN='mean', na.rm=T)
andelSvart3mnd <- tapply(RyggData$Ferdigstilt1b3mnd==1, RyggData$Aar, FUN='mean', na.rm=T)
andelSvart12mnd <- tapply(RyggData$Ferdigstilt1b12mnd==1, RyggData$Aar, FUN='mean', na.rm=T)

RyggData$over70 <- 0
RyggData$over70[RyggData$Alder>=70] <- 1
andel70aar <- tapply(RyggData$over70,RyggData$Aar, FUN='mean', na.rm=T)
alderGjsn <- tapply(RyggData$Alder,RyggData$Aar, FUN='mean', na.rm=T)
alderMedian <- tapply(RyggData$Alder,RyggData$Aar, FUN='median', na.rm=T)
andelKvinner <- 1-tapply(RyggData$ErMann,RyggData$Aar, FUN='mean', na.rm=T)

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
andelForn <- function(RyggData, ktr=1){
  RyggData$Variabel <- 0
  RyggData$Utfylt <- switch(ktr, RyggData$Utfylt3mnd, RyggData$Utfylt12mnd)
  RyggData$Fornoyd <- switch(ktr, RyggData$Fornoyd3mnd, RyggData$Fornoyd12mnd)
  ind <- intersect(which(RyggData$Utfylt==1), which(RyggData$Fornoyd %in% 1:5))
  DataDum <- RyggData[ind, c('Fornoyd', 'Variabel', 'Aar')]
  DataDum$Variabel[DataDum$Fornoyd %in% 1:2] <- 1
  andelFornoyd <- tapply(DataDum$Variabel, DataDum$Aar, FUN='mean', na.rm=T)
}
andelFornoyd3mnd <- andelForn(RyggData, ktr=1)
andelFornoyd12mnd <- andelForn(RyggData, ktr=2)


#Betydelig endring
andelEndring <- function(Data, ktr=2){
  Data$Bedre <- 0
  Data$Verre <- 0
  Data$Utfylt <- switch(ktr, '1' = Data$Utfylt3mnd, '2'=Data$Utfylt12mnd)
  Data$NytteOpr <- switch(ktr, Data$Nytte3mnd, Data$Nytte12mnd)
  DataEndring <-  Data[intersect(which(Data$Utfylt==1),
                                 which(Data$NytteOpr %in% 1:7)),
                       c('NytteOpr', 'Bedre', 'Verre','Aar')]
  DataEndring$Bedre[DataEndring$NytteOpr %in% 1:2] <- 1
  DataEndring$Verre[DataEndring$NytteOpr %in% 6:7] <- 1

  andelSuksess <- tapply(DataEndring$Bedre, DataEndring$Aar, FUN='mean', na.rm=T)
  andelVerre <- tapply(DataEndring$Verre, DataEndring$Aar, FUN='mean', na.rm=T)
  return(list('Suksess'= andelSuksess,  'Verre' = andelVerre))
}
endring <- andelEndring(RyggData, ktr=1)
andelSuksess3mnd <- andelEndring(RyggData, ktr=1)$Suksess
#andelSuksess12mnd <- andelEndring(RyggData, ktr=2)$Suksess
andelVerre3mnd <- andelEndring(RyggData, ktr=1)$Verre
#andelVerre12mnd <- andelEndring(RyggData, ktr=2)$Verre
RegData <- RegData[which(RegData$VentetidSpesialistTilOpr %in% 1:4),]
RegData$Variabel[which(RegData$VentetidSpesialistTilOpr == 1)] <- 1

andelVentetUnder3mnd <- RegData[which(RegData$VentetidSpesialistTilOpr %in% 1:4),]
RegData$Variabel[which(RegData$VentetidSpesialistTilOpr == 1)] <- 1

NokkeltallRygg <- rbind(
  'Antall avdelinger' = antSh,
  'Antall operasjoner' = antOp,
  'Svart på oppfølging, 3 mnd.' = andelSvart3mnd,
#  'Svart på oppfølging, 12 mnd.' = andelSvart12mnd,
  'Andel over 70 år'	= andel70aar,
  'Gjennomsnittsalder' = alderGjsn,
  #   'Medianalder' = alderMedian,
  'Andel kvinner' = andelKvinner,
  'Fornøyd med behandlingen, 3 mnd. etter' = andelFornoyd3mnd,
#  'Fornøyd med behandlingen, 12 mnd. etter' = andelFornoyd12mnd,
  'Helt restituert/mye bedre, 3 mnd. etter' = andelSuksess3mnd,
#  'Helt restituert/mye bedre, 12 mnd. etter' = andelSuksess12mnd,
  'Verre 3 mnd. etter' = andelVerre3mnd,
#  'Verre 12 mnd. etter' = andelVerre12mnd
)
tabNokkeltallRygg <- cbind(row.names(NokkeltallRygg),NokkeltallRygg)
tabNokkeltallRygg[,c('2017','2018', '2019')]

write.table(tabNokkeltallRygg, file = 'NokkeltallRygg.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )

RyggFigAndeler(RyggData, valgtVar = 'nytte', ktr=2, aar = 2017, outfile = 'NytteFord.png')







#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#---------Figurer, utdata til videre beregninger
UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='utd', aar = startAar:rappAar, preprosess = 0,
                                outfile='FigUtdAar.pdf')
UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='uforetrygdPre', preprosess = 0, outfile='FigUforTid.pdf')
ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='erstatningPre', preprosess = 0, outfile='FigErstatTid.pdf')

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', outfile='FigRokerTid.pdf')
Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='utd', datoFra=datoFra1aar, datoTil=datoTil,
                            outfile='FigUtd.pdf')


#AndelTidlOp <- RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')


HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='hovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='HovedInngrep.pdf')


TidlOp3 <- RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'FigTidlOpAnt3.pdf')

KpInf3mndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='kpInf3mnd',
                                   hovedkat = 1, outfile='FigKpInf3mndTidPro.pdf')
KpInf3mndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='kpInf3mnd',
                                   hovedkat=9, outfile='FigKpInf3mndTidSS.pdf')

DuraPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura',
                               aar=(rappAar-1):rappAar, Ngrense = 20,
                               hastegrad = 1, tidlOp = 4, hovedkat = 1, outfile='FigDuraPro.pdf')
DuraSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura',
                              aar=(rappAar-1):rappAar, Ngrense = 20,
                              hastegrad = 1, tidlOp = 4, hovedkat=9, outfile='FigDuraSS.pdf')
# sjekket
FremmedSpraakAar <-  RyggFigAndelTid(RegData=RegData, valgtVar='morsmal', aar = startAar:rappAar,
                                     outfile='FigMorsmalAar.pdf', preprosess = 0)






#--------------TABELLER OG TALL---------------------------
RegDataPro <- RegData[which(RegData$HovedInngrep==1),]
RegDataPro12mnd <- RegDataPro[which(RegDataPro$Aar<rappAar), ]
RegDataSS <- RyggUtvalgEnh(RegData, hovedkat=9)$RegData
#(Sjekk om antall stemmer!)

RegData$ODIendr <- RegData$OswTotPre-RegData$OswTot12mnd
RegData$ODIpst <- with(RegData, (OswTotPre-OswTot12mnd)/OswTotPre*100)



tabAvdN <- addmargins(table(RegData[c('ShNavn','Aar')]))
antKol <- ncol(tabAvdN)
tabAvdN5 <- tabAvdN[,(antKol-5):antKol]
rownames(tabAvdN5)[dim(tabAvdN5)[1] ]<- 'TOTALT, alle avd.:'
colnames(tabAvdN5)[dim(tabAvdN5)[2] ]<- paste0(min(RegData$Aar),'-',rappAar)

xtable(tabAvdN5, digits=0, align=c('l', rep('r', 6)),
       caption=paste0('Antall registreringer ved hver avdeling siste 5 år, samt totalt siden ', min(RegData$Aar, na.rm=T),'.'),
       label = 'tab:AntReg')

#Gjennomsnittsalderen per år:
(AlderAar <- tapply(RegData$Alder, RegData$Aar, 'mean', na.rm=T))
AlderAar <- sprintf('%.1f', AlderAar)
Over 70 år i rappAar:  Andel70 per år pg \% (sum(RegData1aar$Alder>=70)
                                             #(Andel70 <- sprintf('%.0f',sum(RegData1aar$Alder>=70, na.rm=T)/sum(RegData1aar$Alder > -1, na.rm=T)*100))
                                             Alder70Aar$AggVerdier$Hoved

#Andelen pasienter med fedme:
  FedmeAar <- table(RegData$BMI>30, RegData$Aar)
  (AndelFedmeAar <- FedmeAar['TRUE',]/table(RegData$Aar)*100)

#Kjønnsfordeling, alle år, kvinner menn:
  (tabKjPst <- sprintf('%.1f',table(RegData$ErMann)/Ntot*100))

#Andelen fremmedspråklige (inkl. samisk) per år:
  FremmedSpraakAar$AggVerdier$Hoved
  #(FremmedSpraak <- sprintf('%.1f', FremmedSpraakAar$AggVerdier$Hoved))

#Andelen ryggopererte med høyere utdanning (høyskole eller universitet):
  UtdanningTid$AggVerdier$Hoved
  #UtdanningAar <- sprintf('%.1f', UtdanningTid$AggVerdier$Hoved)


#Andel i fullt arbeid når de blir ryggoperert, årsrapportåret:
ArbNum <- round(table(RegData1aar$ArbstatusPre)*100/sum(table(RegData1aar$ArbstatusPre)), 1)
ArbNum[1]

#Andel pasienter svart på spørsmål om arbeidsstatus, årsrapportåret: \%
  NsvarArb <- sum(RegData1aar$ArbstatusPre %in% 1:9)
  round(NsvarArb/Ntot1aar*100, 1)

Arb <- paste0(ArbNum, '%')
# names(Arb) <- c('I arbeid', 'Hjemmeværende', 'Student/skoleelev', 'Pensjonist', 'Arbeidsledig',
#                 'Sykemeldt', 'Aktiv sykemeldt', 'Delvis Sykemeldt', 'Attføring/rehabiliteirng', 'Uføretrygdet')
names(Arb) <- c("Fulltidsjobb","Deltidsjobb","Student/skoleelev",
           "Alderspensjonist", "Arbeidsledig","Sykemeldt","Delvis sykemeldt",
           "Arbeidsavklaringspenger", "Uførepensjonert","Ikke utfylt")

xtable(cbind('Andeler'=Arb),  align=c('l','r'),
       caption=paste0('Arbeidsstatus, pasienter operert i ', rappAar,'.'),
       label="tab:Arb")

#Mottok sykepenger (sykemeldte, uføretrygdede eller attføring):
sum(ArbNum[6:9])


#Har søkt eller planlegger å søke uføretrygd, rappAar:
UforTid$AggVerdier$Hoved
#UforAar <- sprintf('%.1f', UforTid$AggVerdier$Hoved)

#Har søkt eller planlegger å søke erstatning, rappAar:
ErstatningTid$AggVerdier$Hoved
#ErstatningAar <- sprintf('%.1f', ErstatningTid$AggVerdier$Hoved)


#Tabell, ASA
ASAant <- table(factor(RegData1aar$ASA, levels=1:5), useNA='a')
ASApst <- round(ASAant*100/Ntot1aar, 1)
ASA <- cbind('Antall' = ASAant,
             'Prosent' = paste0(ASApst, '%'))
rownames(ASA) <- c('I','II','III','IV', 'V', 'Ikke besvart')
xtable(ASA, caption=paste0('Fordeling av ASA-grad, operasjoner utført i ', rappAar),
       label="tab:ASA", align=c('c','r','r'))

#Andelen pasienter med ASA grad I-II:
round(sum(table(RegData1aar$ASA)[1:2])/Ntot1aar*100, 1)

#Andel røykere som ryggopereres, per år:
RoykTid$AggVerdier$Hoved

# #Tabell, pasienter som har vært til radiologiske undersøkelser.
# #MÅ EVT OPPDATERES. Borte: RvDiscogr', 'RvRadigr',
# #Sjekk om skal være med!
RVvar <- c('RvCt', 'RvMr', 'RvRtgLscol', 'RvFunksjo', 'RvDpregblok')
RVant <- colSums(RegData1aar[, RVvar], na.rm=T)
RVpst <- round(RVant*100/Ntot1aar,1)

RV <- cbind('Antall' = c(RVant, Ntot1aar),
            'Andeler' = c(paste0(RVpst, '%'),' ')
)
 rownames(RV) <- c('CT', 'MR', 'Røntgen LS-columna', 'Funskjonsopptak',
                   'Diagnostisk blokade', 'Tot. ant.')
xtable(RV, caption=paste0('Radiologisk vurdering, ',rappAar),
       label="tab:RV", align=c('l','r','r'))


#Tabell, diagnoser basert på radiologiske funn
#Sjekk inklusjon av variabler !!!!!!!! Borte: "RfNormal", "RfForamino", "RfDegen", "RfPseudom"
RFvar <- c( "RfSkive", "RfSentr", "RfLateral",
           "RfSpondtypeIsmisk", "RfSpondtypeDegen", "RfDegskol", "RfSynovpre")
RFant <- colSums(RegData1aar[, RFvar], na.rm=T)
RFpst <- round(RFant*100/Ntot1aar)
RF <- cbind('Antall' = c(RFant, Ntot1aar),
            'Andeler' = c(paste0(RFpst, '%'),' '))

# #Teller opp de som er diagnostisert som normale:
# RFNorm <- intersect(which(rowSums(RegData1aar[, RFvar[2:11]], na.rm=T) == 0),
#                     which(RegData1aar$RfNormal == 1))
# rownames(RF) <- c('Skiveprolaps', 'Sentral spinalstenose', 'Lateral spinalstenose',
#                   'Foraminal stenose', 'Degenerativ rygg/skivedegenerasjon',
#                   'Istmisk spondylolistese', 'Degenerativ spondylolistese',
#                   'Degenerativ skoliose', 'Synovial syste', 'Pseudomeningocele', 'Tot.ant.')
xtable(RF, caption=paste0('Radiologiske diagnoser, ', rappAar),
       label="tab:RF", align=c('l','r','r'))


#1: Samme nivå, 2:Annet nivå, 3: Annet og sm. nivå, 4: Primæroperasjon
  #NB: I figuren er 4 kodet om til 0 !!!
#Andelen reoperasjoner, per år:
AndelReop <- round(colSums(AndelTidlOp$AndelerHoved[2:4,]))

#Av de pasientene operert i \Sexpr{rappAar} som hadde vært operert tidligere:
AndelerSisteAar <- AndelTidlOp$AndelerHoved[,dim(AndelTidlOp$AndelerHoved)[2]]
AndelerTidlOp <- sprintf('%.1f', AndelerSisteAar[2:4]/sum(AndelerSisteAar[2:4])*100)
AndelerTidlOp[1] #\% operert i samme nivå,
AndelerTidlOp[2] #operert i annet nivå
AndelerTidlOp[3] #operert i både samme og annet nivå.

#Prolapspasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
AndelTidlOp3Pro <- round(table(RegDataPro$TidlOprAntall>2,RegDataPro$Aar)['TRUE',]/table(RegDataPro$Aar)*100,1)
min(AndelTidlOp3Pro)
max(AndelTidlOp3Pro)
#lumbal spinal stenosepasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
AndelTidlOp3SS <- round(table(RegDataSS$TidlOprAntall>2,RegDataSS$Aar)['TRUE',]/table(RegDataSS$Aar)*100,1)
c(min(AndelTidlOp3SS), max(AndelTidlOp3SS))


#Andelen operert for lumbalt prolaps ved hjelp av synsfremmende midler:
(MikroAarPro <- prop.table(table(RegDataPro$OpMikro, RegDataPro$Aar),2)*100)
BruktMikroAarPro <- sprintf('%.0f', MikroAarPro['1',])

#Andelen operert for lumbal spinal stenose ved hjelp av synsfremmende midler:
(MikroAarSS <- prop.table(table(RegDataSS$OpMikro, RegDataSS$Aar),2)*100)
BruktMikroAarSS <- sprintf('%.0f', MikroAarSS['1',])


# #Sårinfeksjon, KpInf3mnd, spinal stenose
# (SaarInfSS <- prop.table(table(RegDataSS$KpInf3mnd, RegDataSS$Aar),2)*100 )
#
# #FornoydPro <- sprintf('%.0f',)
# prop.table(table(RegDataPro12mnd$Fornoyd12mnd, RegDataPro12mnd$Aar),2)[1,]*100
# #FornoydSS <- sprintf('%.0f', )
# prop.table(table(RegDataSS$Fornoyd12mnd, RegDataSS$Aar),2)[1,]*100



#Hyppigste tilstandene pasienter ble operert for i rappAar} var
#Tabell, fordeling av hovedinngrepstype
  HovedInngrepTab <- cbind('Antall' = HovedInngrep$AggVerdier$Hoved,
                           'Andeler' = paste0(round(as.numeric(HovedInngrep$AggVerdier$Hoved)),'%')
  )
rownames(HovedInngrepTab) <- HovedInngrep$grtxt
xtable(HovedInngrepTab, align=c('l','r','r'), caption=paste0('Fordeling av hovedinngrep, ', rappAar), label="tab:HovedInngrep", digits=1)




#Andelen operert med dagkirurgi for hhv prolaps og spinal stenose
ProDagTid <- table(RegDataPro[ ,c('Dagkirurgi', 'Aar')], useNA = 'a')
ProDagPst <- prop.table(ProDagTid[1:2,],2)*100
ProDag11_naa <- c(round(ProDagPst['1','2011']), round(ProDagPst['1',as.character(rappAar)]))

SSDagTid <- table(RegDataSS[ ,c('Dagkirurgi', 'Aar')], useNA = 'a')
SSDagPst <- prop.table(SSDagTid[1:2,],2)*100
SSDag11_naa <- c(round(SSDagPst['1','2011']), round(SSDagPst['1',as.character(rappAar)]))

#Andel operert for spinal stenose som også hadde Degenerativ spondylolistese, rappAar
AntDegenSpondSS <-  dim(RyggUtvalgEnh(RegDataSS, hovedkat = 9, aar = rappAar)$RegData)[1]
AntSS <- sum(RegDataSS$Aar==rappAar)
AndelDegSponSS <- round(AntDegenSpondSS/AntSS*100,1)



#andelen som får tilleggsbehandling med fusjonskirurgi:

#MANGLER:

#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, før operasjon:
indProPP <-  with(RegDataPro, which((Aar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIprePro <- sprintf('%.1f', mean(RegDataPro$OswTotPre[indProPP]))
#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, etter operasjon:
ODIpostPro <- sprintf('%.1f', mean(RegDataPro$OswTot12mnd[indProPP]))

#ODI-pre, lumbal spinal stenose:
indSSPP <-  with(RegDataSS, which((Aar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIpreSS <- sprintf('%.1f', mean(RegDataSS$OswTotPre[indSSPP]))
#ODI-post, lumbal spinal stenose:
ODIpostSS <- sprintf('%.1f', mean(RegDataSS$OswTot12mnd[indSSPP]))

#ODI-pre, fusjonkirurgi:
indFusjPP <-  with(RegData,
                   which(HovedInngrep==5 & (Aar==(rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIpreFusj <- sprintf('%.1f', mean(RegData$OswTotPre[indFusjPP]))
#ODI-pre, fusjonkirurgi:
ODIpostFusj <- sprintf('%.1f', mean(RegData$OswTot12mnd[indFusjPP]))



#Andel fornøyde, lumbalt prolaps, ett år etter:
FornoydProTid
#Andel fornøyde, lumbal spinal stenose, ett år etter:
FornoydSSTid

#Liggetid, prolaps:
LiggetidPro <- tapply(RegDataPro$Liggedogn[indPro], RegDataPro$Aar[indPro], mean, na.rm=T)
NedgLiggetidPro <- sprintf('%.1f', abs(LiggetidPro[as.character(rappAar)]-LiggetidPro['2010']))




  ODIProOpKat <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$OpKat),2)*100,1)
ODIProTidlOpAnt3 <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$TidlOprAntall>2),2)*100,1)

AndelOhjSS <- round(prop.table(table(RegData$OpKat[indSS]))*100,1)
ODISSTidlOpAnt3 <- round(prop.table(table((RegData$ODIpst)[indSS]>=30,
                                          RegData$TidlOprAntall[indSS]>2),2)*100,1)

hastegrad <- 1  #Bare elektive pasienter
tidlOp <- 4 #Bare primæroperasjoner



#suksessraten, lumbalt prolaps, ikke tidl. operert:
ODIProTidlOp <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$TidlOpr==4),2)*100,1)
ODIProTidlOp['TRUE','TRUE']

#Tabell, symptomvarighet
UtsRHnum <- round(table(RegData1aar$SympVarighUtstr, useNA='a')*100/Ntot1aar, 1)
UtsRH <- paste(UtsRHnum, '%', sep='')
names(UtsRH) <- c('Ingen utstrålende smerter', '< 3 mnd',
                  '3 - 12 mnd', '1 - 2 år', '> 2 år', 'Ikke besvart')	#, 'Tot. ant.')
xtable(cbind('Andeler'=UtsRH), caption=paste0('Varighet av nåværende utstrålende smerter, pasienter operert i ',
                                              rappAar),
       label="tab:Utstr", align=c('l','r'), digits=1)




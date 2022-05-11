#Generere filer og tall til årsrapport for 2021.
library(rygg)
library(xtable)

#Felles parametre:
startAar <- 2011
rappAar <- 2021
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

setwd('/home/rstudio/speil/aarsrapp/')
RyggData <- RyggRegDataSQLV2V3(alleVarV3 = 0)
RegData <- RyggPreprosess(RegData=RyggData)
Ntot07 <- dim(RegData)[1]

#Gjør utvalg/tilrettelegge årsfiler
RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)$RegData #RegData[which(RegData$InnDato>= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil)), ] #
RegData1aar <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
#RegData12mnd <- RegData[which(RegData$Aar < rappAar), ] #For å ikke få med de som har fått 12mnd-skjema i inneværende år.
#write.table(RegData, file = 'RyggAarsrapp2021.csv', sep = ';', row.names = F, fileEncoding = 'latin1', na = '')

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]
AntAvd <- length(unique(RegData$ShNavn))

#MANGLER: Alle stabelfigurer. Eks. RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')
#NB: Sjekk om stabelfigurene er i bruk

#---------FIGURER, årsrapport 2020--------------

# Dekningsgrad for hvert sykehus, Se tidligere figurer. IKKE FÅTT DEKN.GRADSANALYSE FOR 2020 (1.mar 2021)
#RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Rygg', outfile='DGrygg.pdf')
#RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Nakke', outfile='DGnakke.pdf') #

#NY2021: Ventetid fra operasjon bestemt til opr.tidpk
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
               hastegrad=1, outfile='VentetidBestOp_Sh.pdf')

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

RyggFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar='fornoydhet', ktr=ktr,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20
                    , hovedkat=1,  hastegrad=1, tidlOp=4,  outfile='FornoydAvdPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar='fornoydhet', ktr=ktr,
                    aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                    hovedkat=9,  hastegrad=1, tidlOp=4,  outfile='FornoydAvdSS.pdf')

RyggFigAndelerGrVar(RegData = RegData1aar, preprosess = 0, valgtVar='morsmal',
                    outfile = 'Morsmal.pdf')

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

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=rappAar, tidlAar=tidlAar, hastegrad=1, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=aar2, outfile='SympVarighUtstrShPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, preprosess = 0,
                    Ngrense=20, aar=aar2, outfile='SympVarighUtstrShFusj.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=9, preprosess = 0,
                    Ngrense=20, aar=aar2, outfile='SympVarighUtstrShSS.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, hastegrad=1, outfile='SympVarighUtstrTidPro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, outfile='SympVarighUtstrTidFusj.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=9, outfile='SympVarighUtstrTidSS.pdf')


BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='smBePreLav', aar=aar2,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPrePro.pdf')
BeinsmLavPre <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='smBePreLav', aar=rappAar, tidlAar=tidlAar,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPreProAar.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='smBePreLav', hovedkat=1, outfile='BeinsmLavPreProTid.pdf')

#Infeksjoner ikke registrert i 2019
#3 kval.ind: Prolaps, Fusjon, SS
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                    Ngrense = 30, hovedkat = 1, outfile='KpInf3mndPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                    Ngrense = 30, hovedkat = 5, outfile='KpInf3mndFusj.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar,
                                   Ngrense = 30, hovedkat=9, outfile='KpInf3mndSS.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat = 1, outfile='KpInf3mndProAar.pdf')
# dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
#                     Ngrense = 30, hovedkat = 5, outfile='KpInf3mndFusjAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat=9, outfile='KpInf3mndSSAar.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=1, outfile='KpInf3mndProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=5, outfile='KpInf3mndFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=9, outfile='KpInf3mndSSTid.pdf')


RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura', aar=rappAar,
                    Ngrense = 30, hovedkat = 1, outfile='PeropKompDuraPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura', aar=rappAar,
                    Ngrense = 30, hovedkat = 5, outfile='PeropKompDuraFusj.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura', aar=rappAar,
                    Ngrense = 30, hovedkat=9, outfile='PeropKompDuraSS.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, #tidlOp=4, #hastegrad=1, fjernet fra -21
                       Ngrense = 30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarPro.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, #tidlOp=4, #hastegrad=1, fjernet for -21
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarFusj.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, #tidlOp=4, #hastegrad=1, #fjernet fra -21
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarSS.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, outfile='PeropKompDuraProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, outfile='PeropKompDuraFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, outfile='PeropKompDuraSSTid.pdf')



RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidDS.pdf', tidsenhet = 'Aar',
               valgtVar='OswEndr', hovedkat=10, ktr=ktr)

#Betydelig forbedring av ODI etter prolapskirurgi og spinal stenose
RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr20',  outfile='OswEndr20Pro.pdf',
                    aar=aar2_12mnd, hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)
RyggFigAndelTid(RegData=RegData, valgtVar='OswEndr20', outfile='OswEndr20ProTid.pdf',
                hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSS.pdf',
                    aar=aar2_12mnd, hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)
RyggFigAndelTid(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSSTid.pdf',
                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2)

# Andel skjema som er registrert innen 12 uker etter at pasienten er uskrevet, registreringsforsinkelse per sykehus.
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='regForsinkelse', preprosess = 0,
                    outfile='RegForsinkelse_Sh.pdf') #RegForsinkelse_Sh.pdf
RyggFigAndelTid(RegData=RegData, valgtVar='regForsinkelse', outfile='RegForsinkelseTid.pdf')

#Andel skjema som ikke er utfylt innen 3 mnd.etter operasjon,  tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='oppf3mnd', outfile='Oppf3mndTid.pdf')

#Andel operasjoner der sjekkliste for «Trygg kirurgi» er brukt, tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='tryggKir', outfile='TryggKirurgiTid.pdf')

#Ventetid fra kirurgi er besl. til utført under 3 mnd., tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='ventetidSpesOp', outfile='VentetidSpesOpTid.pdf')



#Andel som rapporterte betydelig forbedring av funksjon i dagliglivet (ODI) etter  kirurgi, , tidstrend


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
setwd("/home/rstudio/rygg/Aarsrapp")
rappAar <- 2020
valgteAar <- 2011:rappAar
RyggData <- RyggUtvalgEnh(RegData=RyggData, aar=valgteAar)$RegData


DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='sympVarighUtstr', aar=valgteAar,
                                 hovedkat=1,
                                 lastNedFil=1, indID = 'nkr_rygg_varighet_bensmerter', filUt = 'ind1_Varighet_bensmerter')

#--Bensmerter mindre eller lik 3 på numerisk smerteskala
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='smBePreLav', aar=valgteAar, hovedkat=1,
                                lastNedFil=1, indID = 'nkr_rygg_lav_bensmerte_prolaps', filUt = 'ind2_lav_bensmerte_prolaps')

#--Sårinfeksjon, dyp og overfladisk
DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar, hovedkat=1,
                             lastNedFil=1, indID = 'nkr_rygg_saarinfeksjon_prolaps', filUt = 'ind3_Saarinfeksjon_prolaps')

DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar, hovedkat=9,
                             lastNedFil=1, indID = 'nkr_rygg_saarinfeksjon_stenose', filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=1, tidlOp=4, hastegrad=1,
                                lastNedFil=1, indID = 'nkr_rygg_durarift_prolaps', filUt = 'ind5_Durarift prolaps')
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=9, tidlOp=4, hastegrad=1,
                                lastNedFil=1, indID = 'nkr_rygg_durarift_stenose', filUt = 'ind6_Durarift_stenose')

# IKKE? valgtVar='peropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,

#Ventetid, operasjon bestemt til utført
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='ventetidSpesOp', aar=valgteAar,
                                hovedkat=9, tidlOp=4, hastegrad=1,
                                lastNedFil=1, indID = 'nkr_rygg_ventetid_kirurgi', filUt = 'ind8_VentetidOperasjon')

#-------Oswestry------
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr20', aar=valgteAar,
                                hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2,
                                lastNedFil=1, indID = 'nkr_rygg_odi20p12mnd_prolaps', filUt = 'ind9_OswEndr20poengPro')
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr30pst', aar=valgteAar,
                                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2,
                                lastNedFil=1, indID = 'nkr_rygg_odi30pst12mnd_stenose', filUt = 'ind10_OswEndr30pstPro')


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

RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'TidlOpAnt3Tid.pdf') #TidlOp3 <-
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='tidlOp3', aar=rappAar, outfile='TidlOpAnt3Sh.pdf')


UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='utd', aar = startAar:rappAar, preprosess = 0,
                                outfile='FigUtdAar.pdf')
UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='uforetrygdPre', preprosess = 0, outfile='FigUforTid.pdf')
ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='erstatningPre', preprosess = 0, outfile='FigErstatTid.pdf')

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', outfile='FigRokerTid.pdf')
Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='utd', datoFra=datoFra1aar, datoTil=datoTil,
                            outfile='FigUtd.pdf')




HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='hovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='HovedInngrep.pdf')



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
AlderAar <- tapply(RegData$Alder, RegData$Aar, 'mean', na.rm=T)
(AlderAar <- sprintf('%.1f', AlderAar))
#Andel over 70 år:
(Andel70 <- sprintf('%.1f', Alder70Aar$AggVerdier$Hoved))


#Andelen pasienter med fedme:
  FedmeAar <- table(RegData$BMI>30, RegData$Aar)
  (AndelFedmeAar <- FedmeAar['TRUE',]/table(RegData$Aar)*100)

#Kjønnsfordeling, alle år, kvinner menn:
  (tabKjPst <- sprintf('%.1f',table(RegData$ErMann)/Ntot*100))

#Andelen fremmedspråklige (inkl. samisk) per år:
  round(FremmedSpraakAar$AggVerdier$Hoved,2)
  #(FremmedSpraak <- sprintf('%.1f', FremmedSpraakAar$AggVerdier$Hoved))

#Andelen ryggopererte med høyere utdanning (høyskole eller universitet):
  round(UtdanningTid$AggVerdier$Hoved,1)
  #UtdanningAar <- sprintf('%.1f', UtdanningTid$AggVerdier$Hoved)


#Andel i fullt arbeid når de blir ryggoperert:
ArbNum <- round(table(RegData1aar$ArbstatusPre)*100/sum(table(RegData1aar$ArbstatusPre)), 1)
ArbNum[1]

#Andel pasienter svart på spørsmål om arbeidsstatus, årsrapportåret: \% 22: 94,1
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


#Har søkt eller planlegger å søke uføretrygd:
round(UforTid$AggVerdier$Hoved,1)
#UforAar <- sprintf('%.1f', UforTid$AggVerdier$Hoved)

#Har søkt eller planlegger å søke erstatning:
round(ErstatningTid$AggVerdier$Hoved,1)
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
round(RoykTid$AggVerdier$Hoved, 1)


# #1: Samme nivå, 2:Annet nivå, 3: Annet og sm. nivå, 4: Primæroperasjon
#   #NB: I figuren er 4 kodet om til 0 !!!
# #Andelen reoperasjoner, per år:
# #
# AndelTidlOp <- RyggFigAndelStabelTid(RegData=RegData, valgtVar='tidlOp') #'TidlOp.pdf'
# AndelReop <- round(colSums(AndelTidlOp$AndelerHoved[2:4,]))
#
# #Av de pasientene operert i \Sexpr{rappAar} som hadde vært operert tidligere:
# AndelerSisteAar <- AndelTidlOp$AndelerHoved[,dim(AndelTidlOp$AndelerHoved)[2]]
# AndelerTidlOp <- sprintf('%.1f', AndelerSisteAar[2:4]/sum(AndelerSisteAar[2:4])*100)
# AndelerTidlOp[1] #\% operert i samme nivå,
# AndelerTidlOp[2] #operert i annet nivå
# AndelerTidlOp[3] #operert i både samme og annet nivå.

#Prolapspasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
round(table(RegDataPro$TidlOprAntall>2,RegDataPro$Aar)['TRUE',]/table(RegDataPro$Aar)*100,1)

#Andel lumbal spinal stenosepasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
round(table(RegDataSS$TidlOprAntall>2,RegDataSS$Aar)['TRUE',]/table(RegDataSS$Aar)*100,1)


#Andelen operert for lumbalt prolaps ved hjelp av synsfremmende midler:
round(prop.table(table(RegDataPro$OpMikro, RegDataPro$Aar),2)*100,1)

#Andelen operert for lumbal spinal stenose ved hjelp av synsfremmende midler:
round(prop.table(table(RegDataSS$OpMikro, RegDataSS$Aar),2)*100,1)



#Hyppigste tilstandene pasienter ble operert for i rappAar} var
#Tabell, fordeling av hovedinngrepstype
  HovedInngrepTab <- cbind('Antall' = HovedInngrep$Nvar$Hoved,
                           'Andeler' = paste0(round(as.numeric(HovedInngrep$AggVerdier$Hoved),1),'%')
  )
rownames(HovedInngrepTab) <- HovedInngrep$grtxt
xtable(HovedInngrepTab, align=c('l','r','r'), caption=paste0('Fordeling av hovedinngrep, ', rappAar), label="tab:HovedInngrep", digits=1)


#Andelen operert med dagkirurgi for  prolaps
ProDagTid <- table(RegDataPro[ ,c('Dagkirurgi', 'Aar')], useNA = 'a')
round(prop.table(ProDagTid[1:2,],2)*100,1)

#Andelen operert med dagkirurgi for spinal stenose
SSDagTid <- table(RegDataSS[ ,c('Dagkirurgi', 'Aar')], useNA = 'a')
round(prop.table(SSDagTid[1:2,],2)*100,1)

#Andel operert for spinal stenose som også hadde Degenerativ spondylolistese,
AntDegenSpondSS <-  dim(RyggUtvalgEnh(RegDataSS, hovedkat = 10, aar = rappAar)$RegData)[1]
round(AntDegenSpondSS/sum(RegDataSS$Aar==rappAar)*100,1)






#MANGLER:

#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, FØR operasjon:
indProPP <-  with(RegDataPro, which((Aar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
sprintf('%.1f', mean(RegDataPro$OswTotPre[indProPP]))
#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, ETTER operasjon:
sprintf('%.1f', mean(RegDataPro$OswTot12mnd[indProPP]))

#ODI-pre, lumbal spinal stenose:
indSSPP <-  with(RegDataSS, which((Aar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
sprintf('%.1f', mean(RegDataSS$OswTotPre[indSSPP]))
#ODI-post, lumbal spinal stenose:
sprintf('%.1f', mean(RegDataSS$OswTot12mnd[indSSPP]))

#ODI-pre, fusjonkirurgi:
indFusjPP <-  with(RegData,
                   which(HovedInngrepV2V3==5 & (Aar==(rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
sprintf('%.1f', mean(RegData$OswTotPre[indFusjPP]))
#ODI-pre, fusjonkirurgi:
sprintf('%.1f', mean(RegData$OswTot12mnd[indFusjPP]))



#Tabell, symptomvarighet
# UtsRHnum <- round(table(RegData1aar$SympVarighUtstr, useNA='a')*100/dim(RegData1aar)[1], 1)
# UtsRH <- paste(UtsRHnum, '%', sep='')
# names(UtsRH) <- c('Ingen utstrålende smerter', '< 3 mnd',
#                   '3 - 12 mnd', '1 - 2 år', '> 2 år', 'Ikke besvart')	#, 'Tot. ant.')
# xtable(cbind('Andeler'=UtsRH), caption=paste0('Varighet av nåværende utstrålende smerter, pasienter operert i ',
#                                               rappAar),
#        label="tab:Utstr", align=c('l','r'), digits=1)




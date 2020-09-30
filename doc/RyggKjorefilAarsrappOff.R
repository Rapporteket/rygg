#Generere filer og tall til årsrapport for 2019.
library(rygg)
library(xtable)

#Felles parametre:
startAar <- 2011
rappAar <- 2019
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
RegData12mnd <- RegData[which(RegData$Aar < rappAar), ] #For å ikke få med de som har fått 12mnd-skjema i inneværende år.
RegDataPro <- RegData[which(RegData$HovedInngrep==1),]
RegDataPro12mnd <- RegDataPro[which(RegDataPro$Aar<rappAar), ]
RegDataSS <- RyggUtvalgEnh(RegData, hovedkat=9)$RegData
#(Sjekk om antall stemmer!)

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]
AntAvd <- length(unique(RegData$ShNavn))

#RegData <- read.table('A:/Rygg/RyggV2V3_2020-09-22.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE,)

#--NB: For 2019 mangler registrering av infeksjoner, dvs. KpInfOverfla3Mnd','KpInfDyp3Mnd,
#table(RegData[,c('ShNavn', 'Aar')])

#MANGLER: Alle stabelfigurer. Eks. RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')
#NB: Sjekk om stabelfigurene er i bruk
#KpInfOverfla og Dyp - bruk data fra 2020
#-----------------



#---------FIGURER, årsrapport 2019--------------

# Dekningsgrad for hvert sykehus, Se tidligere figurer.

#NY2020: Ventetid fra operasjon bestemt til opr.tidpkt.N=20, Siste år, Variabel: VentetidSpesialistTilOpr, Utvalg: Elektive.
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
               hastegrad=1, outfile='VentetidBestOp_Sh.pdf')

# Andel skjema som er registrert innen 12 uker etter at pasienten er uskrevet, registreringsforsinkelse per sykehus.
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='regForsinkelse', preprosess = 0,
               outfile='RegForinkelse_Sh.pdf')

RyggFigAndeler(RegData = RegData1aar, valgtVar='AntibiotikaMedikament',
               preprosess = 0, outfile = 'AntibiotikaMedikament.pdf')


RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=aar2, tidlAar=tidlAar2, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, outfile='SympVarighUtstrTidPro.pdf')

BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='smBePreLav', aar=(rappAar-1):rappAar,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPrePro.pdf')
# KpInf3MndTidSmlGr.pdf - kan ikke ha med i årsrapport for 2019, men Resport.
# KpInf3MndProAar
# KpInf3MndSSAar.pdf
# KpInf3MndPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', aar=(rappAar-1):rappAar,
#                                     Ngrense = 20,
#                                     hovedkat = 1, outfile='FigKpInf3MndPro.pdf')
# KpInf3MndSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', aar=(rappAar-1):rappAar,
#                                    Ngrense = 20,
#                                    hovedkat=9, outfile='FigKpInf3MndSS.pdf')
# Vent: PeropKompDuraTidSmlGr.pdf #linjediagram per år, ulike operasjonstyper og totalt

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarPro.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarFusj.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarSS.pdf')

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

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidDS.pdf', tidsenhet = 'Aar',
               valgtVar='OswEndr', hovedkat=10, ktr=ktr)

Alder70Aar <- RyggFigAndelTid(RegData=RegData, datoFra = datoFra, valgtVar='alder70', preprosess = 0,
                              outfile='Alder70.pdf')

RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdPro.pdf',
                 valgtVar='liggedogn', hovedkat = 1, valgtMaal = 'Gjsn')
RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdSS.pdf',
                 valgtVar='liggedogn', hovedkat=9, valgtMaal = 'Gjsn')

#NYE:
RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr20',  outfile='OswEndr20Pro.pdf',
                aar=aar2, hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSS.pdf',
                aar=aar2, hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)

#-----------------------------------------------------------------------------------------------------------------------------
#---------------------------Kvalitetsindikatorkjøring, 2018
#-----------------------------------------------------------------------------------------------------------------------------

#-----------Symptomvarighet, utstrålende smerter, før operasjon (andel ventet >1 år)
#Mål: 		Under 30 % (grønn) ventet mer enn ett år
#Hensikt: 	Redusere andel pasienter som har hatt symptomer for lenge før ryggoperasjon.
#Utvalg: prolapskirurgi og  foraminotomi + laminectomi slått sammen i en gruppe.
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='SympVarighUtstr', hovedkat=1,
                       Ngrense=20, aar=aar2, tidlAar=tidlAar2, outfile=paste0('SympVarighUtstrProAar.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='SympVarighUtstr', hovedkat=1,
                    Ngrense=20, aar=aar2, outfile=paste0('SympVarighUtstrPro.', format))
#RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='SympVarighUtstr', hovedkat=2:3,
#                       Ngrense=20, aar=aar2, tidlAar=tidlAar2, outfile='SympVarighUtstrProAar.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='SympVarighUtstr', hovedkat=1, outfile=paste0('SympVarighUtstrProTid.', format))

#-----------Bensmerter mindre eller lik 3 på numerisk smerteskala
#Mål: 		Cutt off på 3 % (grønn)
#Hensikt: 	Redusere andel pasienter som opereres på dårlig operasjonsindikasjon (lite bensmerter).
#Utvalg: prolaps
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='beinsmLavPre', aar=aar2, tidlAar=tidlAar2, #datoFra=datoFra, datoTil=datoTil,
                       hovedkat=1, Ngrense=20, outfile=paste0('BeinsmLavPreProAar.', format)) #hastegrad=hastegrad, tidlOp=tidlOp,
RyggFigAndelerGrVar(RegData=RegData, valgtVar='beinsmLavPre', aar=aar2,
                    hovedkat=1, Ngrense=20, outfile=paste0('BeinsmLavPrePro.', format)) #hastegrad=hastegrad, tidlOp=tidlOp,
RyggFigAndelTid(RegData=RegData, valgtVar='beinsmLavPre',  #datoFra=datoFra, datoTil=datoTil,
                hovedkat=1, outfile=paste0('BeinsmLavPreProTid.', format))

#-----------Sårinfeksjon, dyp og overfladisk
# Mål: 		Prolaps 2 % høy måloppnåelse (grønt),
# Stenose 3 % høy måloppnåelse (grønt)
# Fusjonskirurgi (LSS+Deg. Sp.listese)
# Hensikt: 	Redusere postoperative sårinfeksjoner
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1,  #Prolaps
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndProAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=9,  #Stenose
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndSSAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, #Fusjonskirurgi
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndFusjAar.', format))

RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1,  #Prolaps
                    Ngrense=30, aar=aar2,  outfile=paste0('KpInf3MndPro.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=9,  #Stenose
                    Ngrense=30, aar=aar2, outfile=paste0('KpInf3MndSS.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, #Fusjonskirurgi
                    Ngrense=30, aar=aar2, outfile=paste0('KpInf3MndFusj.', format))


RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1, outfile=paste0('KpInf3MndProTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, outfile=paste0('KpInf3MndFusjTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=9, outfile=paste0('KpInf3MndSSTid.', format))

#-----------Durarift
#Mål: 		Prolaps 2 % høy måloppnåelse (grønt),
#Stenose 3 % høy måloppnåelse (grønt),
#Fusjonskirurgi (LSS+Deg. Sp.listese)
#Hensikt: 	Redusere forekomst av peroperativ komplikasjon
#Komplikasjon durarift ved operasjon (prolaps, elektiv, primærop.), - lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraProAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraFusjAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=9, tidlOp=4, hastegrad=1,
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraSSAar.', format))

RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, hastegrad=1,
                    Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraPro.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, hastegrad=1,
                    Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraFusj.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=9, tidlOp=4, hastegrad=1,
                    Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraSS.', format))

RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, hastegrad=1,
                outfile=paste0('PeropKompDuraProTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, hastegrad=1, outfile=paste0('PeropKompDuraFusjTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=9, tidlOp=4, hastegrad=1, outfile=paste0('PeropKompDuraSSTid.', format))


format <- 'pdf'
#Gjennomsnittlig forbedring av ODI,
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, hastegrad=1, ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='') #paste0('ODIendrProAar.', format)) # tidlAar=tidlAar2,
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile=paste0('ODIendrFusjAar.', format))
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile=paste0('ODIendrSSAar.', format))
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1,  ktr=2,
                 Ngrense=30, aar=2011:(rappAar-1), outfile=paste0('ODIendrDSAar.', format))

RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, hastegrad=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrProTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, hastegrad=1, tidsenhet = 'Aar',  ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrFusjTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrSSTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrDSTid.', format))


#Nye, 2018-data			:
#Gjennomsnittlig forbedring av ODI, Prolaps
#Gjennomsnittlig forbedring av ODI, SS
#Gjennomsnittlig forbedring av ODI, Fusjonskirurgi
#Sårinfeksjon, fusjonskirurgi
#Durarift, fusjonskirurgi

(Ut <- RyggFigAndelTid(RegData=RegData, datoTil= '2017-12-31', valgtVar='Verre', ktr = 2,
                       hovedkat=1, #tidlOp=4, hastegrad=1,
                       aar=2011:2017))

#------------------------------------------------------------------------------------
#-----------Filer til Resultatportalen -----------------------
#------------------------------------------------------------------------------------
rm(list=ls())
library(nkr)
#NKRdata1 <- read.table('A:/Rygg/Versjon2/NKR2019-01-16.csv', sep=';', header=T) #, encoding = 'UTF-8')
NKRdata <- read.table('A:/Rygg/Versjon2/NKR2019-09.csv', sep=';', header=T) #, encoding = 'UTF-8')
RyggData <- RyggPreprosess(RegData=NKRdata)
datoFra = '2011-01-01'

DataTilResultatportalen(RegData = RyggData, valgtVar='SympVarighUtstr', datoFra = '2011-01-01', hovedkat=1,
                        filUt = 'ind1_Varighet_bensmerter_Rygg')

#--Bensmerter mindre eller lik 3 på numerisk smerteskala
DataTilResultatportalen(RegData = RyggData, valgtVar='beinsmLavPre', datoFra = '2011-01-01', hovedkat=1,
                        filUt = 'ind2_lav_bensmerte_prolaps_Rygg')

#--Sårinfeksjon, dyp og overfladisk
DataTilResultatportalen(RegData = RyggData, valgtVar='KpInf3Mnd', datoFra = '2011-01-01', hovedkat=1,
                        filUt = 'ind3_Saarinfeksjon_prolaps')
DataTilResultatportalen(RegData = RyggData, valgtVar='KpInf3Mnd', datoFra = '2011-01-01', hovedkat=9,
                        filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
DataTilResultatportalen(RegData = RyggData, valgtVar='PeropKompDura', datoFra = '2011-01-01', hovedkat=1,
                        tidlOp=4, hastegrad=1, filUt = 'ind5_Durarift prolaps_Rygg')
DataTilResultatportalen(RegData = RyggData, valgtVar='PeropKompDura', datoFra = '2011-01-01', hovedkat=9,
                        tidlOp=4, hastegrad=1, filUt = 'ind6_Durarift_stenose_Rygg')

#Alle sykehus og resh:
ShResh <- unique(RegData[c('ReshId', 'ShNavn')])
write.table(ShResh, file = 'A:/Resultatportalen/RyggShResh', sep = ';', row.names = F)

DataTilResultatportalen <- function(RegData = RegData, valgtVar, datoFra = '2011-01-01',
                                    hovedkat=99, hastegrad=99, tidlOp='', filUt='dummy'){
  filUt <- paste0(ifelse(filUt=='dummy',  valgtVar, filUt), '.csv')
  RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  RyggUtvalg <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, datoFra = datoFra, hovedkat=hovedkat, tidlOp=tidlOp, hastegrad=hastegrad) #, datoTil=datoTil, aar=aar)
  RegData <- RyggUtvalg$RegData
  RyggTilOffvalgtVar <- RegData[,c('Aar', "ShNavn", "ReshId", "Variabel")]
  info <- c(RyggVarSpes$tittel, RyggUtvalg$utvalgTxt)
  RyggTilOffvalgtVar$info <- c(info, rep(NA, dim(RyggTilOffvalgtVar)[1]-length(info)))
  write.table(RyggTilOffvalgtVar, file = paste0('A:/Resultatportalen/', filUt), sep = ';', row.names = F) #, fileEncoding = 'UTF-8')
}

#---Nøkkelinformasjon, Resultatportalen------
#---- R Y G G
#Til forside årsrapport:
load('A:/Rygg/Rygg2010-2018aarsrapp.Rdata')
RyggData <- RyggPreprosess(RegData)
antSh <- colSums(table(as.character(RyggData$ShNavn),RyggData$Aar)>0)
antOp <- table(RyggData$Aar)

#Andel som svarer på oppfølging 3 og 12 mnd.
andelSvart3mnd <- tapply(RyggData$Utfylt3Mnd,RyggData$Aar, FUN='mean', na.rm=T)
andelSvart12mnd <- tapply(RyggData$Utfylt12Mnd,RyggData$Aar, FUN='mean', na.rm=T)

RyggData$over70 <- 0
RyggData$over70[RyggData$Alder>=70] <- 1
andel70aar <- tapply(RyggData$over70,RyggData$Aar, FUN='mean', na.rm=T)
alderGjsn <- tapply(RyggData$Alder,RyggData$Aar, FUN='mean', na.rm=T)
alderMedian <- tapply(RyggData$Alder,RyggData$Aar, FUN='median', na.rm=T)
andelKvinner <- 1-tapply(RyggData$ErMann,RyggData$Aar, FUN='mean', na.rm=T)

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
andelForn <- function(RyggData, ktr=1){
  RyggData$Variabel <- 0
  RyggData$Utfylt <- switch(ktr, RyggData$Utfylt3Mnd, RyggData$Utfylt12Mnd)
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
  Data$Utfylt <- switch(ktr, '1' = Data$Utfylt3Mnd, '2'=Data$Utfylt12Mnd)
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
andelSuksess12mnd <- andelEndring(RyggData, ktr=2)$Suksess
andelVerre3mnd <- andelEndring(RyggData, ktr=1)$Verre
andelVerre12mnd <- andelEndring(RyggData, ktr=2)$Verre


NokkeltallRygg <- rbind(
  'Antall avdelinger' = antSh,
  'Antall operasjoner' = antOp,
  'Svart på oppfølging, 3 mnd.' = andelSvart3mnd,
  'Svart på oppfølging, 12 mnd.' = andelSvart12mnd,
  'Andel over 70 år'	= andel70aar,
  'Gjennomsnittsalder' = alderGjsn,
  #   'Medianalder' = alderMedian,
  'Andel kvinner' = andelKvinner,
  'Fornøyd med behandlingen, 3 mnd. etter' = andelFornoyd3mnd,
  'Fornøyd med behandlingen, 12 mnd. etter' = andelFornoyd12mnd,
  'Helt restituert/mye bedre, 3 mnd. etter' = andelSuksess,
  'Helt restituert/mye bedre, 12 mnd. etter' = andelSuksess,
  'Verre 3 mnd. etter' = andelVerre
  'Verre 12 mnd. etter' = andelVerre
)
tabNokkeltallRygg <- cbind(row.names(NokkeltallRygg),NokkeltallRygg)
tabNokkeltallRygg[,c('2017','2018')]

#write.table(tabNokkeltallRygg, file = 'A:/Resultatportalen/NokkeltallRygg.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )

RyggFigAndeler(RyggData, valgtVar = 'nytte', ktr=2, aar = 2017, outfile = 'NytteFord.png')







#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#---------Figurer, utdata til videre beregninger
UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='utd', aar = startAar:rappAar, preprosess = 0,
                                outfile='FigUtdAar.pdf')
UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='uforetrygdPre', preprosess = 0, outfile='FigUforTid.pdf')
ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='erstatningPre', preprosess = 0, outfile='FigErstatTid.pdf')

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', outfile='FigRokerTid.pdf')
Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='Utd', datoFra=datoFra1aar, datoTil=datoTil,
                            outfile='FigUtd.pdf')


AndelTidlOp <- RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')


HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='hovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='HovedInngrep.pdf')


TidlOp3 <- RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'FigTidlOpAnt3.pdf')

KpInf3MndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='KpInf3Mnd',
                                   hovedkat = 1, outfile='FigKpInf3MndTidPro.pdf')
KpInf3MndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='KpInf3Mnd',
                                   hovedkat=9, outfile='FigKpInf3MndTidSS.pdf')

DuraPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura',
                               aar=(rappAar-1):rappAar, Ngrense = 20,
                               hastegrad = 1, tidlOp = 4, hovedkat = 1, outfile='FigDuraPro.pdf')
DuraSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura',
                              aar=(rappAar-1):rappAar, Ngrense = 20,
                              hastegrad = 1, tidlOp = 4, hovedkat=9, outfile='FigDuraSS.pdf')
# sjekket
FremmedSpraakAar <-  RyggFigAndelTid(RegData=RegData, valgtVar='morsmal', aar = startAar:rappAar,
                                     outfile='FigMorsmalAar.pdf', preprosess = 0)


#--------Figurer som muligens er ute--------------
RyggFigGjsnBox(RegData=RegData, aar = rappAar-1, valgtVar ='smBeinEndrPre',  preprosess = 0,
               ktr=2, outfile='FigBeinsmEndrPre.pdf')


RyggFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='alder', datoFra=datoFra1aar, datoTil=datoTil, outfile='FigAlderFord.pdf')

RyggFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='BMI', datoFra=datoFra, datoTil=datoTil,
               outfile='FigBMI.pdf')

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrPro.pdf',
               aar=startAar:(rappAar-1), valgtVar='OswEndr', hovedkat=1, ktr=2)
RyggFigGjsnBox(RegData=RegData, outfile='OswEndrSS.pdf',
               aar=startAar:(rappAar-1), valgtVar='OswEndr', hovedkat=9, ktr=2)

RyggFigGjsnGrVar(RegData=RegData, outfile='OswEndrAvdPro.pdf', Ngrense = 20,
                 aar=c((rappAar-2):(rappAar-1)),
                 valgtVar='OswEndr', hovedkat=1, tidlOp=4, hastegrad=1, ktr=ktr)
RyggFigGjsnGrVar(RegData=RegData, outfile='OswEndrAvdSS.pdf', Ngrense = 20,
                 aar=c((rappAar-2):(rappAar-1)),
                 valgtVar='OswEndr', hovedkat=9, tidlOp=4, ktr=ktr)


RyggFigAndelTid(RegData=RegData, outfile='Osw20TidPro.pdf',
                aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr20', hovedkat=1, ktr=ktr)
RyggFigAndelTid(RegData=RegData, outfile='Osw30TidSS.pdf',
                aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr30pst', hovedkat=9, ktr=ktr)

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidPro.pdf',
               aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr', hovedkat=1, ktr=ktr)

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidSS.pdf',
               aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr', hovedkat=9, ktr=ktr)

RyggFigAndelStabelTid(RegData=RegData, aar=startAar:(rappAar-1), outfile='FigNyttePro.pdf', valgtVar='Nytte',
                      hovedkat=1, ktr=ktr)

RyggFigAndelStabelTid(RegData=RegData, aar=startAar:(rappAar-1), outfile='FigNytteSS.pdf', valgtVar='Nytte',
                      hovedkat=9, ktr=ktr)
FornoydProTid <- RyggFigAndelStabelTid(RegData=RegData, outfile='FigFornoydPro.pdf',
                                       valgtVar='Fornoyd', hovedkat=1, aar=startAar:(rappAar-1), ktr=ktr)
FornoydSSTid <- RyggFigAndelStabelTid(RegData=RegData, outfile='FigFornoydSS.pdf',
                                      valgtVar='Fornoyd', hovedkat=9, aar=startAar:(rappAar-1), ktr=ktr)

RyggFigGjsnBox(RegData=RegData, valgtVar='Liggedogn', datoFra=datoFra, datoTil=datoTil,
               hovedkat = 1, outfile='LiggetidProlaps.pdf') #



RyggFigAndelerGrVar(valgtVar='SympVarighUtstr', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=1,
                           preprosess = 0, outfile='VarighUtstrAvdPro.pdf')
RyggFigAndelerGrVar(valgtVar='SympVarighUtstr', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=9,
                           preprosess = 0, outfile='VarighUtstrAvdSS.pdf')

RyggFigAndelTid(valgtVar='SympVarighUtstr', RegData=RegData,
                       preprosess = 0, hovedkat=9,outfile='VarighUtstrTidSS.pdf')


RyggFigAndelerGrVar(valgtVar='SymptVarighRyggHof', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=1,
                           preprosess = 0, outfile='VarighRyggHofAvdPro.pdf')
RyggFigAndelerGrVar(valgtVar='SymptVarighRyggHof', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=9,
                           preprosess = 0, outfile='VarighRyggHofAvdSS.pdf')








#--------------TABELLER OG TALL---------------------------
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
#AlderAar <- sprintf('%.1f', AlderAar)
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

#Tabell, pasienter som har vært til radiologiske undersøkelser.
#MÅ EVT OPPDATERES. Borte: RvDiscogr', 'RvRadigr',
#Sjekk om skal være med!
# RVvar <- c('RvCt', 'RvMr', 'RvDpregblok', 'RvRtgLscol', 'RvFunksjo')
# RVant <- colSums(RegData1aar[, RVvar], na.rm=T)
# RVpst <- round(RVant*100/Ntot1aar,1)
#
# RV <- cbind('Antall' = c(RVant, Ntot1aar),
#             'Andeler' = c(paste0(RVpst, '%'),' ')
# )
# rownames(RV) <- c('CT', 'MR', 'Radikulografi', 'Diskografi', 'Diagnostisk blokade',
#                   'Røntgen LS-columna', 'Med fleksjon/ekstensjon', 'Tot. ant.')
# xtable(RV, caption=paste0('Radiologisk vurdering, ',rappAar),
#        label="tab:RV", align=c('l','r','r'))



#Tabell, diagnoser basert på radiologiske funn
#Sjekk inklusjon av variabler !!!!!!!! Borte: "RfNormal", "RfForamino", "RfDegen", "RfPseudom"
# RFvar <- c( "RfSkive", "RfSentr", "RfLateral",
#            "RfSpondtypeIsmisk", "RfSpondtypeDegen", "RfDegskol", "RfSynovpre")
# RFant <- colSums(RegData1aar[, RFvar], na.rm=T)[-1]
# RFpst <- round(RFant*100/Ntot1aar)
# RF <- cbind('Antall' = c(RFant, Ntot1aar),
#             'Andeler' = c(paste0(RFpst, '%'),' '))
#
# #Teller opp de som er diagnostisert som normale:
# RFNorm <- intersect(which(rowSums(RegData1aar[, RFvar[2:11]], na.rm=T) == 0),
#                     which(RegData1aar$RfNormal == 1))
# rownames(RF) <- c('Skiveprolaps', 'Sentral spinalstenose', 'Lateral spinalstenose',
#                   'Foraminal stenose', 'Degenerativ rygg/skivedegenerasjon',
#                   'Istmisk spondylolistese', 'Degenerativ spondylolistese',
#                   'Degenerativ skoliose', 'Synovial syste', 'Pseudomeningocele', 'Tot.ant.')
# xtable(RF, caption=paste0('Radiologiske diagnoser, ', rappAar),
#        label="tab:RF", align=c('l','r','r'))


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
MikroAarPro <- prop.table(table(RegDataPro$OpMikro, RegDataPro$Aar),2)*100
BruktMikroAarPro <- sprintf('%.0f', MikroAarPro['1',])

#Andelen operert for lumbal spinal stenose ved hjelp av synsfremmende midler:
MikroAarSS <- prop.table(table(RegDataSS$OpMikro, RegDataSS$Aar),2)*100
BruktMikroAarSS <- sprintf('%.0f', MikroAarSS['1',])


#SaarInfSS <- prop.table(table(RegDataSS$KpInf3Mnd, RegDataSS$Aar),2)*100 - har ikke sårinfeksjon for 2019
#FornoydPro <- sprintf('%.0f',)
prop.table(table(RegDataPro12mnd$Fornoyd12mnd, RegDataPro12mnd$Aar),2)[1,]*100
#FornoydSS <- sprintf('%.0f', )
prop.table(table(RegDataSS$Fornoyd12mnd, RegDataSS$Aar),2)[1,]*100



#Hyppigste tilstandene pasienter ble operert for i rappAar} var
#Tabell, fordeling av hovedinngrepstype
  HovedInngrepTab <- cbind('Antall' = HovedInngrep$AggVerdier$Hoved,
                           'Andeler' = paste0(round(as.numeric(HovedInngrep$AggVerdier$Hoved)),'%')
  )
rownames(HovedInngrepTab) <- HovedInngrep$grtxt
xtable(HovedInngrepTab, align=c('l','r','r'), caption=paste0('Fordeling av hovedinngrep, ', rappAar), label="tab:AntHovedInngrep", digits=1)




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



#Tabell, antibiotika
AntibiotikaData <- RyggFigAndelTid(RegData=RegData, outfile='Antibiotika.pdf',
                                     valgtVar = 'antibiotika')
Antibiotika <- rbind('Andeler' = paste(round(AntibiotikaData$AggVerdier$Hoved, 1), '%',sep=''),
                     'Antall' = AntibiotikaData$Ngr$Hoved)
xtable(Antibiotika, caption=AntibiotikaData$Tittel, label="tab:AntibiotikaAndel",
       align=c('l',rep('r', rappAar- startAar +1)))





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




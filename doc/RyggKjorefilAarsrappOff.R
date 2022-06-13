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

#---------FIGURER, årsrapport --------------

# Dekningsgrad for hvert sykehus, Se tidligere figurer. IKKE FÅTT DEKN.GRADSANALYSE FOR 2020 (1.mar 2021)
#RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Rygg', outfile='DGrygg.pdf')
#RyggFigAndelerGrVar(RegData=0, valgtVar='dekn19Nakke', outfile='DGnakke.pdf') #

#NY2021: Ventetid fra operasjon bestemt til opr.tidpk
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
               hastegrad=1, outfile='VentetidBestOp_Sh.pdf')

RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidHenvTimePol', Ngrense = 20,
                    hastegrad=1, outfile='VentetidHenvTimePol_Sh.pdf')

# RyggFigAndeler(RegData = RegData1aar, valgtVar='antibiotikaMedikament',
#                preprosess = 0, outfile = 'AntibiotikaMedikament.pdf')


# Vent: PeropKompDuraTidSmlGr.pdf #linjediagram per år, ulike operasjonstyper og totalt


# RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, hastegrad=1, ktr=2,
#                  Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarPro.pdf')
# RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, hastegrad=1,  ktr=2,
#                  Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarFusj.pdf')
# RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, hastegrad=1,  ktr=2,
#                  Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarSS.pdf')
# RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=10, tidlOp=4, hastegrad=1,  ktr=2,
#                  Ngrense=30, aar=aar2_12mnd, outfile='ODIendrAarDS.pdf')

# RyggFigAndelerGrVar(RegData=RegData, valgtVar='Osw22', ktr=2,
#                     aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
#                     hovedkat=1,  hastegrad=1, tidlOp=4,  outfile='Osw22Pro.pdf')
# RyggFigAndelerGrVar(RegData=RegData, valgtVar='Osw22', ktr=2,
#                     aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
#                     hovedkat=9,  hastegrad=1, tidlOp=4,  outfile='Osw22SS.pdf')

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


#RyggFigGjsnGrVar(RegData=RegData1aar, valgtVar='liggetidPostOp', outfile='LiggetidPostOpGjsn.pdf')

#------ KVALITETSINDIKATORER------------

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=rappAar, tidlAar=tidlAar, hastegrad=1, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, preprosess = 0,
                    Ngrense=20, aar=aar2, outfile='SympVarighUtstrShFusj.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, hastegrad=1, outfile='SympVarighUtstrTidPro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, outfile='SympVarighUtstrTidFusj.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=9, outfile='SympVarighUtstrTidSS.pdf')


BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='smBePreLav', aar=aar2,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPrePro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='smBePreLav', hovedkat=1, outfile='BeinsmLavPreProTid.pdf')

#Infeksjoner ikke registrert i 2019
#3 kval.ind: Prolaps, Fusjon, SS
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat = 1, outfile='KpInf3mndProAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat=9, outfile='KpInf3mndSSAar.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=1, outfile='KpInf3mndProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=5, outfile='KpInf3mndFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=9, outfile='KpInf3mndSSTid.pdf')


RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura', aar=rappAar, tidlOp=4,
                    Ngrense = 30, hovedkat=9, outfile='PeropKompDuraSS.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, #hastegrad=1, fjernet fra -21
                       Ngrense = 30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarPro.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, #hastegrad=1, #fjernet fra -21
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarSS.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, outfile='PeropKompDuraProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, outfile='PeropKompDuraFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, outfile='PeropKompDuraSSTid.pdf')



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

#Ventetid fra kirurgi er besl. til utført under 3 mnd., tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='ventetidSpesOp', outfile='VentetidSpesOpTid.pdf')



#------------------------------------------------------------------------------------
#-----------Filer til Interaktive nettsider -----------------------
#------------------------------------------------------------------------------------

library(rygg)
library(magrittr)
setwd('/home/rstudio/speil/aarsrapp/')
RyggData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())
rappAar <- 2021
valgteAar <- 2011:rappAar
RyggData <- RyggUtvalgEnh(RegData=RyggData, aar=valgteAar)$RegData

#Varighet av utstrålende smerter minst ett år
# Andel pasienter operert for lumbalt prolaps som har hatt utstrålende smerter i mer enn ett år før operasjonen.
# ØNSKET MÅLNIVÅ: ≤ 20 %
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='sympVarighUtstr', aar=valgteAar,
                                 hovedkat=1,
                                 hastegrad = 1,
                                 indID = 'nkr_rygg_varighet_bensmerter',
                                 filUt = 'ind1_Varighet_bensmerter')

#--Bensmerter mindre eller lik 3 på numerisk smerteskala. SLÅ SAMMEN 2 ÅR
# Lite beinsmerter og ingen parese - SJEKK PARESE!
# Andel pasienter med lite beinsmerter (≤ 3) operert for lumbale prolaps siste to år
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='smBePreLav', aar=valgteAar,
                                 hovedkat=1,
                                 slaaSmToAar=1,
                                 indID = 'nkr_rygg_lav_bensmerte_prolaps',
                                filUt = 'ind2_lav_bensmerte_prolaps')

#--Sårinfeksjon, dyp og overfladisk
# Sårinfeksjon, pasientrapportert
# Andel pasienter som rapporterer om sårinfeksjon (overflies og dyp) 3 måneder etter
#lumbal prolapskirurgi de siste 2 års perioder.
# ØNSKET MÅLNIVÅ: ≤ 2,0 %
DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar,
                                hovedkat=1,
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_saarinfeksjon_prolaps',
                                filUt = 'ind3_Saarinfeksjon_prolaps')

# Sårinfeksjon etter lumbal spinal stenose operasjon
# Andel pasienter som rapporterer om sårinfeksjon (overfladisk og dyp) 3 måneder etter
#lumbal spinal stenose operasjon de siste 2 års perioder.
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
DataTilRes <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd', aar=rappAar,
                                hovedkat=9,
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_saarinfeksjon_stenose',
                                filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
# Andel pasienter som fikk durarift etter kirurgi for lumbalt prolaps.
# Andel pasienter som fikk durarift etter kirurgi for lumbalt prolaps de siste 2 års perioder, elektive pasienter, ikke tidligere ryggopererte.
# ØNSKET MÅLNIVÅ: ≤ 2,0 %
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=1, tidlOp=4, hastegrad=1,
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_durarift_prolaps',
                                filUt = 'ind5_Durarift prolaps')

# Andel pasienter som fikk durarift etter kirurgi for lumbal spinal stenose siste 2 år.
# Andel pasienter som fikk durarift etter kirurgi for lumbal spinal stenose siste 2 år, elektive pasienter, ikke tidligere ryggopererte.
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura', aar=valgteAar,
                                hovedkat=9, tidlOp=4, hastegrad=1,
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_durarift_stenose',
                                filUt = 'ind6_Durarift_stenose')

#Ventetid, operasjon bestemt til utført
# Ventetid på kirurgi
# Ventetid < 3 måneder fra ryggkirurgi ble bestemt (ved spesialist poliklinikk) til operasjonen ble utført.
# ØNSKET MÅLNIVÅ: ≥ 80 %
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='ventetidSpesOp', aar=valgteAar,
                                hastegrad=1,
                                indID = 'nkr_rygg_ventetid_kirurgi', filUt = 'ind8_VentetidOperasjon')

#-------Oswestry------
# Forbedring av fysisk funksjon i dagliglivet, prolapskirurgi
# Andel som oppnår 20 prosentpoeng forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter prolapskirurgi
# ØNSKET MÅLNIVÅ: ≥ 69 %
#! SPM - for 2021, skal vi vise de som ble operert i 2021 eller de som svarte i 2021
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr20', aar=valgteAar,
                                hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_odi20p12mnd_prolaps', filUt = 'ind9_OswEndr20poengPro')
# Forbedring av fysisk funksjon i dagliglivet, spinal stenose kirurgi
# Andel som oppnår 30 % forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter kirurgi for spinal stenose
# ØNSKET MÅLNIVÅ: ≥ 67 %
#! Skal vise de som svarte i 2021
DataTilSKDE <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr30pst', aar=valgteAar,
                                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                                slaaSmToAar=1,
                                indID = 'nkr_rygg_odi30pst12mnd_stenose', filUt = 'ind10_OswEndr30pstSS')


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

#---------Figurer, utdata til videre beregninger----

RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'TidlOpAnt3Tid.pdf') #TidlOp3 <-
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='tidlOp3', aar=rappAar, outfile='TidlOpAnt3Sh.pdf')


UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='utd', aar = startAar:rappAar, preprosess = 0,
                                outfile='')
UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='uforetrygdPre', preprosess = 0, outfile='')
ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='erstatningPre', preprosess = 0, outfile='')

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', outfile='')
Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='utd', datoFra=datoFra1aar, datoTil=datoTil,
                            outfile='')

HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='hovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='')

KpInf3mndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='kpInf3mnd',
                                   hovedkat = 1, outfile='')
KpInf3mndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='kpInf3mnd',
                                   hovedkat=9, outfile='')

DuraPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura',
                               aar=(rappAar-1):rappAar, Ngrense = 20,
                               hastegrad = 1, tidlOp = 4, hovedkat = 1, outfile='')
DuraSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura',
                              aar=(rappAar-1):rappAar, Ngrense = 20,
                              hastegrad = 1, tidlOp = 4, hovedkat=9, outfile='')
# sjekket
FremmedSpraakAar <-  RyggFigAndelTid(RegData=RegData, valgtVar='morsmal', aar = startAar:rappAar,
                                     outfile='', preprosess = 0)






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


#Generere filer og tall til årsrapport for 2022.
library(rygg)
library(xtable)
setwd('~/Aarsrappresultater/NKR/')

#Felles parametre:
startAar <- 2011
rappAar <- 2022
datoFra1aar <- paste0(rappAar,'-01-01')
datoFra2aar <- paste0(rappAar-1,'-01-01')
datoFra3aar <- paste0(rappAar-2,'-01-01')
datoTil12mnd <- paste0(rappAar-1,'-12-31')
datoFra <- as.Date(paste0(startAar,'-01-01'))
datoTil <- as.Date(paste0(rappAar,'-12-31'))

aar2 <- (rappAar-1):rappAar
aar2_12mnd <- aar2-1
tidlAar <- rappAar-1
tidlAar2 <- (rappAar-3):(rappAar-2)
Ngrense <- 20
AKjust <- 0
ktr <- 2

RyggData <- RyggRegDataSQLV2V3(alleVarV3 = 0)
RegData <- RyggPreprosess(RegData=RyggData)
Ntot07 <- dim(RegData)[1]

RegDataV3 <- RegData[RegData$Aar >2018, ]
table(RegData[,c('Aar', "Ferdigstilt1b3mnd")])
test <- RegData[ ,c('Aar', "Ferdigstilt1b3mnd", 'Utfdato3mnd', "Nytte3mnd")]
table(RegData[,c('Aar', "KpInfDyp3mnd")], useNA = 'a')


#Gjør utvalg/tilrettelegge årsfiler
RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)$RegData #RegData[which(RegData$InnDato>= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil)), ] #
RegData1aar <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
write.table(RegData, file = 'RyggAarsrapp2022.csv', sep = ';', row.names = F, fileEncoding = 'latin1', na = '')

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]
AntAvd <- length(unique(RegData$ShNavn))

#---------FIGURER, årsrapport --------------

# Dekningsgrad for hvert sykehus, Se tidligere figurer.
#RyggFigAndelerGrVar(RegData=0, valgtVar='dekn21Rygg', outfile='DGrygg.pdf')


RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidHenvTimePol', Ngrense = 20,
                    hastegrad=1, outfile='VentetidHenvTimePol_Sh.pdf')


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
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=rappAar, tidlAar=tidlAar, hastegrad=1, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, preprosess = 0,
                    Ngrense=20, aar=aar2, outfile='SympVarighUtstrShFusj.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, hastegrad=1, outfile='SympVarighUtstrTidPro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=5, outfile='SympVarighUtstrTidFusj.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=9, outfile='SympVarighUtstrTidSS.pdf')



#Infeksjoner ikke registrert i 2019
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat = 1, outfile='KpInf3mndProAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat=9, outfile='KpInf3mndSSAar.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=1, outfile='KpInf3mndProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=5, outfile='KpInf3mndFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='kpInf3mnd', hovedkat=9, outfile='KpInf3mndSSTid.pdf')


RyggFigAndelerGrVar(RegData=RegData, valgtVar='peropKompDura', aar=rappAar, tidlOp=4,
                    Ngrense = 30, hovedkat=9, outfile='PeropKompDuraSS.pdf')
#Lena
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, #hastegrad=1, fjernet fra -21
                       Ngrense = 30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarPro.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, #hastegrad=1, #fjernet fra -21
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraAarSS.pdf')

RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=1, tidlOp=4, outfile='PeropKompDuraProTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=5, outfile='PeropKompDuraFusjTid.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='peropKompDura', hovedkat=9, tidlOp=4, outfile='PeropKompDuraSSTid.pdf')




#Andel oppfølgingsskjema som er besvart,  tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='oppf3mnd', outfile='Oppf3mndTid.pdf')



#------ KVALITETSINDIKATORER------------
#K1 Ventetid fra kirurgi er besl. til utført under 3 mnd., tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='ventetidSpesOp', outfile='VentetidSpesOpTid.pdf')
#NY2021: Ventetid fra operasjon bestemt til opr.tidpk
RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
                    hastegrad=1, outfile='VentetidBestOp_Sh.pdf')


#K2 Lite beinsmerter/utstrålende smerter før prolapskirurgi
BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='smBePreLav', aar=aar2,
                                    Ngrense = 20, preprosess = 0, hovedkat=1,   outfile='BeinsmLavPrePro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='smBePreLav', hovedkat=1, outfile='BeinsmLavPreProTid.pdf')

# K3 og K4 Betydelig forbedring av ODI etter prolapskirurgi og spinal stenose
RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr20',  outfile='OswEndr20Pro.pdf',
                    aar=aar2_12mnd, hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)
RyggFigAndelTid(RegData=RegData, valgtVar='OswEndr20', outfile='OswEndr20ProTid.pdf',
                hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSS.pdf',
                    aar=aar2_12mnd, hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, Ngrense = 30)
RyggFigAndelTid(RegData=RegData, valgtVar='OswEndr30pst', outfile='OswEndr30pstSSTid.pdf',
                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2)


#Ny, mars 2023
# Kl 5. Andel pasienter med degenerativ spondylolistese som blir operert med
# fusjonskirurgi ved første operasjon
# Mål: ≤ landsgjennomsnittet høy måloppnåelse (grønt), &gt; landsgjennomsnittet moderat/lav (gult)
# Hensikt: Redusere andel pasienter med degenerativ spondylolistese som blir operert
# med fusjonskirurgi ved første operasjon
# Datakilde: NKR, legeskjema perioperativt
#MÅ finne ut hva som er første operasjon, og dette må gjøres før vi gjør utvalg.
#Betyr at variabelen operasjonsnummer må beregnes i preprosess

RyggFigAndelerGrVar(RegData=RegData, valgtVar='degSponFusj1op', preprosess = 0,
                    Ngrense=20, aar=(rappAar-1):rappAar, outfile='degSponFusj1opKISh.pdf')

RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar='degSponFusj1op',
                datoFra = '01-01-2019', outfile='degSponFusj1opKITid.pdf')

PROMadm <- rapbase::loadRegData(registryName="rygg",
                     query='SELECT * FROM proms')

#Ny, mars 2023
# K6 Andel som får tromboseprofylakse i forbindelse med lett ryggkirurgi.
# Spesifisering: (BlodfortynnendeFast = 0 &  ASA grad< 3 & Kjønn = 1 (mann)) & (HovedInngrepV2V3=1 eller HovedInngrepV2V3=2)
# Mål:  < landsgjennomsnittet høy måloppnåelse (grønt), ≥ landsgjennomsnittet moderat/lav (gult)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='trombProfylLettKI', preprosess = 0,
                    Ngrense=20, aar=rappAar, outfile='trombProfylLettKISh.pdf')

RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar='trombProfylLettKI',
                datoFra = '01-01-2019', outfile='trombProfylLettKITid.pdf')






#------------------------------------------------------------------------------------
#-----------Filer til Interaktive nettsider -----------------------
# Endring i kvalitetsindikatorer. MÅ OPPDATERES!!
#------------------------------------------------------------------------------------

library(rygg)
library(magrittr)
setwd('/home/rstudio/speil/aarsrapp/')
RyggData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())
rappAar <- 2021
valgteAar <- 2011:rappAar
#Viktig å angi valgte år siden vi
RyggData <- RyggUtvalgEnh(RegData=RyggData, aar=valgteAar)$RegData

#Varighet av utstrålende smerter minst ett år
# Andel pasienter operert for lumbalt prolaps som har hatt utstrålende smerter i mer enn ett år før operasjonen.
# ØNSKET MÅLNIVÅ: ≤ 20 %
ind1 <- dataTilOffVisning(RegData = RyggData, valgtVar='sympVarighUtstr',
                          aar = valgteAar,
                          slaaSmToAar=0,
                          hovedkat=1,
                          hastegrad = 1,
                          indID = 'nkr_rygg_varighet_bensmerter',
                          filUt = 'ind1_Varighet_bensmerter')

#--Bensmerter mindre eller lik 3 på numerisk smerteskala. SLÅ SAMMEN 2 ÅR
# Lite beinsmerter og ingen parese - SJEKK PARESE!
# Andel pasienter med lite beinsmerter (≤ 3) operert for lumbale prolaps siste to år
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
ind2 <- dataTilOffVisning(RegData = RyggData, valgtVar='smBePreLav',
                                 hovedkat=1,
                          aar = valgteAar,
                          slaaSmToAar=1,
                                 indID = 'nkr_rygg_lav_bensmerte_prolaps',
                                filUt = 'ind2_lav_bensmerte_prolaps')
#--Sårinfeksjon, dyp og overfladisk
# Sårinfeksjon, pasientrapportert
# Andel pasienter som rapporterer om sårinfeksjon (overflies og dyp) 3 måneder etter
#lumbal prolapskirurgi de siste 2 års perioder.
# ØNSKET MÅLNIVÅ: ≤ 2,0 %
ind3 <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd',
                                hovedkat=1,
                          aar = valgteAar,
                          slaaSmToAar=1,
                                indID = 'nkr_rygg_saarinfeksjon_prolaps',
                                filUt = 'ind3_Saarinfeksjon_prolaps')

# Sårinfeksjon etter lumbal spinal stenose operasjon
# Andel pasienter som rapporterer om sårinfeksjon (overfladisk og dyp) 3 måneder etter
#lumbal spinal stenose operasjon de siste 2 års perioder.
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
ind4 <- dataTilOffVisning(RegData = RyggData, valgtVar='kpInf3mnd',
                                hovedkat=9,
                          aar = valgteAar,
                          slaaSmToAar=1,
                                indID = 'nkr_rygg_saarinfeksjon_stenose',
                                filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
# Andel pasienter som fikk durarift etter kirurgi for lumbalt prolaps.
# Andel pasienter som fikk durarift etter kirurgi for lumbalt prolaps de siste 2 års perioder, elektive pasienter, ikke tidligere ryggopererte.
# ØNSKET MÅLNIVÅ: ≤ 2,0 %
ind5 <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura',
                                hovedkat=1, tidlOp=4, hastegrad=1,
                          aar = valgteAar,
                          slaaSmToAar=1,
                                indID = 'nkr_rygg_durarift_prolaps',
                                filUt = 'ind5_Durarift prolaps')

# Andel pasienter som fikk durarift etter kirurgi for lumbal spinal stenose siste 2 år.
# Andel pasienter som fikk durarift etter kirurgi for lumbal spinal stenose siste 2 år, elektive pasienter, ikke tidligere ryggopererte.
# ØNSKET MÅLNIVÅ: ≤ 3,0 %
ind6 <- dataTilOffVisning(RegData = RyggData, valgtVar='peropKompDura',
                                hovedkat=9, tidlOp=4, hastegrad=1,
                          aar = valgteAar,
                          slaaSmToAar=1,
                                indID = 'nkr_rygg_durarift_stenose',
                                filUt = 'ind6_Durarift_stenose')

#Ventetid, operasjon bestemt til utført
# Ventetid på kirurgi
# Ventetid < 3 måneder fra ryggkirurgi ble bestemt (ved spesialist poliklinikk) til operasjonen ble utført.
# ØNSKET MÅLNIVÅ: ≥ 80 %
ind8 <- dataTilOffVisning(RegData = RyggData, valgtVar='ventetidSpesOp',
                          aar = valgteAar,
                          slaaSmToAar=0,
                          hastegrad=1,
                          indID = 'nkr_rygg_ventetid_kirurgi', filUt = 'ind8_VentetidOperasjon')

#-----Oswestry---
# Forbedring av fysisk funksjon i dagliglivet, prolapskirurgi
# Andel som oppnår 20 prosentpoeng forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter prolapskirurgi
# ØNSKET MÅLNIVÅ: ≥ 69 %
#! SPM - for 2021, skal vi vise de som svarte i 2021
ind9 <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr20',
                          hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                          aar = valgteAar,
                          slaaSmToAar=1,
                                indID = 'nkr_rygg_odi20p12mnd_prolaps', filUt = 'ind9_OswEndr20poengPro')
# Forbedring av fysisk funksjon i dagliglivet, spinal stenose kirurgi
# Andel som oppnår 30 % forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter kirurgi for spinal stenose
# ØNSKET MÅLNIVÅ: ≥ 67 %
#! Skal vise de som svarte i 2021
ind10 <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr30pst',
                                hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                           aar = valgteAar,
                           slaaSmToAar=1,
                                indID = 'nkr_rygg_odi30pst12mnd_stenose', filUt = 'ind10_OswEndr30pstSS')

FellesFil <- rbind(ind1, ind2, ind3, ind4, ind5, ind6, ind8, ind9, ind10) #ind7,
write.table(FellesFil, file = 'NKRryggKvalInd.csv', sep = ';', row.names = F)
table(FellesFil$ind_id, FellesFil$year)

SS <- RyggUtvalgEnh(RegData = RyggData, hovedkat = 9)$RegData
table(SS$Aar)
Pro <- RyggUtvalgEnh(RegData = RyggData, hovedkat = 1)$RegData
table(Pro$Aar)

#Alle sykehus og resh:
ShResh <- unique(RyggData[c('ReshId', 'ShNavn')])
write.table(ShResh, file = 'RyggShResh.csv', sep = ';', row.names = F)


#---Nøkkelinformasjon, ------
#---- R Y G G

RyggData <- RyggPreprosess(
  RegData=RyggRegDataSQLV2V3())
RyggData1aar <- RyggUtvalgEnh(RegData = RyggData, datoFra = datoFra1aar, datoTil = datoTil)$RegData
Dum <- RyggUtvalgEnh(RegData = RyggData, aar=rappAar)$RegData

FornoydData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                   valgtVar = 'fornoydhet', ktr = 1, figurtype = 'andelGrVar')$RegData
BedreData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                   valgtVar = 'nytte', ktr = 1, figurtype = 'andelGrVar')$RegData
VerreData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                 valgtVar = 'verre', ktr = 1, figurtype = 'andelGrVar')$RegData
VentetidKirData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                 valgtVar = 'ventetidSpesOp', ktr = 1, figurtype = 'andelGrVar')$RegData

NokkeltallRygg <- rbind(
  'Antall avdelinger' = length(unique((RyggData1aar$ShNavn))),
  'Antall operasjoner' = dim(RyggData1aar)[1],
  'Svart på oppfølging, 3 mnd.' = mean(RyggData1aar$Ferdigstilt1b3mnd==1, na.rm=T),
  'Andel over 70 år'	= mean(RyggData1aar$Alder>=70, na.rm=T),
  'Gjennomsnittsalder' = mean(RyggData1aar$Alder, na.rm=T),
  'Andel kvinner' = 1-mean(RyggData1aar$ErMann, na.rm=T),
  'Fornøyd med behandlingen, 3 mnd. etter' = mean(FornoydData$Variabel),
  'Helt restituert/mye bedre, 3 mnd. etter' = mean(BedreData$Variabel),
  'Verre 3 mnd. etter' = mean(VerreData$Variabel),
  'Ventet <3 mnd fra operasjon bestemt til kirurgi utført' = mean(VentetidKirData$Variabel)
)

tabNokkeltallRygg <- cbind(row.names(NokkeltallRygg),NokkeltallRygg)
write.table(tabNokkeltallRygg, file = 'NokkeltallRygg.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )

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
(startAar:rappAar)
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


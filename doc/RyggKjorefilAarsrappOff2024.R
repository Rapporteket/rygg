#Generere filer og tall til årsrapport for 2024.
library(rygg)
library(xtable)
setwd('../Aarsrapp/NKR/')
Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

#Felles parametre:
startAar <- 2011
rappAar <- 2024
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

RyggData <- RyggRegDataSQLV2V3(datoFra = '2007-01-01', alleVarV3 = 0)
#ReshSh <- unique(RyggData[,c('SykehusNavn', 'AvdRESH')])
#ReshSh <- unique(ReshSh[order(ReshSh$SykehusNavn), ])
#Lagt til i preprosseser:
#RyggData$SykehusNavn[which(RyggData$AvdRESH %in% c(999975, 107511))] <- 'Aleris Oslo'

RegData <- RyggPreprosess(RegData=RyggData)
RegData <- RyggUtvalgEnh(RegData=RegData, datoTil=datoTil)$RegData
Ntot07 <- dim(RegData)[1]
# table(RegData[,c('Aar', "Ferdigstilt1b12mnd")])
# table(RegData[,c('Aar', "Ferdigstilt1b3mnd")])

#Datasjekk
SykehusNavnResh <- unique(RegData[ ,c('SykehusNavn', 'ReshId')])
ShNavnResh <- unique(RegData[ ,c('ShNavn', 'ReshId')])
write.csv2(ShNavnResh[order(ShNavnResh$ShNavn), ], file = 'RyggSykehusNavnAVDResh.csv', row.names = F, fileEncoding = 'latin1')


#Gjør utvalg/tilrettelegge årsfiler
RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)$RegData
RegData1aar <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
#write.table(RegData, file = 'RyggAarsrapp2023.csv', sep = ';', row.names = F, fileEncoding = 'latin1', na = '')

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]
AntAvd <- length(unique(RegData$ShNavn))

#---------FIGURER, årsrapport --------------
#Registreringsforsinkelse

dum <- RyggFigAndeler(RegData=RegData1aar, valgtVar='regForsinkelse', datoFra=datoFra1aar,
                            outfile='RegForsinkelseFord.pdf')
dum <-  RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='regForsinkelse', Ngrense = 10,
                               outfile='RegForsinkelseSh.pdf')
dum <- RyggFigAndelTid(RegData=RegData, valgtVar='regForsinkelse',  outfile='RegForsinkelseTid.pdf')

#Responsrate
dum <-  RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='oppf3mnd', Ngrense = 10,
                            outfile='Oppf3mndSh.pdf')
dum <- RyggFigAndelTid(RegData=RegData, valgtVar='oppf3mnd',  outfile='Oppf3mndTid.pdf')


RyggFigAndelerGrVar(RegData = RegData1aar, preprosess = 0, valgtVar='morsmal',
                    outfile = 'Morsmal.pdf')

HoyUtdAvd <- RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='utd', Ngrense = 10,
                                 outfile='HoyUtdAvd.pdf')

UforetrygdPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='uforetrygdPre', datoFra=datoFra1aar,
                                     outfile='UforAvd.pdf')

RyggFigAndelTid(RegData=RegData, datoFra = datoFra, valgtVar='alder70', preprosess = 0,
                              outfile='Alder70.pdf')

RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdPro.pdf',
                 valgtVar='liggedogn', hovedkat = 1, valgtMaal = 'Gjsn')
RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdSS.pdf',
                 valgtVar='liggedogn', hovedkat=9, valgtMaal = 'Gjsn')


RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, preprosess = 0,
                       Ngrense=20, aar=rappAar, tidlAar=tidlAar, hastegrad=1, outfile='SympVarighUtstrAarPro.pdf')
RyggFigAndelTid(RegData=RegData, valgtVar='sympVarighUtstr', hovedkat=1, hastegrad=1, outfile='SympVarighUtstrTidPro.pdf')


#Infeksjoner ikke registrert i 2019
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat = 1, outfile='KpInf3mndProAar.pdf')
dum <- RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='kpInf3mnd', aar=rappAar, tidlAar=tidlAar,
                    Ngrense = 30, hovedkat=9, outfile='KpInf3mndSSAar.pdf')

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
# Sett 70 % på KI 3 og 4 (ODI) og la det ligge fast.
# KI 5, fusjonskirurgi: fast på 10 % 'degSponFusj1op' - vises bare i andelgrvar
# KI 6, tromboseprofylakse: fast på 10 % 'trombProfylLettKI' - vises bare i andelgrvar
# 'ventetidSpesOp'  - vises bare i andelgrvar
# 'smBePreLav',  - vises bare i andelgrvar
# 'OswEndr20', 'OswEndr30pst', - må defineres som kvalitetsindikatorer


#K1 NY2021: Ventetid fra kirurgi er besl. til utført under 3 mnd., tidstrend
RyggFigAndelTid(RegData=RegData, valgtVar='ventetidSpesOp', hastegrad=1,
                outfile='VentetidSpesOpTid.pdf')

RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='ventetidSpesOp', Ngrense = 20,
                    hastegrad=1, outfile='VentetidSpesOp_Sh.pdf')

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
RyggFigAndelTid(RegData=RegData, valgtVar='OswEndr30pstSSKI', outfile='OswEndr30pstSSTid.pdf',
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

#Ny, mars 2023
# K6 Andel som får tromboseprofylakse i forbindelse med lett ryggkirurgi.
# Spesifisering: (BlodfortynnendeFast = 0 &  ASA grad< 3 & Kjønn = 1 (mann)) & (HovedInngrepV2V3=1 eller HovedInngrepV2V3=2)
# Mål:  < landsgjennomsnittet høy måloppnåelse (grønt), ≥ landsgjennomsnittet moderat/lav (gult)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='trombProfylLettKI', preprosess = 0,
                    Ngrense=20, aar=rappAar, outfile='trombProfylLettKISh.pdf')

RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar='trombProfylLettKI',
                datoFra = '01-01-2019', outfile='trombProfylLettKITid.pdf')




#---Nøkkelinformasjon, ------
#---- R Y G G
RyggData <- RegData
RyggData1aar <- RegData1aar
RyggData <- RyggPreprosess(
  RegData=RyggRegDataSQLV2V3())
RyggData1aar <- RyggUtvalgEnh(RegData = RyggData, aar=rappAar)$RegData

FornoydData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                   valgtVar = 'fornoydhet', ktr = 1, figurtype = 'andelGrVar')$RegData
BedreData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                   valgtVar = 'nytte', ktr = 1, figurtype = 'andelGrVar')$RegData
VerreData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                 valgtVar = 'verre', ktr = 1, figurtype = 'andelGrVar')$RegData
VentetidKirData <- RyggVarTilrettelegg(RegData = RyggData1aar,
                                 valgtVar = 'ventetidSpesOp', ktr = 1, figurtype = 'andelGrVar')$RegData

svarpst <- 100*mean(RyggData1aar$Ferdigstilt1b3mnd==1, na.rm=T)
paste(sprintf('%.1f', svarpst), '%')

NokkeltallRygg <- rbind(
  'Antall avdelinger' = length(unique((RyggData1aar$SykehusNavn))),
  'Antall operasjoner' = dim(RyggData1aar)[1],
  'Svart på oppfølging, 3 mnd.' = paste(sprintf('%.1f', 100*mean(RyggData1aar$Ferdigstilt1b3mnd==1, na.rm=T)), '%'),
  'Andel over 70 år'	= paste(sprintf('%.1f', 100*mean(RyggData1aar$Alder>=70, na.rm=T)), '%'),
  'Gjennomsnittsalder' = sprintf('%.1f',mean(RyggData1aar$Alder, na.rm=T)),
  'Andel kvinner' = paste(sprintf('%.1f', 100*(1-mean(RyggData1aar$ErMann, na.rm=T))), '%'),
  'Fornøyd med behandlingen, 3 mnd. etter' = paste(sprintf('%.1f', 100*mean(FornoydData$Variabel)), '%'),
  'Helt restituert/mye bedre, 3 mnd. etter' = paste(sprintf('%.1f', 100*mean(BedreData$Variabel)), '%'),
  'Verre 3 mnd. etter' = paste(sprintf('%.1f', 00*mean(VerreData$Variabel)), '%'),
  'Ventet <3 mnd fra operasjon bestemt til kirurgi utført' = paste(sprintf('%.1f', 100*mean(VentetidKirData$Variabel)), '%')
)

xtable::xtable(NokkeltallRygg,
               align = c('l','r'),
               caption = paste('Nøkkeltall,', rappAar)
               )


tabNokkeltallRygg <- cbind(row.names(NokkeltallRygg),NokkeltallRygg)
write.table(tabNokkeltallRygg, file = 'NokkeltallRygg.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )

#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#---------Figurer, utdata til videre beregninger----

#RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'TidlOpAnt3Tid.pdf') #TidlOp3 <-
#RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='tidlOp3', aar=rappAar, outfile='TidlOpAnt3Sh.pdf')

UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='utd', aar = startAar:rappAar, preprosess = 0,
                                outfile='')
UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='uforetrygdPre', preprosess = 0, outfile='')
ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='erstatningPre', preprosess = 0, outfile='')

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', outfile='')
Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='utd', datoFra=datoFra1aar, datoTil=datoTil,
                            outfile='')

HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='hovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='')

FremmedSpraakAar <-  RyggFigAndelTid(RegData=RegData, valgtVar='morsmal', aar = startAar:rappAar,
                                     outfile='', preprosess = 0)



#--------------TABELLER OG TALL---------------------------
library(xtable)
RegDataPro <- RegData[which(RegData$HovedInngrep==1),]
RegDataPro12mnd <- RegDataPro[which(RegDataPro$Aar<rappAar), ]
RegDataSS <- RyggUtvalgEnh(RegData, hovedkat=9)$RegData

RegData$ODIendr <- RegData$OswTotPre-RegData$OswTot12mnd
RegData$ODIpst <- with(RegData, (OswTotPre-OswTot12mnd)/OswTotPre*100)

tabAvdN <- addmargins(table(RegData[c('SykehusNavn','Aar')]))
antKol <- ncol(tabAvdN)
tabAvdN5 <- tabAvdN[,(antKol-5):antKol]
rownames(tabAvdN5)[dim(tabAvdN5)[1] ]<- 'TOTALT, alle avd.:'
colnames(tabAvdN5)[dim(tabAvdN5)[2] ]<- paste0(min(RegData$Aar),'-',rappAar)

xtable(tabAvdN5, digits=0, align=c('l', rep('r', 6)),
       caption=paste0('Antall registreringer ved hver avdeling siste 5 år, samt totalt siden ', min(RegData$Aar, na.rm=T),'.'),
       label = 'tab:AntReg')

#Gjennomsnittsalder per år:
AlderAar <- tapply(RegData$Alder, RegData$Aar, 'mean', na.rm=T)
(startAar:rappAar)
(AlderAar <- sprintf('%.1f', AlderAar))
#Andel over 70 år:
Alder70Aar <- RyggFigAndelTid(RegData=RegData, datoFra = datoFra, valgtVar='alder70', preprosess = 0, lagFig = 0)
Andel70 <- as.numeric(sprintf('%.1f', Alder70Aar$AggVerdier$Hoved))
names(Andel70) <- as.character(startAar:rappAar)
Andel70

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
ArbNum <- round(table(RegData1aar$ArbstatusPreV3)*100/sum(table(RegData1aar$ArbstatusPreV3)), 1)
ArbNum[1]

#Andel pasienter svart på spørsmål om arbeidsstatus, årsrapportåret: \% 22: 94,1
  NsvarArb <- sum(RegData1aar$ArbstatusPreV3 %in% 1:9)
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


#------------------------------------------------------------------------------------
#-----------Filer til Interaktive nettsider -----------------------
#------------------------------------------------------------------------------------
# Følgende indikatorer (med markering for de som finnes i csv-fila) er gyldige for
# Nasjonalt kvalitetsregister for ryggkirurgi – ryggkirurgi:
#   nkr_rygg_degSponFusj1op, nkr_rygg_dg, nkr_rygg_durarift_prolaps,
# nkr_rygg_durarift_stenose, nkr_rygg_lav_bensmerte_prolaps, nkr_rygg_odi20p12mnd_prolaps, nkr_rygg_odi30pst12mnd_stenose, nkr_rygg_saarinfeksjon_prolaps, nkr_rygg_saarinfeksjon_stenose, nkr_rygg_trombProfylLettKI, nkr_rygg_varighet_bensmerter, nkr_rygg_ventetid_kirurgi.
library(rygg)
library(magrittr)
Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
RyggData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())
valgteAar <- 2011:2025
#Ønsker å vise alle data
RyggData <- RyggUtvalgEnh(RegData=RyggData, datoFra = '2011-01-01')$RegData

#Sjekk om nye resh:
nyresh <- setdiff(sort(unique(RyggData$ReshId)), sort(names(nyID)))
RyggData$ShNavn[match(nyresh, RyggData$ReshId)]
table(RyggData$Aar[RyggData$ReshId == nyresh])

setwd('../Aarsrapp/NKR/')
#Ventetid, operasjon bestemt til utført
# Ventetid på kirurgi
# Ventetid < 3 måneder fra ryggkirurgi ble bestemt (ved spesialist poliklinikk) til operasjonen ble utført.
# ØNSKET MÅLNIVÅ: ≥ 80 %
ind1 <- dataTilOffVisning(RegData = RyggData, valgtVar='ventetidSpesOp',
                          aar = valgteAar,
                          slaaSmToAar=0,
                          hastegrad=1,
                          indID = 'nkr_rygg_ventetid_kirurgi', filUt = 'ind1_VentetidOperasjon')

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

#-----Oswestry---
# Forbedring av fysisk funksjon i dagliglivet, prolapskirurgi
# Andel som oppnår 20 prosentpoeng forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter prolapskirurgi
# ØNSKET MÅLNIVÅ: ≥ 70 %
#! Skal vise de som svarte i rapporteringsåret. Dette er tatt hånd om i funksjonen når velger ktr=2
ind3 <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr20',
                          hovedkat=1, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                          aar = valgteAar,
                          slaaSmToAar=1,
                          indID = 'nkr_rygg_odi20p12mnd_prolaps', filUt = 'ind3_OswEndr20poengPro')
# Forbedring av fysisk funksjon i dagliglivet, spinal stenose kirurgi
# Andel som oppnår 30 % forbedring av Oswestry Disabiliy Index (ODI) 12 måneder etter kirurgi for spinal stenose
# ØNSKET MÅLNIVÅ: ≥ 70 %
#! Skal vise de som svarte i rapporteringsåret. Dette er tatt hånd om i funksjonen når velger ktr=2
ind4 <- dataTilOffVisning(RegData = RyggData, valgtVar='OswEndr30pst',
                          hovedkat=9, hastegrad = 1, tidlOp = 4, ktr=2, #Skal være utvalg både på elektiv og ikke tidl.op
                          aar = valgteAar,
                          slaaSmToAar=1,
                          indID = 'nkr_rygg_odi30pst12mnd_stenose', filUt = 'ind4_OswEndr30pstSS')

# Kl 5. Andel pasienter med degenerativ spondylolistese som blir operert med
# fusjonskirurgi ved første operasjon
# RyggFigAndelerGrVar(RegData=RegData, valgtVar='degSponFusj1op', preprosess = 0,
#                     Ngrense=20, aar=(rappAar-1):rappAar, outfile='degSponFusj1opKISh.pdf')

ind5 <- dataTilOffVisning(RegData = RyggData, valgtVar='degSponFusj1op',
                          aar = valgteAar,
                          slaaSmToAar=1,
                          indID = 'nkr_rygg_degSponFusj1op', filUt = 'ind5_degSponFusj1op')

# Andel pasienter med degenerativ spondylolistese som blir operert med
# fusjonskirurgi ved første operasjon
# Mål: 	høy	≤ 10%
# Hensikt: 	Redusere andel pasienter med degenerativ spondylolistese som blir operert med fusjonskirurgi ved første operasjon


# K6 Andel som får tromboseprofylakse i forbindelse med lett ryggkirurgi.
# Spesifisering: (BlodfortynnendeFast = 0 &  ASA grad< 3 & Kjønn = 1 (mann)) & (HovedInngrepV2V3=1 eller HovedInngrepV2V3=2)
# RyggFigAndelerGrVar(RegData=RegData, valgtVar='trombProfylLettKI', preprosess = 0,
#                     Ngrense=20, aar=rappAar, outfile='trombProfylLettKISh.pdf')

ind6 <- dataTilOffVisning(RegData = RyggData, valgtVar='trombProfylLettKI',
                          aar = valgteAar,
                          slaaSmToAar=0,
                          indID = 'nkr_rygg_trombProfylLettKI', filUt = 'ind6_trombProfylLettKI')

#Kl 6. 		Andel som får tromboseprofylakse i forbindelse med lett ryggkirurgi
# Mål: 	< landsgjennomsnittet høy måloppnåelse (grønt), ≥ landsgjennomsnittet moderat/lav (gult). 2023: 10%
# Hensikt: 	Økt etterlevelse av nasjonale retningslinjer for bruk av tromboseprofylakse ved å redusere andelen som får slik profylakse i forbindelse med lett ryggkirurgi, der det ikke er anbefalt
# Datakilde: 	NKR, legeskjema perioperativt

FellesFil <- rbind(ind1, ind2, ind3, ind4, ind5, ind6) #ind7,
write.table(FellesFil, file = 'NKRryggKvalInd.csv', sep = ';', row.names = F)
table(FellesFil$ind_id, FellesFil$year)




SS <- RyggUtvalgEnh(RegData = RyggData, hovedkat = 9)$RegData
table(SS$Aar)
Pro <- RyggUtvalgEnh(RegData = RyggData, hovedkat = 1)$RegData
table(Pro$Aar)

#Alle sykehus og resh:
ShResh <- unique(RyggData[c('ReshId', 'ShNavn')])
write.table(ShResh, file = 'RyggShResh.csv', sep = ';', row.names = F)

#-------Dekningsgrad-------------------
ReshShNavn <- unique(RegData[ , c("ReshId", "ShNavn")])
write.csv2(ReshShNavn, file = 'data-raw/RyggReshSh.csv', row.names = F)

#NakkeData <- nakke::NakkePreprosess(RegData = nakke::NakkeRegDataSQL())
#ReshShNavnNakke <- unique(NakkeData[ , c("ReshId", "ShNavn")])
#write.csv2(ReshShNavnNakke, file = '~/nakke/data-raw/NakkeReshSh.csv', row.names = F)

#Rygg:
ReshSh <- read.csv2('data-raw/RyggReshSh.csv', encoding = 'UTF-8')
RyggDg <- readxl::read_excel('C:/Registerinfo/DeknGrad/NKR2023/DekningsgraderRygg2023.xlsx', sheet = 'RyggFig')
RyggDgSh <- aggregate(RyggDg[ ,c("RegRygg", 'Total')], by = list(RyggDg$ReshId), FUN = 'sum')
RyggDgSh$ShNavn <- ReshSh$ShNavn[match(RyggDgSh$Group.1, ReshSh$ReshId)]
#RyggDgSh$DG_nkr <- 100*RyggDgSh$RegRygg/RyggDgSh$Total

RyggFigAndelerGrVar(RegData=RyggDgSh, valgtVar='dekn23Rygg', outfile='DGrygg.pdf')

#Nakke:
ReshSh <- read.csv2('../nakke/data-raw/NakkeReshSh.csv', encoding = 'UTF-8')
NakkeDg <- readxl::read_excel('C:/Registerinfo/DeknGrad/NKR2023/DekningsgraderNakke2023.xlsx', sheet = 'NakkeFig')
NakkeDg$ShNavn <- ReshSh$ShNavn[match(NakkeDg$ReshId, ReshSh$ReshId)]
#table(NakkeDg$ShNavn)
rygg::RyggFigAndelerGrVar(RegData=NakkeDg, valgtVar='dekn23Nakke', outfile='DGnakke.pdf') #

#Data til nettsider (legge på orgnr: nyID)
DataDgOrg <- RyggDg
DataDgOrg$orgnr <- as.character(nyID[as.character(DataDgOrg$ReshId)])
DataDgOrg$ind_id <- 'nkr_rygg_dg' #'nakke_dg'
#Variabler: year, orgnr, var, denominator, ind_id
DataDgOrg <- dplyr::rename(DataDgOrg, var=RegNKR, denominator=Total, )
DataDgOrg$context <- 'caregiver'
DataDgOrg$year <- 2023
DataDgOrg <- DataDgOrg[ ,-which(names(DataDgOrg) %in% c('ReshId', "Sykehus"))]
write.csv2(DataDgOrg, file = 'RyggDg2023.csv')




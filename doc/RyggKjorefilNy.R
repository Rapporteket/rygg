
devtools::install_github("Thinkr-open/golem", ref='master')
devtools::install_github("Rapporteket/rapbase", ref='rel')

#------------------------------------
#Sjekk
library(rygg)
library(magrittr)
RegData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())
forsinketReg(RegData=RegData, fraDato=Sys.Date()-400,
             tilDato=Sys.Date()-100, forsinkelse=100, reshID=601161)

#----Sjekk av Ferdig1b3mnd----

forl3mndIkkeFunnet <- c(16811, 17244, 17947, 18187, 22301, 16622)

tab <- ePROMadmTab[which(ePROMadmTab$MCEID %in% forl3mndIkkeFunnet),
            c("MCEID", "TSSENDT", "TSRECEIVED", "NOTIFICATION_CHANNEL", "DISTRIBUTION_RULE",
              'REGISTRATION_TYPE', 'STATUS')]
tab[tab$REGISTRATION_TYPE %in% c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'), ]

#----Sjekk av utfyltdato----
setwd('/home/rstudio/speil/aarsrapp/Rygg')
RegData <- RyggPreprosess(RegData = RyggRegDataSQLV2V3(datoFra = '2010-01-01'))
RegData$Diff3 <- as.integer(difftime(RegData$Utfdato3mnd, RegData$InnDato, units = 'days'))
RegData$Diff12 <- as.integer(difftime(RegData$Utfdato12mnd, RegData$InnDato, units = 'days'))

ind3 <- which(RegData$Diff3 < 70)
feil3 <- RegData[ind3, c("ForlopsID", "ShNavn", 'InnDato',  "Utfdato3mnd", "Diff3")]
write.table(feil3, file="Feil3mnd.csv", sep=';')

ind12 <- which(RegData$Diff12 < 300)
feil12 <- RegData[ind12, c("ForlopsID", "ShNavn", 'InnDato',  "Utfdato12mnd", "Diff12")]
write.table(feil12, file="Feil12mnd.csv", sep=';')

#V2 og V3

RegDataV2 <- rapbase::loadRegData(registryName="rygg",
                                  query='SELECT * FROM Uttrekk_Rapport_FROM_TORE')
#Legg til ledende 0 i V2
indUten0 <- which(nchar(RegDataV2$Personnummer)==10)
RegDataV2$Personnummer[indUten0] <- paste0(0,RegDataV2$Personnummer[indUten0])

PIDtab <- rapbase::loadRegData(registryName="rygg", query='SELECT * FROM koblingstabell')

length(intersect(unique(sort(PIDtab$SSN)),unique(sort(RegDataV2$Personnummer))))
head(RegDataV2$Personnummer)

5266/6321
#-------------- Laste fil og evt. lagre -------------
rm(list=ls())
setwd('C:/ResultattjenesteGIT/nkr')

reshID <- 601161 #Haukeland nevr.kir: 105588, NIMI:  104279, Unn: 601161, St Olav: 105783 Namsos:105899,
#fil <- 'A:/Rygg/NKR2010-2017aarsrapp'
dato <- '2019-08-21'
#RegData <- read.table(paste0(fil, '.csv'), sep=';', header=T, encoding = 'UTF-8') #, stringsAsFactors = FALSE, na.strings = "NULL",
SkjemaOversikt <- read.table(paste0('A:/Rygg/SkjemaOversikt',dato,'.csv'),
                             sep=';', header=T, encoding = 'UTF-8') #IKKE sensitive data. Kan legges i pakken.
#usethis::use_data(SkjemaOversikt, internal = TRUE, overwrite = TRUE)
ForlopsOversikt <- read.table(paste0('A:/Rygg/ForlopsOversikt',dato,'.csv'),
                             sep=';', header=T, encoding = 'UTF-8')
RegData <- read.table(paste0('A:/Rygg/AlleVarNum',dato,'.csv'),
                             sep=';', header=T, encoding = 'UTF-8')
save(SkjemaOversikt, ForlopsOversikt, RegData, file = 'A:/Rygg/RyggData.RData')
#RegData$Kjonn <- 0

#save(RegData, file=paste0(fil, '.Rdata'))
load(file=paste0(fil, '.Rdata'))
#load('a:/Rygg/Rygg2010-2018aarsrapp.Rdata')

#Versjon 3
#Lage tulledata:
#RegData <- RegData[sample(1:dim(RegData)[1],10000), ]
load('A:/Rygg/RyggData.RData')

RegData <- RyggRegDataSQLV2V3()


#--------------Kompletthet---------------------------

# RyggDataRaa <- read.table('C:/Registerdata/Rygg/RyggdataDump2022-12-09fra2018.csv',
#                           sep=';', header=T, encoding = 'UTF-8')
RyggDataRaa <- read.table('C:/Registerdata/Rygg/RyggdataDump2022-12-09fra2018.csv',
                      sep=';', header=T, encoding = 'latin1', dec = ',')
#RyggDataRaa <- RyggDataRaa[-which(RyggDataRaa$ForlopsID == 8274),]
RyggData <- RyggPreprosess(RyggDataRaa)

Variabler <- c('AntibiotikaV3',	'ASA',	'TidlOpr',	'OpKat',	'BMI',	'SympVarighUtstr',
               'SmRyPre',	'SmBePre',	'SmStiPre',	'ArbstatusPre')


tab <- prop.table(table(RyggData[,c('Aar','AntibiotikaV3')], useNA = 'a'), margin=1)
AntibiotikaV3 <- 1-tab[1:4,'9']

tab <- prop.table(table(RyggData[,c('Aar','ASA')], useNA = 'a'), margin=1)
ASA <- 1-tab[1:4,'9']

tab <- prop.table(table(RyggData[,c('Aar','TidlOpr')], useNA = 'a'), margin=1)
TidlOpr <- 1-tab[1:4,'9']

tab <- prop.table(table(RyggData[,c('Aar','OpKat')], useNA = 'a'), margin=1)
OpKat <- 1-tab[1:4,'9'] #Ser bort fra NA..

data <- data.frame(Aar=RyggData$Aar, BMI=is.na(RyggData$BMI))
tab <- prop.table(table(data, useNA = 'a'), margin=1)
BMI <- 1-tab[1:4,'TRUE']

tab <- prop.table(table(RyggData[,c('Aar','SympVarighUtstr')], useNA = 'a'), margin=1)
SympVarighUtstr <- 1-rowSums(tab[1:4, 6:7])

tab <- prop.table(table(RyggData[,c('Aar','SmRyPre')], useNA = 'a'), margin=1)
SmRyPre <- 1-tab[1:4,'99']

tab <- prop.table(table(RyggData[,c('Aar','SmBePre')], useNA = 'a'), margin=1)
SmBePre <- 1-tab[1:4,'99']

tab <- prop.table(table(RyggData[,c('Aar','SmStiPre')], useNA = 'a'), margin=1)
SmStiPre <- 1-rowSums(tab[1:4,3:4])

tab <- prop.table(table(RyggData[,c('Aar','ArbstatusPreV3')], useNA = 'a'), margin=1)
ArbstatusPreV3 <- 1-tab[1:4,'99']

Kompletthet <- cbind(AntibiotikaV3,	ASA,	TidlOpr,	OpKat,	BMI,	SympVarighUtstr,
  SmRyPre,	SmBePre,	SmStiPre,	ArbstatusPreV3)

write.csv2(Kompletthet, file = 'c:/Registerdata/rygg/RyggKompl.csv', fileEncoding = 'UTF-8')

#_________________________________________________________________________________________
#Registreringsoversikter for 2019-data
#_________________________________________________________________________________________

SkjemaOversikt <- read.table('A:\Rygg\SkjemaOversikt2019-11-04.csv',
                             sep=';', header=T, encoding = 'UTF-8') #IKKE sensitive data. Kan legges i pakken.
#SkjemaOversikt$Skjemanavn <- SkjemaOversikt$X.U.FEFF.Skjemanavn
SkjemaOversikt$MndAar <- format(as.Date(SkjemaOversikt$HovedDato), '%y.%m')
table(SkjemaOversikt$MndAar)

indPasientskjema <- which((SkjemaOversikt$SkjemaRekkeflg==5) & SkjemaOversikt$SkjemaStatus %in% -1:1)
table(SkjemaOversikt[indPasientskjema, c('Sykehusnavn','MndAar', "SkjemaStatus")]) # ,


#----------- Teste valgtVar for nye data -----------------------------
#Benytter AlleVarNum som RegData
#Tester Andeler, AndelTid, AndelGrVar, GjsnBox og GjsnGrVar
#Oppfølgingsskjema har ikke fått selvvalgte navn. Variable fra oppfølgingsskjema sjekkes IKKE

library(rygg)

tapply(RegData$OswTotPre, RegData$Aar, FUN = 'median')


rm(list=ls())
outfile <- ''
tidsenhet <- 'Mnd'
valgtVar <- 'regForsinkelse'

# RegData <- read.table('A:/Rygg/AlleVarNum2019-08-12.csv',
#                       sep=';', header=T, encoding = 'UTF-8')
RegData <- RyggRegDataSQLV2V3()
test <- RyggFigAndeler(RegData=RegData)  #, valgtVar=valgtVar, enhetsUtvalg = 1, reshID = reshID) #, outfile='test.pdf')
utdata <- RyggFigAndelerGrVar(RegData=RegData) #, valgtVar=valgtVar,outfile=outfile)
utdata <- RyggFigAndelTid(RegData=RegData, valgtVar='roker', tidsenhet = 'Aar') #, outfile=outfile
RyggFigGjsnBox(RegData=RegData, outfile=outfile, valgtVar=valgtVar,tidsenhet = tidsenhet) #aar=aar,
RyggFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar)

#---Status per 19.juli 2019
#valgtVarOK:  alder, alder70, arbstatus, ASA, beinsmLavPre, BMI, degSponSSSten, EQ5DPre, OswTotPre,
#   smBePre, smRyPre, EQangstPre, EQgangePre, erstatningPre, komplPer, morsmal, opIndPareseGrad, hastegrad
#   peropKomp, roker, saardren, sivilStatus, smStiPre, smStiPreHypp, symptVarighRyggHof, sympVarighUtstr,
#   uforetrygdPre, utd
variable <- c('alder', 'alder70', 'arbstatus', 'ASA', 'beinsmLavPre', 'BMI', 'degSponSSSten',
              'EQ5DPre', 'OswTotPre', 'smBePre', 'smRyPre', 'EQangstPre', 'EQgangePre',
              'erstatningPre', 'komplPer', 'morsmal', 'opIndPareseGrad', 'hastegrad', 'peropKomp',
              'roker', 'saardren', 'sivilStatus', 'smStiPre', 'smStiPreHypp', 'symptVarighRyggHof',
              'sympVarighUtstr', 'uforetrygdPre', 'utd')

for (valgtVar in variable) {
  outfile <- paste0(valgtVar,'_ford.png')
  RyggFigAndeler(RegData=RegData, valgtVar=valgtVar, outfile = outfile)}

#valgtVarIKKE ok: antNivOpr, beinsmEndrLav, degSponFusj, EQ5DEndr, EQ5DEndrPre, OswEndr, OswEndrPre,
#   OswEndrLav, OswEndr20, OswEndr30pst, Osw22, Osw48, smBeinEndr, smBeinEndrPre, smRyggEndr, smRyggEndrPre
# fornoydhet, hovedInngrep (ikke def.), komplPost, kp3Mnd, kpInf3Mnd, liggedogn, misfornoyd, nytte,
#  opInd og opIndSmeType (mangler varnavn OpIndSme/-Type),  radUnders (mangler varnavn),
#  tidlOpr, tidlOprAntall, tidlOp3, underkat (ikke def. hovedkat), verre


#_________________________________________________________________________________________
#
#""""""""""""""""""""""""""""" F I G U R F U N K S J O N E R """""""""""""""""""""""""""""""""""""
#_________________________________________________________________________________________

#-----------Samlerapporter----------------------------
knit('SamleRappNKR.Rnw') #, encoding = 'UTF-8')
texi2pdf('SamleRappNKR.tex')

reshID <- 601161
library(rygg)
setwd('/home/rstudio/rygg/inst')
knitr::knit('RyggMndRapp.Rnw')
tools::texi2pdf('RyggMndRapp.tex')
#knitr::knit2pdf('RyggMndRapp.Rnw')

library(knitr)
knit('C:/ResultattjenesteGIT/nkr/AarsrappOff/ResultaterAarsrapp.Rnw') #, encoding = 'UTF-8')
library(tools)
texi2pdf('ResultaterAarsrapp.tex')

#---------------------------Dekningsgrad-------------------------------------
RegData=0
preprosess=0
outfile=''
valgtVar='DeknNakke17' #DeknNakke17, 'DeknRygg17'
#RyggFigAndelerGrVarDeknGr(RegData=0, preprosess=0, outfile='', valgtVar='DeknNakke17')
RyggFigAndelerGrVarDeknGr(RegData=RegData, outfile='', valgtVar='SymptVarighRyggHof')
RyggFigAndelerGrVar(RegData = RegData, valgtVar='Morsmal', outfile = '')

#----------------------------------------------------------------
#			FigAndeler (RyggFigAndeler.r
#----------------------------------------------------------------
valgtVar <- 'fornoydhet'	#Må velge...
#NB: Hvis variabel='Underkat', MÅ hovedkat velges, dvs. ikke 99.
outfile <- 'FordelingsFigurregForsinkelse_2021-03-27.pdf'
FordUt <- RyggFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
		minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat, preprosess=1,
		 enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

 variable <- c('alder', 'antibiotika', 'antNivOpr', 'arbstatus', #'Arbstatus3mnd', 'Arbstatus12mnd',
               'ASA', 'BMI', 'EQangstPre', 'EQgangePre', 'erstatningPre', 'fornoydhet',  'hovedInngrep',
              'komorbiditet', 'komplPer', 'komplPost', 'liggedogn', 'morsmal', 'nytte', #3mnd', 'Nytte12mnd',
              'opIndPareseGrad', 'opInd', 'opIndSmeType', 'hastegrad','radUnders',
              'roker', 'sivilStatus','smStiPre', 'smStiPreHypp', 'SymptVarighRyggHof',
              'SympVarighUtstr', 'saardren', 'tidlOpr', 'tidlOprAntall','uforetrygdPre', 'utd', 'underkat')
 variable <- c('SympVarighUtstr', 'SymptVarighRyggHof','saardren' )

for (valgtVar in variable) {
      print(valgtVar)
      outfile <- paste0(valgtVar, '.png')
      utdata <- RyggFigAndeler(RegData <- RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,
                           outfile = outfile, reshID=reshID)
}

 #----------------------------------------------------------
 #                   RyggFigGjsnGrVar
 #                   RyggFigGjsnBox
 #----------------------------------------------------------
 #------- Endring i effektmål som funksjon av tid eller prescore
 library(rygg)
 RegData <- RyggRegDataSQLV2V3()

 RyggFigGjsnGrVar(RegData=RegData, outfile='', valgtVar='OswTotPre')
 #, tidlOp=tidlOp, erMann=erMann,
 #                 hovedkat=hovedkat, minald=minald, maxald=maxald, ktr=ktr, tittel=tittel, valgtMaal='Gjsn',
 #                 datoFra=datoFra, datoTil=datoTil, aar=aar, enhetsUtvalg=enhetsUtvalg)
 utdata <- RyggFigGjsnBox(RegData=RegData, outfile='', aar=2007:2020, tidsenhet = 'Aar', valgtVar='OswEndr')
 # , tidlOp=tidlOp, erMann=erMann, hastegrad = hastegrad,
 #                          hovedkat=hovedkat, minald=minald, maxald=maxald, ktr=ktr, tittel=tittel, valgtMaal=valgtMaal,
 #                          datoFra=datoFra, datoTil=datoTil, enhetsUtvalg=enhetsUtvalg, tidsenhet = tidsenhet, reshID=reshID) #aar=aar,

variable <- c('alder', 'liggedogn', 'EQ5DPre', 'EQ5DEndr', 'EQ5DEndrPre',
              'OswTotPre', 'smBePre', 'smRyPre', 'OswEndrPre', 'smRyggEndr', 'smBeinEndr')

for (var in variable) {
  # outfile <- paste0(var, 'GjsnSh.png')
  # RyggFigGjsnGrVar(valgtVar=var, RegData=RegData, datoFra='2018-01-01', ktr=1,
  #                 medKI = 0,  outfile=outfile)
  (outfile <- paste0(var, 'MedBox.png'))
      RyggFigGjsnBox(valgtVar=var, RegData=RegData, datoFra='2017-01-01', #valgtMaal = 'Med',
                     tidsenhet = 'Kvartal', outfile=outfile)
}


#----------------------------------------------------------------------------------------------------------------
#-------- Andel per sykehus eller annen gr.variabel (AndelGrVar)-----------------------------------------
#---------AndelTid
#-------------------------------------------------------
RegData <- RyggRegDataSQLV2V3() #read.table('A:/Rygg/AlleVarNum2019-08-12.csv', sep=';', header=T, encoding = 'UTF-8')


DataUt <- RyggFigAndelerGrVar(valgtVar='fornoydhet', RegData=RegData, #hovedkat = hovedkat, tidlOp=tidlOp,  Ngrense=20, hastegrad=hastegrad,
                    datoFra='2019-01-01', datoTil = '2021-02-01',ktr=1, outfile='')

DataUt <- RyggFigAndelTid(RegData=RegData, outfile='', valgtVar='nytte', datoFra = '2017-01-01', ktr = 1)
tab <- lagTabavFig(UtDataFraFig=DataUt, figurtype='andelTid')

# , hovedkat=hovedkat, preprosess=1,
#                 minald = minald, maxald = maxald, aar=aar, tidsenhet = tidsenhet,
#                 erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1,
#                 reshID=reshID)
#Sjekk Oswestry-skår for V3
 variable <- c('alder70', 'antibiotika', 'arbstatus', 'ASA', 'smBePreLav',
              'beinsmEndrLav', 'BMI', 'degSponFusj', 'degSponSSSten', 'erstatningPre', 'fornoydhet',
              'KpInf3Mnd', 'Kp3Mnd', 'misfornoyd', 'nytte', 'OswEndrLav', 'OswEndr20', 'OswEndr30pst',
              'Osw22', 'Osw48', 'peropKompDura', 'roker', 'saardren', 'smStiPre', 'symptVarighRyggHof',
              'sympVarighUtstr', 'tidlOp3', 'uforetrygdPre', 'utd', 'verre')
for (var in variable) {
      # outfile <- paste0(var, 'Sh.png')
      # RyggFigAndelerGrVar(valgtVar=var, RegData=RegData, datoFra='2016-01-01', ktr=1, outfile=outfile)
      outfile <- paste0(var, 'ATid.png')
      RyggFigAndelTid(valgtVar=var, RegData=RegData, datoFra='2011-01-01', ktr=1, outfile=outfile)
}

#---------Traktplott------
library(rapFigurer)
library(qicharts2)
RegData <- read.table('A:/Rygg/AlleVarNum2019-08-12.csv',
                      sep=';', header=T, encoding = 'UTF-8')
AndelGrVarData <- RyggFigAndelerGrVar(valgtVar='uforetrygdPre', RegData=RegData, Ngrense = 20,
                                      datoFra='2019-01-01', outfile='')
with(AndelGrVarData, rapFigurer::FigTraktplott(sykehus = grtxt, andel = as.numeric(AggVerdier/100),
                                               nevner = as.numeric(Ngr),
                                               tittel = tittel, undertittel = utvalgTxt, sort = 'andel'))


#----------------------------------------------------------------------------------------------------------------
#-------- Andel per sykehus eller annen gr.variabel (AndelGrVar), samt siste 3 år-----------------------------------------
#----------------------------------------------------------------------------------------------------------------
#Lage bostedfil:
Innbyggere2007_2015kjonn <- read.table('./Innbyggere2007_2015kjonn.csv', sep=';', header = T, encoding = 'UTF-8')
BoStederInnb <- aggregate(Innbyggere2007_2015kjonn$AntInnb,
                          by=Innbyggere2007_2015kjonn[ ,c('Kommune','KommNr', 'BoRHF','Fylke', 'Aar')], FUN='sum')#'BoHF',
#Kommune 301 Oslo har fire BoHF. For øvrig kommunenummer bestemmer kommunenummer entydig BoHF.
BoStederInnb2015 <- BoStederInnb[which(BoStederInnb$Aar == '2015'),]
save(BoStederInnb2015, file="./data/BostederInnb2015.Rdata")
write.table(BoStederInnb2015, file="./data/BostederInnb2015.csv", sep=';')


rm(list=ls())
setwd("C:/ResultattjenesteGIT/nkr/")
library(nkr)

NKRdata <- read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep=';', header=T)

load(file="./data/BostederInnb2015.Rdata")
#For alders-og kjønnsstandardisering:
Innbyggere2015aldkj <- read.table('./Innbyggere2015aldkj.csv', sep=';', header = T, encoding = 'UTF-8')
#Innbyggere2015alder <- read.table('C:/VariasjonKvalitet/Innbyggere2015.csv', sep=';', header = T)
#Innbyggere <- with(Innbyggere2015alder, aggregate('AntInnb', by=list('KommNr', 'BoHF', 'BoRHF','Fylke'), FUN='sum')
RegData <- merge(NKRdata, BoStederInnb2015, by.x = "Kommunenr", by.y = "KommNr", all.x = TRUE, all.y = FALSE)



valgtVar <- 'SympVarighUtstr'   #, #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten, KpInf3Mnd
                              # OswEndr13, OswEndr20, OswEndr30pst, Osw48, SympVarighUtstr, Verre,

#Indikatorprosjekt: BeinsmLavPre, OswEndr20
outfile <- paste0(valgtVar, '_', grVar,'Aar.pdf')

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
#RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, siste3aar = 1,
                minald=minald, maxald=maxald, erMann=erMann, ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

variableInd <- c('BeinsmLavPre', 'OswEndr20','SympVarighUtstr')
variable <- c('Alder', 'Antibiotika', 'ArbstatusPre', 'Arbstatus3mnd', 'Arbstatus12mnd', 'ASA', 'BMI',
             'ErstatningPre', 'Fornoyd3mnd','Fornoyd12mnd', 'Kp3Mnd', 'Misfor3mnd', 'Misfor12mnd',
          'Nytte3mnd', 'Nytte12mnd', 'PeropKomp', 'Osw30_3mnd', 'Osw30_12mnd', 'PeropKompDura', 'Roker',
          'Saardren', 'SmStiPre', 'SymptVarighRyggHof', 'SympVarighUtstr', 'UforetrygdPre', 'Utd', 'Verre3mnd', 'Verre12mnd')
#for (grVar in c('Fylke', 'ShNavn', 'BoRHF')){
grVar <- 'ShNavn' #'BoRHF'
  for (valgtVar in variableInd) {
      outfile <- paste0(valgtVar, '_', grVar, 'Aar.pdf')
      RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                        minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat,ktr=ktr,
                        preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile,
                      grVar = grVar)
}
#}

#-------------------------------------------------------
#---------AndelStabelTid
#-------------------------------------------------------
#query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Fornoyd3mnd, Fornoyd12mnd, TidlOpr,
#			Nytte3mnd, Nytte12mnd,
#			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from nkr_test.dbo.Uttrekk_Rapport'

valgtVar <- 'TidlOp'	#velge TidlOp, Fornoyd eller Nytte
outfile <- ''#paste0(valgtVar, '.png')	#Navn angis av Jasper

RyggFigAndelStabelTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, hovedkat=hovedkat, preprosess=1,
                   minald = minald, maxald = maxald,
                        erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1, reshID=reshID)
AndelTidlOp <- RyggAndelStabelTid(RegData=RegDataLand, outfile='TidlOp.pdf', valgtVar='TidlOp', hovedkat=hovedkat,
                                  minald = minald, maxald = maxald,
                                  erMann=erMann, enhetsUtvalg=enhetsUtvalg, tittel=0, reshID=reshID)


for (valgtVar in c('TidlOp', 'Fornoyd', 'Nytte')) {
      outfile <- paste0(valgtVar, '.png')
      RyggAndelStabelTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, hovedkat=hovedkat, preprosess=1,
                         erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1, reshID=reshID)
}


#----------------------------------------------------------
#-----------------AndelStabelEgetLand-----------------------------
#----------------------------------------------------------
#Variabel: ASA, OpKat(Hastegrad)
library(gridExtra)

rm(list=ls())
query = 'select ASA, OpKat, Kjonn, TidlOpr, HovedInngrep, HovedInngreptxt,
Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from nkr_test.dbo.Uttrekk_Rapport'

#__Inndata til funksjon:
reshID <- 100133	#999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
variabel <- 'OpKat'		#Må velges. 'ASA', 'OpKat'
kjonn <- 1			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
outfile <- paste(variabel, '.png', sep='')	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/RAndelStabelEgetLand/")

source("fun_AndelStabelEgetLand.R", encoding="UTF-8")
AndelStabelEgetLand(opdata=opdata, outfile=outfile, variabel=variabel,
                    kjonn=kjonn, hovedkat=hovedkat, aar=aar, reshID=reshID)


#----------------------------------------------------------
#                FordPrePost
#----------------------------------------------------------
rm(list=ls())
setwd('C:/RyggRegister/Rapport')

query = 'select PID, Kjonn, HovedInngrep, HovedInngreptxt,
		EQ5DPre, EQ5D3mnd, EQ5D12mnd, OswTotPre, OswTot3mnd, OswTot12mnd,
		SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd,
		Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'

plotType <- 'S'	#'L' el. 'S'. Søyler er standard.
valgtVar <- 'EQ5D'			#EQ5D, Oswestry, SmBein, SmeRygg

for (variabel in c('EQ5D', 'Oswestry', 'SmBein', 'SmRygg')) {
#variabel <- c(KomplPer, KomplPost)[i]
outfile <- paste(variabel,'FEs.pdf', sep='')
source("fun_FordPrePost.R", encoding="UTF-8")
FordPrePost(opdata=opdata, outfile=outfile, reshID=reshID, egenavd=egenavd,
		variabel=variabel, plotType=plotType, aar=aar, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)
}

#----------------------------------------------------------
#--------SoyleSml, differanse m/konf.int. gj.sn., alle s-hus
#----------------------------------------------------------

rm(list=ls())
query = 'select OswTotPre, OswTot3mnd, OswTot12mnd, EQ5DPre, EQ5D3mnd, EQ5D12mnd,
		Alder, Kjonn, HovedInngrep, HovedInngreptxt, TidlOpr, Inngrep, Inngreptxt,
			AvdID, AvdReshID, AvdNavn, OpDato, ASA, Utd from Uttrekk_Rapport'

setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RSoyleSml/")
source("fun_SoyleSml.R", encoding="UTF-8")
test <- SoyleSml(opdata=opdata, outfile=outfile, reshID=reshID, variabel=variabel,
		tidlOp=tidlOp, aar=aar, kjonn=kjonn, shgr=0, hovedkat=hovedkat, ktr=ktr, just=0)
#For å kunne standardisere, må hele datamaterialet sendes med

#-----------------------------------------------------------------------------------------------------


#---------------------------------------------------------------
#				Kvalitetskontroll av data
#---------------------------------------------------------------
library(xtable)
Sweave('KvalKtrAvData.Rnw')

#---------------------------KVALITETSSJEKK-----------------------------
RegData <- RyggPreprosess(RegData=RegData)
#Dobbeltregistrering
PIDop <- table(RegData$PID, RegData$OpDato)
names(PIDop[which(PIDop>1)])
testDato <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpDato')], drop=TRUE, FUN=length)
test[which(test$x >1), ]
testMnd <- aggregate(RegData$InnDato, by=RegData[ ,c('PID','Mnd','OpAar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpAar')], drop=TRUE, FUN=length)
sum(testAar$x >1)

#------------------------------ Data til NPR, Dekningsgradsanalyse-------------
load('A:/Rygg/NKR2010-2017aarsrapp.Rdata')
query <-
      'SELECT loepenummer, Alder, Kjonn, HFNavn, HFID, HFReshID, AvdNavn, AvdID, AvdReshID,
OpDato, Dagkirurgi, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, ProsKode1, ProsKode2
FROM Uttrekk_Rapport
WHERE OpAar=2017'
# variable <- c('PID', 'Alder', 'Kjonn', 'HFNavn', 'HFID', 'HFReshID',
#               'AvdNavn', 'AvdID', 'AvdReshID', 'OpDato', 'Dagkirurgi',
#               'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'ProsKode1', 'ProsKode2')
NKR2017 <- read.table('A:/Rygg/NPR2017.csv', sep=';', header=T, encoding = 'UTF-8')
NKR2017 <- RegData[which(NKR2017$OpDato<2017), ]
sort(variable)[which(!(sort(variable) %in% sort(names(RegData))))]



#------------Sjekk av gml. ny resh------------

RegDataV2 <- rapbase::LoadRegData(registryName="rygg",
                                  query='SELECT * FROM Uttrekk_Rapport_FROM_TORE')
RegDataV3 <- rapbase::LoadRegData(registryName="rygg",
                                  query='SELECT * FROM AlleVarNum')
RegData <- RyggRegDataSQLV2V3()
#table(RegData[,c(ReshId)])
tab <- unique(RegData[, c("ShNavn", "ReshId")])
tab[order(tab$ShNavn),]

sort(table(tab$ShNavn)) #To: Aleris Bergen, Aleris Oslo, Tromsø, Volvat
sort(table(tab$ReshId)) #To: 105153  601161
tab[tab$ReshId==601161,]

rm(list = ls())
setwd("C:/ResultattjenesteGIT/nkr/")
library(nkr)

NKRdata <-
      read.table('C:/VariasjonKvalitet/NKR2013_2015VarKval.csv', sep = ';', header = T, encoding = 'UTF-8')

Innbyggere2013_2015kjonn <-
      read.table('./Innbyggere2013_2015kjonn.csv', sep = ';', header = T, encoding = 'UTF-8')
Innbyggere2013_2015kjonn$BydelNum <-
      factor(as.character(Innbyggere2013_2015kjonn$BydelNum), exclude = "")
BoStederInnb <- aggregate(Innbyggere2013_2015kjonn$AntInnb,
                          by = Innbyggere2013_2015kjonn[,c('Kommune','KommNr', 'BydelNum','BoRHF',"BoHF", 'Fylke', 'Aar')], 
                          FUN = 'sum')#'BoHF',
RHFInnb <-
      aggregate(BoStederInnb$x, by = BoStederInnb[,c('KommNr', 'BoRHF','Aar')], FUN = 'sum')
#RegData <- merge(NKRdata, BoStederInnb, by.x = c("Kommunenr", "OpAar", "Bydelkode"), by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
#RegData1 <- merge(NKRdata, BoStederInnb, by.x = c("Kommunenr", "OpAar", "Bydelkode"),
#                by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
RegData1 <-
      merge(NKRdata, BoStederInnb[,c('BoHF', 'Fylke', "KommNr", "Aar", "BydelNum")],
            by.x = c("Kommunenr", "OpAar", "Bydelkode"), by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
RegData <-
      merge(RegData1, RHFInnb[,c('BoRHF', "KommNr", "Aar")], by.x = c("Kommunenr", "OpAar"),
            by.y = c("KommNr", "Aar"), all.x = TRUE, all.y = FALSE)
#RegData <- RegData[which(RegData$OpAar %in% 2013:2015),]

#write.table(RegData, file='RegDataTilTest.csv', sep=';', row.names = F)
#Mister BoHF for registreringer som mangler bydelkode for Oslo. Disse kan få BoRHF. Legger derfor til BoRHF og BoHF separat

#Endre sykehusnavn:
  sykehusnavn <- read.table('E:/FELLES/Prosjekter/Indikatorprosjektet/Analyse/Figurer/Sykehusnavn.csv', header=TRUE, sep=";", encoding = 'UFT-8')
  sykehusnavn <- sykehusnavn[,c(1,3)]
  names(sykehusnavn) <- c("BehSh_nr","BehSh_navn")

  RegData <- merge(RegData, sykehusnavn,
                           by.x = "AvdNavnNum", by.y = "BehSh_nr", all.x = TRUE, all.y = FALSE)
#  names(RegData)
  names(RegData)[which(names(RegData)=='AvdNavn')] <-  'AvdNavnLang'
  names(RegData)[which(names(RegData)=='BehSh_navn')] <-  'AvdNavn'
  


#__Inndata til funksjon:
#reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2013-01-01'
datoTil <- '2015-12-31'
erMann <- ''
tidlOp <- 99
minald <- 20
maxald <- 85
opKat <- 99  #Hastegrad
tidlOp <- 99 #
ktr <- 1
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
AKjust <- 1
grVar <- 'BoHF'  #ShNavn, Fylke, BoHF, BoRHF
valgtVar <- 'SympVarighUtstr'   #BeinsmEndrLav', SympVarighUtstr, OswEndr13, OswEndr20, OswEndr30pst, Osw48, Verre
outfile <- paste0(valgtVar, '_', grVar, AKjust,'Aar.png')

AKjustDum <- 1 #Settes automatisk til 0 hvis grVar ulik BoRHF eller BoHF
grupperingInd <- c('ShNavn', 'BoHF') #c('Fylke', 'ShNavn', 'BoRHF', 'BoHF')
for (grVar in grupperingInd) {
      ifelse (grVar %in% c('BoHF', 'BoRHF'), AKjust <- AKjustDum, AKjust <- 0)
      valgtVar <- 'SympVarighUtstr' 
            outfile <- paste0(valgtVar, '1_1', grVar, AKjust,'Aar.pdf')
            hovedkat <- 1
            opKat <- 1  #Bare elektive pasienter
            RyggFigAndelerGrVarAarVarKval(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil, minald = minald, 
                  maxald = maxald, erMann=erMann, hovedkat = hovedkat, ktr = ktr,preprosess=1, opKat=opKat, tidlOp=tidlOp,
                  outfile = outfile, grVar = grVar, AKjust=AKjust)
      }      

#GML:
AKjustDum <- 1 #Settes automatisk til 0 hvis grVar ulik BoRHF eller BoHF
grupperingInd <- c('ShNavn', 'BoHF') #c('Fylke', 'ShNavn', 'BoRHF', 'BoHF')
for (grVar in grupperingInd) {
      ifelse (grVar %in% c('BoHF', 'BoRHF'), AKjust <- AKjustDum, AKjust <- 0)
      for (valgtVar in c('BeinsmLavPre', 'SympVarighUtstr')) {
            outfile <- paste0(valgtVar, '1_1', grVar, AKjust,'Aar.pdf')
            hovedkat <- 1
            opKat <- 1  #Bare elektive pasienter
            RyggFigAndelerGrVarAarVarKval(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil, minald = minald, 
                  maxald = maxald, erMann=erMann, hovedkat = hovedkat, ktr = ktr,preprosess=1, opKat=opKat, tidlOp=tidlOp,
                  enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar, siste3aar=siste3aar, AKjust=AKjust)
      }
      valgtVar <- 'KpInf3Mnd'
            hovedkat <- 1:3
            outfile <- paste0(valgtVar, '123_', grVar, AKjust,'Aar.pdf')
            RyggFigAndelerGrVarAarVarKval(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,hovedkat=hovedkat,
                  minald = minald, maxald = maxald, erMann = erMann, ktr = ktr,  siste3aar=siste3aar, AKjust=AKjust,
                  preprosess = 1, reshID = reshID, outfile = outfile, grVar = grVar)
     
}

#Kommune 301 Oslo har fire BoHF. For øvrig kommunenummer bestemmer kommunenummer entydig BoHF.
#BoStederInnb2015 <- BoStederInnb[which(BoStederInnb$Aar == '2015'),]
#save(BoStederInnb2015, file="./data/BostederInnb2015.Rdata")
#write.table(BoStederInnb2015, file="./data/BostederInnb2015.csv", sep=';')



#----------------Alders-og kjønnsstandardisering:-----------------------
#Gruppere på alder og kjønn. Velger aldersgrupper ut fra hendelsesfordeing, dvs. fra
#aldersfordelinga i registerpopulasjonen

NKRdata <-
      read.table(
            'C:/Registre/nkr/data/NKR2010_2015.csv', sep = ';', header = T, encoding = 'UTF-8'
      )
NKRdata <- RyggPreprosess(NKRdata)
NKRUtvalg <-
      RyggUtvalg(
            RegData = NKRdata, datoFra = datoFra, datoTil = datoTil, minald = minald, maxald =
                  maxald,
            erMann = erMann, hovedkat = hovedkat
      )
RegData <- NKRUtvalg$RegData
NKRUtvalg$utvalgTxt

#Velger 4 aldersgrupper
aldKvant <- quantile(RegData$Alder, c(25, 50, 75) / 100, na.rm = T)
#25% 50% 75% Hele populasjonen gir 43,57,67
#37  46  57
#aldKvant <- c(36,45,56)
aldgr <- c(minald - 1,aldKvant,85)
RegData$AlderGr <-
      cut(RegData$Alder,aldgr) #grensene er øverste grense i aldersintervallet
table(RegData$AlderGr)

#Må finne andel av normalpopulasjonen i disse gruppene ut fra befolkningsfil
#For alders-og kjønnsstandardisering:
Innb2015aldkj <-
      read.table(
            './Innbyggere2015aldkj.csv', sep = ';', header = T, encoding = 'UTF-8'
      )
Innb2015aldkj$AlderGr <- cut(Innb2015aldkj$Alder,aldgr)
#Innbyggere2015alder <- read.table('C:/VariasjonKvalitet/Innbyggere2015.csv', sep=';', header = T)
#Innbyggere <- with(Innbyggere2015alder, aggregate('AntInnb', by=list('KommNr', 'BoHF', 'BoRHF','Fylke'), FUN='sum')
PopAldKjGr <-
      aggregate(AntInnb ~ erMann + AlderGr, data = Innb2015aldkj,FUN = sum)
PopAldKjGr$Vekt <-
      prop.table((PopAldKjGr$AntInnb))#PopAldKjGr$AntInnb/sum(PopAldKjGr$AntInnb)

#Finne resultat i hver alder-og kjønnsgruppe

#Beregne kjønns- og aldersstandardisert resultat, dvs. vekte resultatene i gruppene
#ut fra normal-populasjon


#Andel over 70 år

NKRdata <-
      read.table('C:/Registre/nkr/HUNT/RyggAlle.csv', sep = ';', header = T, encoding = 'UTF-8')
nevner <- table(NKRdata[, 'OpAar'])
teller <- table(NKRdata[which(NKRdata$Alder>=70), 'OpAar'])
andelAlle <- 100*teller/nevner
round(tapply(NKRdata$Alder, NKRdata$OpAar, 'mean', na.rm=T),1)
tapply(NKRdata$Alder, NKRdata$OpAar, 'median', na.rm=T)

'%i%' <- intersect
ind <- which(NKRdata$HovedInngrep==1) %i% which(NKRdata$OpKat==1)
NKRdata <- NKRdata[ind,]
nevner <- table(NKRdata[, 'OpAar'])
teller <- table(NKRdata[which(NKRdata$Alder>=70), 'OpAar'])
andel <- 100*teller/nevner
plot(andel)
round(tapply(NKRdata$Alder, NKRdata$OpAar, 'mean', na.rm=T),1)
tapply(NKRdata$Alder, NKRdata$OpAar, 'median', na.rm=T)
tapply(NKRdata$Alder, NKRdata$OpAar, 'summary', na.rm=T)
summary(NKRdata$Alder)


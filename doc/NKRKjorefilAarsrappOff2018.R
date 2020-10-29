#**************************************************
#********************* Tilrettelegge filer ****************************
NKRdata <- read.table('A:/Rygg/RyggAarsrapp2018.csv', sep=';', header=TRUE) #, fileEncoding = 'UTF-8') #, encoding = 'UTF-8') # #, ) #
#NKRdata <- read.csv2('A:/Rygg/RyggAarsrapp2018.csv', sep=';', header=T,encoding = 'UTF-8') #fileEncoding = 'UTF-8')
table(NKRdata$Region)

RegData <- NKRdata
save(RegData, file=paste0('A:/Rygg/Rygg2010-2018aarsrapp', '.Rdata'))

#load('A:/Rygg/NKR2010-2017aarsrapp.Rdata') #IKKE preprossessert
#save(RegData, file='C:/Registre/nkr/data/NKR2010-2016aarsrapp.Rdata')

#Nakke
NakkeSkjemaDataRaa <- read.table(paste0('A:/Nakke/SkjemaOversiktAarsrappRaa.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #, encoding = 'UTF-8')
NakkeSkjemaData <- SkjemaData[which(as.Date(SkjemaData$HovedDato) <= '2018-12-31'), ]
write.table(NakkeSkjemaData, file = 'A:/Nakke/NakkeSkjemaDataAarsrapp2018.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')

NakkeDataRaa <- read.table(paste0('A:/Nakke/AlleVarNumAarsrappRaa.csv'), sep=';', header=T, encoding = 'UTF-8')
NakkeData <- NakkeDataRaa[which(as.Date(NakkeDataRaa$OprDato) <= '2018-12-31'), ]
write.table(NakkeData, file = 'A:/Nakke/NakkeDataAarsrapp2018.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')

NakkeAarsrapp2018 <- save(NakkeSkjemaData, NakkeData, file = 'A:/Nakke/NakkeAarsrapp2018.Rdata')
load('A:/Nakke/NakkeAarsrapp2018.Rdata')
#------------------Resultatkapittel--------------------------------
library(nkr)
library(knitr)
library(tools)

rm(list=ls())
setwd('C:/ResultattjenesteGIT/nkr/AarsrappOff/')
#load('A:/Rygg/Rygg2010-2018aarsrapp.Rdata') #IKKE preprossessert
#load('A:/Rygg/NKR2010-2017aarsrapp.Rdata') #IKKE preprossessert

knit('ResultaterAarsrappDiv.Rnw', encoding = 'UTF-8')
texi2pdf('ResultaterAarsrappDiv.tex') 


#__Inndata til funksjon (Står i Rnw-fila)
# datoFra <- '2011-01-01'
# datoTil <- '2017-12-31'
# aarsRappAar <- 2017
# aarsStart <- paste0(aarsRappAar,'-01-01')
# aar <- aarsRappAar
# aar2 <- (aarsRappAar-1):aarsRappAar  #2015:2016
# tidlAar <- aarsRappAar-1
# tidlAar2 <- (aarsRappAar-3):(aarsRappAar-2) #2013:2014
# hovedkat <- 99 		#Hovedinngrep, 0-9, Standard: 99, dvs alle operasjoner
# opKat <- 99  #Bare elektive pasienter
# tidlOp <- 99 #4 - Bare primæroperasjoner
# enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
# grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
# minald<- 0 
# maxald<-130
# erMann<-''
# ktr <- 2
# Ngrense <- 20
# AKjust <- 0
# reshID<-0


#Nakke
valgtVar <- 'KomplStemme3mnd'
myelopati=99
fremBak=0
ktr=0
hentData=0
outfile=''

#----------------------------Kvalitetsindikatorer:
#---NAKKE----------
rm(list=ls())
library(Nakke)
setwd("C:/ResultattjenesteGIT/nkr/AarsrappOff/Figurer/KvalInd")
load('A:/Nakke/NakkeAarsrapp2018.Rdata') #IKKE preprossessert
datoFra <- '2018-01-01'

#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, preprosess=1, valgtVar='Komplinfek',
                     Ngrense=20, outfile= 'NakkeKomplinfekSh.pdf')
NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='Komplinfek',
                        Ngrense=20, ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra,  valgtVar='KomplStemme3mnd',
                   myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplStemme3mndSh.pdf')

NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='KomplStemme3mnd',
                     myelopati=0, fremBak=1, Ngrense=20,
                     ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra,  valgtVar='KomplSvelging3mnd',
                        myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplSvelging3mndSh.pdf')
NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='KomplSvelging3mnd',
                   myelopati=0, fremBak=1, Ngrense=20,
                   ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

# 
#-------------------------- RYGG ---------------------
#Kvalitetsindikatorer, 2018
#-----------Dekningsgrad
#Mål: 		<60 % lav måloppnåelse (rød), 60-80 % middel måloppnåelse (gul) >80 % høy måloppnåelse (grønn)
RegData=0
valgtVar='deknNakke17' #Ikke gjort dekningsgrad for 2018
hovedkat=1
Ngrense=20 
outfile=''
RyggFigAndelerGrVar(RegData=0, valgtVar='deknRygg17', outfile='deknRygg17.pdf')
RyggFigAndelerGrVar(RegData=0, valgtVar='deknNakke17', outfile='deknNakke17.pdf')

#LAGE PANELPLOTT MED UTVIKLING OVER TID FOR HVERT SYKEHUS ?
rm(list = ls())
library(nkr)
load('A:/Rygg/Rygg2010-2018aarsrapp.Rdata')
setwd('C:/ResultattjenesteGIT/nkr/AarsrappOff/Figurer/KvalInd')

#__Inndata til funksjon (Står i Rnw-fila)
datoFra <- '2011-01-01'
datoTil <- '2018-12-31'
datoTil12mnd <- '2017-12-31'
aarsRappAar <- 2018
aarsStart <- paste0(aarsRappAar,'-01-01')
aar <- aarsRappAar
aar2 <- (aarsRappAar-1):aarsRappAar
aar2_12mnd <- (aarsRappAar-2):(aarsRappAar-1)
tidlAar <- aarsRappAar-1
tidlAar2 <- (aarsRappAar-3):(aarsRappAar-2)
format <- 'pdf'

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
                       hovedkat=1, Ngrense=20, outfile=paste0('BeinsmLavPreProAar.', format)) #opKat=opKat, tidlOp=tidlOp, 
RyggFigAndelerGrVar(RegData=RegData, valgtVar='beinsmLavPre', aar=aar2, 
                       hovedkat=1, Ngrense=20, outfile=paste0('BeinsmLavPrePro.', format)) #opKat=opKat, tidlOp=tidlOp, 
RyggFigAndelTid(RegData=RegData, valgtVar='beinsmLavPre',  #datoFra=datoFra, datoTil=datoTil, 
                hovedkat=1, outfile=paste0('BeinsmLavPreProTid.', format))
					   
#-----------Sårinfeksjon, dyp og overfladisk----
# Mål: 		Prolaps 2 % høy måloppnåelse (grønt),
# Stenose 3 % høy måloppnåelse (grønt)
# Fusjonskirurgi (LSS+Deg. Sp.listese)
# Hensikt: 	Redusere postoperative sårinfeksjoner
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1,  #Prolaps
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndProAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8,  #Stenose
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndSSAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, #Fusjonskirurgi  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('KpInf3MndFusjAar.', format))

RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1,  #Prolaps
                       Ngrense=30, aar=aar2,  outfile=paste0('KpInf3MndPro.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8,  #Stenose
                       Ngrense=30, aar=aar2, outfile=paste0('KpInf3MndSS.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, #Fusjonskirurgi  
                       Ngrense=30, aar=aar2, outfile=paste0('KpInf3MndFusj.', format))


RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1, outfile=paste0('KpInf3MndProTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=5, outfile=paste0('KpInf3MndFusjTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8, outfile=paste0('KpInf3MndSSTid.', format))

#-----------Durarift
#Mål: 		Prolaps 2 % høy måloppnåelse (grønt),
#Stenose 3 % høy måloppnåelse (grønt), 
#Fusjonskirurgi (LSS+Deg. Sp.listese)
#Hensikt: 	Redusere forekomst av peroperativ komplikasjon 
#Komplikasjon durarift ved operasjon (prolaps, elektiv, primærop.), - lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraProAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraFusjAar.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile=paste0('PeropKompDuraSSAar.', format))

RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraPro.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraFusj.', format))
RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, outfile=paste0('PeropKompDuraSS.', format))

RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                outfile=paste0('PeropKompDuraProTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=5, tidlOp=4, opKat=1, outfile=paste0('PeropKompDuraFusjTid.', format))
RyggFigAndelTid(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, outfile=paste0('PeropKompDuraSSTid.', format))


format <- 'pdf'
#Gjennomsnittlig forbedring av ODI,   
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, opKat=1, ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile='') #paste0('ODIendrProAar.', format)) # tidlAar=tidlAar2,
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, opKat=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile=paste0('ODIendrFusjAar.', format))
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=8, tidlOp=4, opKat=1,  ktr=2,
                 Ngrense=30, aar=aar2_12mnd, outfile=paste0('ODIendrSSAar.', format))
RyggFigGjsnGrVar(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, opKat=1,  ktr=2,
                 Ngrense=30, aar=2011:(aarsRappAar-1), outfile=paste0('ODIendrDSAar.', format))

RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=1, tidlOp=4, opKat=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrProTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=5, tidlOp=4, opKat=1, tidsenhet = 'Aar',  ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrFusjTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=8, tidlOp=4, opKat=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrSSTid.', format))
RyggFigGjsnBox(RegData=RegData, valgtVar='OswEndr', hovedkat=9, tidlOp=4, opKat=1, tidsenhet = 'Aar', ktr=2,
               datoTil =datoTil12mnd, outfile=paste0('ODIendrDSTid.', format))


#Nye, 2018-data			:
#Gjennomsnittlig forbedring av ODI, Prolaps  
#Gjennomsnittlig forbedring av ODI, SS
#Gjennomsnittlig forbedring av ODI, Fusjonskirurgi
#Sårinfeksjon, fusjonskirurgi 
#Durarift, fusjonskirurgi

(Ut <- RyggFigAndelTid(RegData=RegData, datoTil= '2017-12-31', valgtVar='Verre', ktr = 2, 
                              hovedkat=1, #tidlOp=4, opKat=1, 
                       aar=2011:2017))

#Tatt bort: 
#----------------Oswestry-skår =<22p, 12 mnd. etter (prolaps, elektiv, primærop.) – høy
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='Osw22', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile=paste0('Osw22Pro.', format)) 
#Oswestry-skår =<22p, 12 mnd. etter (spinal stenose, elektiv, primærop.), 12 og 13 mot 14 og 15 – høy
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='Osw22', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile=paste0('Osw22SS.', format)) 

#Helt fornøyde pasienter 12 mnd. etter (prolaps og spinal stenose, elektiv, primær) - høy
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='fornoyd', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile= paste0('FornoydPro.', format))
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='fornoyd', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile=paste0('FornoydSS.', format)) 

#Andel prolapsopererte ODI endring < 13 (failure)
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='OswEndrLav', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile=paste0('OswEndrLavPro.', format)) 
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='OswEndrLav', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile=paste0('OswEndrLavSS.', format))

#----------------Andre figurer til årsrapport---------------



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
DataTilResultatportalen(RegData = RyggData, valgtVar='KpInf3Mnd', datoFra = '2011-01-01', hovedkat=8,
                        filUt = 'ind4_Saarinfeksjon_stenose')

#-----------Durarift
DataTilResultatportalen(RegData = RyggData, valgtVar='PeropKompDura', datoFra = '2011-01-01', hovedkat=1, 
                        tidlOp=4, opKat=1, filUt = 'ind5_Durarift prolaps_Rygg')
DataTilResultatportalen(RegData = RyggData, valgtVar='PeropKompDura', datoFra = '2011-01-01', hovedkat=8, 
                        tidlOp=4, opKat=1, filUt = 'ind6_Durarift_stenose_Rygg')

#Alle sykehus og resh:
ShResh <- unique(RegData[c('ReshId', 'ShNavn')])
write.table(ShResh, file = 'A:/Resultatportalen/RyggShResh', sep = ';', row.names = F)

DataTilResultatportalen <- function(RegData = RegData, valgtVar, datoFra = '2011-01-01',
                                    hovedkat=99, opKat=99, tidlOp='', filUt='dummy'){
      filUt <- paste0(ifelse(filUt=='dummy',  valgtVar, filUt), '.csv')
      RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
      RyggUtvalg <- RyggUtvalgEnh(RegData=RyggVarSpes$RegData, datoFra = datoFra, hovedkat=hovedkat, tidlOp=tidlOp, opKat=opKat) #, datoTil=datoTil, aar=aar)
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

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
# RyggData$Bedre <- 0
# RyggData$Verre <- 0
# RyggDataEndring <-  RyggData[intersect(which(RyggData$Utfylt3Mnd==1), which(RyggData$Nytte3mnd %in% 1:7)), 
#                            c('Nytte3mnd', 'Bedre', 'Verre','Aar')]
# RyggDataEndring$Bedre[RyggDataEndring$Nytte3mnd %in% 1:2] <- 1
# RyggDataEndring$Verre[RyggDataEndring$Nytte3mnd %in% 6:7] <- 1
# 
# andelSuksess <- tapply(RyggDataEndring$Bedre, RyggDataEndring$Aar, FUN='mean', na.rm=T)
# andelVerre <- tapply(RyggDataEndring$Verre, RyggDataEndring$Aar, FUN='mean', na.rm=T)

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
  
#------------------------------Tidligere Kvalitetsindikatorer og andre figurer til årsrapp/off----------------------
#Sårinfeksjon, pasientrapportert (prolaps) – lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='KpInf3Mnd', hovedkat=1,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='KpInf3MndPro.png') 
#Sårinfeksjon, pasientrapportert (spinal stenose) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='KpInf3MndSS.png') 
#Komplikasjon durarift ved operasjon (prolaps, elektiv, primærop.), - lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraPro.png') 
#Komplikasjon durarift ved operasjon (spinal stenose, elektiv, primærop.) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraSS.png') 
#Degen. spondylolistese operert med fusjonskirurgi, hele tidsperioden
RyggFigAndelerGrVar(RegData=RegData, preprosess=0, valgtVar='degSponFusj', aar = (aarsRappAar-4):aarsRappAar,#
                       Ngrense=30, outfile='DegSponFusj.png') #aar=2016, 

#--sml. to og to år. Siste året blir året før årsrapport
#Forbedring av Oswestry-skår <13p, 12mnd etter. (prolaps, elektiv, primærop.) – lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='OswEndrLav', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='OswEndrLav.png') 

#Oswestry-skår =<22p, 12 mnd. etter (prolaps, elektiv, primærop.) – høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Osw22', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Osw22Pro.png') 
#Oswestry-skår =<22p, 12 mnd. etter (spinal stenose, elektiv, primærop.), 12 og 13 mot 14 og 15 – høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Osw22', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Osw22SS.png') 

#Helt fornøyde pasienter 12 mnd. etter (spinal stenose, elektiv, primær) - høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Fornoyd', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Fornoyd.png') 

#Andel durarift, prolaps
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=1, opKat=1, tidlOp=4, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraPro.png')
#Andel durarift, prolaps
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=8, opKat=1, tidlOp=4, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraSS.png')
#Gjennomsnittlig liggetid (spinal stenose)
# Ett år mot forrige
RyggFigGjsnBox(valgtVar='Liggedogn', RegData=RegData, preprosess=0, datoFra='2010-01-01', hovedkat=8, 
               outfile='LiggedognTidSS.png')

RyggFigGjsnGrVar(valgtVar='Liggedogn', Ngrense=20, valgtMaal = 'Gjsn', 
                 RegData=RegData, preprosess=0, datoFra=aarsStart, hovedkat=8, outfile='') #LiggedognSh.png')

#Andel varighet av utstrålende smerter mer enn ett år (SympVarighUtstr). 
#Utvalg: prolapskirurgi og  foraminotomi + laminectomi slått sammen i en gruppe.
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='SympVarighUtstr', hovedkat=1,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='SympVarighUtstrPro.png')


#--------------------------------------N>30:
#??Andel fusjonskirurgi over 75 år. Andel foramenotomi over 75 år
valgtVar <- 'Alder'
ktr <- 1
outfile <- paste0(valgtVar='alder75', '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, 
                    reshID=reshID, outfile=outfile)

#Andel prolapsoperte uten parese (OpIndParese != 1) med NRS bensmerte < 2,5 preoperativt
valgtVar <- 'BeinsmLavPre'
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, 
                    reshID=reshID, outfile=outfile)
     

#Andel pasienter med Degenerativ spondylolistese (RfSpondtypeDegen=1) og sentral spinal stenose (RfSentr=1). 
#Utvalg: Andel  operert med fusjonskirurgi. 
valgtVar <- 'DegSponSSSten'
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Fremmedspråklige
valgtVar <- 'Morsmal'    
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
					


#--------------------------------------N>50

#Andel med  bensmerte endring < 1,5 (failure)
#Utvalg: prolaps
valgtVar <- 'BeinsmEndrLav'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#KpInf3Mnd: Andel sårinfeksjon (enhver). Variabel: KpInf3Mnd 
#Utvalg: prolaps, foraminotomi+ laminectomi slått sammen i en gruppe og fusjonskirurgi)
valgtVar <- 'KpInf3Mnd'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Andel med 12 mnd ODI> 48 (forverring). (Dvs. Osw12mnd>48, uavhengig av prescore.)
#Utvalg: prolaps
valgtVar <- 'Osw48'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 20 (suksess)
#Utvalg: prolaps
valgtVar <- 'OswEndr20'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 30% (suksess)
#Utvalg: foramenotomi + laminectomi
valgtVar <- 'OswEndr30pst'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
#Andel prolapsopererte ODI endring < 13 (failure)
valgtVar <- 'OswEndrLav'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
#Andel  mye verre/verre enn noen sinne. 
valgtVar <- 'Verre'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#---------------------------------------------------------------
#				Kvalitetskontroll av data
#---------------------------------------------------------------
# RYGG
rm(list=ls())
library(nkr)
load('A:/Rygg/NKR2010-2017aarsrapp.Rdata') #IKKE preprossessert

RegData <- RyggPreprosess(RegData=RegData)
#Dobbeltregistrering
PIDop <- table(RegData$PID, RegData$OpDato)
testDato <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpDato')], drop=TRUE, FUN=length)
testDato[which(testDato$x >1), ]
testMnd <- aggregate(RegData$InnDato, by=RegData[ ,c('PID','Mnd','OpAar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpAar')], drop=TRUE, FUN=length)
sum(testAar$x >1)


# NAKKE
library(Nakke)
setwd('C:/ResultattjenesteGIT/nkr/inst/')
NakkeData <- read.table('A:/Nakke/AlleVarNum2018-06-21.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE)  # na.strings = "NULL", 


load('A:/Nakke/NakkeAarsrapp2017.Rdata')
RegData <- NakkePreprosess(RegData=NakkeData)
#Dobbeltregistrering
#PIDop <- table(RegData$PasientID, RegData$OprDato)
testDato <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','OprDato')], drop=TRUE, FUN=length)
testDato[which(testDato$x >1), ]
RegData$Mnd <- RegData$InnDato$mon +1
RegData$Mnd <- RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
testMnd <- aggregate(RegData$OprDato, by=RegData[ ,c('PasientID','Mnd','Aar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','Aar')], drop=TRUE, FUN=length)
sum(testAar$x >1)

#---------------------------Dekningsgrad-----------------------------------------
DekningsgradRygg2017
DeknData <- read.table('P:/Registerinfo og historie/nkr/AarsrappOff/Deknin.csv', sep=';', header=T, encoding = 'UTF-8')
head(DeknData)
RegData <- DeknData

RyggFigAndelerGrVar(RegData=DeknData, preprosess = 0, valgtVar='deknGrad', datoFra='2016-01-01',outfile=outfile)








rm(list = ls())
library(nkr)
load('A:/Rygg/Rygg2010-2018aarsrapp.Rdata')
#Aktuelle variable: KpInf3Mnd (1,5,8), PeropKompDura(1,5,8), OswEndr (1,5,8+9) (gj.sn)

valgtVar <- 'PeropKompDura' #'KpInf3Mnd' 'PeropKompDura' 'OswEndr' 
if (valgtVar == 'OswEndr'){
   datoTil <- '2017-12-31'
   BerData <- RyggFigGjsnBox(RegData=RegData, valgtVar=valgtVar, ktr=2, hovedkat=9, tidlOp = 4, opKat=1,
                             tidsenhet = 'Aar', datoTil=datoTil,
                             lagFig=0)
   DS <- BerData$AggVerdier[1,]
} else {
   BerData <- RyggFigAndelTid(RegData=RegData, valgtVar=valgtVar, #datoFra='2011-01-01', datoTil='3000-12-31', 
                              lagFig=0)
   Prolaps <- RyggFigAndelTid(RegData=RegData, valgtVar=valgtVar, tidsenhet = 'Aar', hovedkat = 1, 
                              lagFig=0)$AggVerdier$Hoved      
   Fusj <- RyggFigAndelTid(RegData=RegData, valgtVar=valgtVar, #datoFra='2011-01-01', datoTil='3000-12-31', 
                           hovedkat = 8, lagFig=0)$AggVerdier$Hoved
   SS <- RyggFigAndelTid(RegData=RegData, valgtVar=valgtVar, #datoFra='2011-01-01', datoTil='3000-12-31', 
                         hovedkat = 5, lagFig=0)$AggVerdier$Hoved
   Alle <- BerData$AggVerdier$Hoved
}

if (valgtVar=='OswEndr'){
   Alle <- BerData$AggVerdier[1,]
   yakseTxt <- 'Gjennomsnittlig endring'
} else {
   BerData$AggVerdier$Hoved
   yakseTxt <- BerData$yakseTxt}
grtxt2 <- BerData$grtxt2
tidtxt <- BerData$tidtxt
tittel <- BerData$tittel
utvalgTxt <- BerData$utvalgTxt
fargepalett <- BerData$fargepalett
#KImaalGrenser <- BerData$KImaalGrenser



AggVerdier <- rbind(
   'Alle' = Alle,
   'Prolaps' = Prolaps,
   'Spinal stenose' = SS,
   'Fusjonskirurgi' = Fusj
#'Degen. spondylolistese' = DS
)

#8+9: 'Spinal stenose' + 'Degen. spondylolistese'

#FIGUR
outfile <- paste0(valgtVar, 'TidSmlGr.pdf')
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=fargepalett)
farger <- FigTypUt$farger
NutvTxt <- length(utvalgTxt)
hmarg <- 0.04+0.01*NutvTxt
par('fig' = c(0,1,0,1-hmarg)) 
cexleg <- 1	#Størrelse på legendtekst
xskala <- 1:length(tidtxt)
xmax <- max(xskala)
ymax <- min(119, 1.25*max(AggVerdier,na.rm=T))
#matplot(t(AggVerdier), type = c("b"),pch=1,col = farger) #plot
plot(xskala, Alle,  font.main=1,  type='o', pch="'", col='white', #type='o', 
     xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
     cex=2, xlab='Operasjonsår', ylab=yakseTxt, ylim=c(0,ymax), yaxs = 'i') 	

grid(nx = NA, ny = NULL, col = farger[4], lty = "solid") #Legge på linjer i plottet. 
axis(side=1, at = xskala, labels = tidtxt)
title(tittel, line=1, font.main=1)

for (k in 1:dim(AggVerdier)[1]) {
   tykkelse <- ifelse(k==1, 5, 2)
   lines(xskala, AggVerdier[k,], col=farger[k], lwd=tykkelse)
}
#KImål
# if (!is.na(KImaalGrenser[1])) {
#       lines(xskala, rep(KImaalGrenser[2],length(xskala)), col= '#4fc63f', lwd=3) #col='#FF7260'
#       text(max(xskala), KImaalGrenser[2], pos=4, 'Mål', cex=0.9, col = '#4fc63f')
# }

# Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')') 
legend('topleft', border=NA, dimnames(AggVerdier)[[1]], bty='n', ncol=1, cex=cexleg, 
       col=farger[1:4], lwd=3)		
#Tekst som angir hvilket utvalg som er gjort
# avst <- 0.8
# utvpos <- 3	#Startlinje for teksten
# mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}




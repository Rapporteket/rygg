#Her tenker jeg å samle kode til ulike datautleveringer fra Rygg

#----------------Dekningsgradsanalyse for 2023-----------------
#samt utlevering til Arendal (egne data)
#Skal bare begrense koblingnøkkelen til de aktuelle data
RyggKobl <- read.csv2(file = 'd:RYGG_koblingstabell.csv')
NakkeKobl <- read.csv2(file = 'd:NAKKE_koblingstabell.csv')

Rygg23 <- read.csv2(file = 'd:Rygg_dataDump_23.csv')
RyggKobl23 <- RyggKobl[which(RyggKobl$PID %in% Rygg23$PID), ]
write.csv2(RyggKobl23, file =  'd:RyggKobl23.csv', row.names = FALSE)

Nakke23 <- read.csv2(file = 'd:Nakke_dataDump_23.csv')
NakkeKobl23 <- NakkeKobl[which(NakkeKobl$PID %in% Nakke23$PasientID), ]
write.csv2(NakkeKobl23, file =  'd:NakkeKobl23.csv', row.names = FALSE)

Arendal <- read.csv2(file = 'd:Arendal_dataDump_fom2020.csv')
ArendalKobl <- RyggKobl[which(RyggKobl$PID %in% Arendal$PID), ]
write.csv2(ArendalKobl, file =  'd:ArendalKobl.csv', row.names = FALSE)

#--------- Data til NPR for å få CCI Charlson-komorbiditets-index-------
RyggV2 <- read.csv2(file = 'C:/Registerdata/rygg/TilCharlson/uttrekk_rapport_from_tore.csv')
RyggV2$PIDny <- paste0('V2PID', RyggV2$PID)
RyggV2$SSN_v3 <- NA
RyggV2$PasientID <- NA
names(RyggV2)[which(names(RyggV2)=='Personnummer')] <- 'SSN'
RyggV2$SSN_v2 <- RyggV2$SSN

RyggV3 <- read.csv2(file = 'C:/Registerdata/rygg/TilCharlson/Rygg_forlopsoversikt.csv')
V3Kobl <- read.csv2(file = 'C:/Registerdata/rygg/TilCharlson/Rygg_koblingstabell.csv')
RyggV3 <- merge(RyggV3, V3Kobl, by.x = 'PasientID', by.y = 'PID', all.x = T)
RyggV3$PIDny <- RyggV3$PasientID
RyggV3$SSN_v2 <- NA
RyggV3$SSN_v3 <- RyggV3$SSN
RyggV3$PersNr <- RyggV3$Personnummer
names(RyggV3)[which(names(RyggV3)=='HovedDato')] <- 'OpDato'

variabler <- c('PIDny', 'SSN', 'SSN_v2', 'SSN_v3', "OpDato", 'PasientID')
RyggData <- rbind(RyggV2[ ,variabler],
                  RyggV3[ ,variabler])

RyggData$SSN <- formatC(RyggData$SSN, width = 11, flag = 0)
# formatC(c(23, 1, 8977), width = 5, flag = 0)

#Entydig PID SSN-var fra V3/koblingstab, Personnummer-var fra V
tidlPas <- match(RyggData$SSN_v3, RyggData$SSN_v2, nomatch = 0, incomparables = NA)
hvilkePas <- which(tidlPas>0)
RyggData$PIDny[hvilkePas] <- RyggData$PIDny[tidlPas[hvilkePas]]


# PIDfraV2 <- 'V2PID13054' #c('V2PID16384', 'V2PID13054')
# RyggData[RyggData$PIDny %in% PIDfraV2,]


RyggKobl <- unique(RyggData[,c('PIDny', 'SSN')])
RyggAkt <- RyggData[,c('PIDny', 'OpDato')]
write.csv2(RyggKobl, file = 'C:/Registerdata/rygg/TilCharlson/RyggKobl.csv', row.names = F)
write.csv2(RyggAkt, file = 'C:/Registerdata/rygg/TilCharlson/RyggOpDato.csv', row.names = F)

#sum(table(table(RyggData$PIDny)))


#------------Omstrukturer til bredt format--------------
#Ønsker omstrukturering til ei rad per person hvor variabler tilhørende påfølgende
#operasjoner kommer på ei linje.
#Lag først datasett hvor operasjonene er nummerert.
#Operasjonsnummer kan da legges til variabelnavnet når vi gjør om til bredt format.


#-------- Til Ole Kristian 2023/4?---------------------------
#Hva heter prosjektet?
#Ønsker data for noen utvalgte PID.
#Har fått PID (felles for V2 og V2) og innleggelsesdato. Vi har ikke innleggelsesdato, men benytter operasjonsdato.
library(rygg)
koblVar <- c('PID', 'OpDato')
#koblingsPID <- readxl::read_xlsx('C:/Registerinfo/Rygg/DataUtlev/PIDinndato_mer_baseline.xlsx',
koblingsPID <- readxl::read_xlsx('data-raw/PIDinndato_mer_baseline.xlsx',
                             col_names = F)
colnames(koblingsPID) <- koblVar
koblingsPID$PID <-toupper(koblingsPID$PID)
koblingsPID$koblKode <- paste(koblingsPID$PID, koblingsPID$OpDato, sep = '_')

varTilUtlev <- c(
  koblVar,
  'ShNavn',
  'OswsmertePre',
  'OswgaaPre',
  'OswreisPre',
  'OswsittPre',
  'OswsovePre',
  'OswstaaPre',
  'OswstelPre',
  'OswsexPre',
  'OswsosiPre',
  'OswloftPre',
  'OswTotPre',
  'SmRyPre',
  'SmBePre',
  'SymptVarighRyggHof',
  'SympVarighUtstr',
  'EqangstV3Pre',
  'EqperstV3Pre',
  'EqgangeV3Pre',
  'EqvanlgjV3Pre',
  'EqsmerteV3Pre',
  'HelsetilstPre',
  'EQ5DV3Pre',
  'BMI',
  'BMIkategori',
  'RokerV3'
)

AlleData <- RyggPreprosess(RyggRegDataSQLV2V3(alleVarV2 = 1))
setdiff(varTilUtlev, names(AlleData))
setdiff(koblingsPID$PID, sort(unique(AlleData$PID)))
setdiff(koblingsPID$koblKode, dataDump$koblKode)
dataDump[which(dataDump$PID=='1145V2')]

#"110171V2" - feil siden vi ikke har så mange registreringer i V2. Høyeste PID er: 41873
#'1145V2'har operasjonsdato "2007-08-14"

dataDump <- tilretteleggDataDumper(RegData=AlleData)
                                   # ,datoFra = min(koblingsPID$OpDato),
                                   # datoTil = max(koblingsPID$OpDato))
dataDump$OpDato[dataDump$PID == '1145V2']
#dataDump <- finnReoperasjoner(RegData = dataDump)
#DataRed <- dataDump[which(dataDump$PID %in% sort(unique(koblingsPID$PID))),varTilUtlev]
dataDump$koblKode <- paste(dataDump$PID, dataDump$OpDato, sep = '_')
DataRed <- dataDump[which(dataDump$koblKode %in% sort(koblingsPID$koblKode)), varTilUtlev]

write.table(DataRed, file = '../Aarsrappresultater/NKR/RyggTilleggsvar.csv', row.names = F, col.names = T, sep = ';')



#--------------------Til Eirik Mikkelsen, dødsdato 2023?------------------------

PIDdato <- read.csv2(file = 'C:/Registerdata/rygg/Populasjon2_PID_OprDato.csv',
                      header = TRUE, dec = '.')
PIDdato$Operasjonsdato <- format(as.Date(PIDdato$Operasjonsdato, "%m/%d/%Y",  "%B %d %Y"))


dataDumpV2 <- PIDdato <- read.csv2(file = 'C:/Registerdata/rygg/dataDumpV2.csv',
                                   header = TRUE, dec = '.')


#Vi har dødsdato kun for V3. Dette er kun V1/V2-data.






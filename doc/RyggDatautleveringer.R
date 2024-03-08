#Her tenker jeg å samle kode til ulike datautleveringer fra Rygg

#------------Omstrukturer til bredt format--------------
#Ønsker omstrukturering til ei rad per person hvor variabler tilhørende påfølgende operasjoner kommer på ei linje.
#Lag først datasett hvor operasjonene er nummerert. Operasjonsnummer kan da legges til variabelnavnet når vi gjør om til bredt format.

#-------- Til Ole Kristian---------------------------
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



#--------------------Til Eirik Mikkelsen, dødsdato------------------------

PIDdato <- read.csv2(file = 'C:/Registerdata/rygg/Populasjon2_PID_OprDato.csv',
                      header = TRUE, dec = '.')
PIDdato$Operasjonsdato <- format(as.Date(PIDdato$Operasjonsdato, "%m/%d/%Y",  "%B %d %Y"))


dataDumpV2 <- PIDdato <- read.csv2(file = 'C:/Registerdata/rygg/dataDumpV2.csv',
                                   header = TRUE, dec = '.')


#Vi har dødsdato kun for V3. Dette er kun V1/V2-data.






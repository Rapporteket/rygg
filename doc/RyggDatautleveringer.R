#Her tenker jeg å samle kode til ulike datautleveringer fra Rygg

#-------- Til Ole Kristian---------------------------
#Hva heter prosjektet?
#Ønsker data for noen utvalgte PID.
#Har fått PID (felles for V2 og V2) og innleggelsesdato. Vi har ikke innleggelsesdato, men benytter operasjonsdato.

koblVar <- c('PID', 'Dato')
#koblingsPID <- readxl::read_xlsx('C:/Registerinfo/Rygg/DataUtlev/PIDinndato_mer_baseline.xlsx',
koblingsPID <- readxl::read_xlsx('data-raw/PIDinndato_mer_baseline.xlsx',
                             col_names = F)
colnames(koblingsPID) <- koblVar

varTilUtlev <- c(
  'SykehusNavn',
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

AlleData <- RyggRegDataSQLV2V3(alleVarV2 = 1)
setdiff(varTilUtlev, names(AlleData))

dataDump <- tilretteleggDataDumper(data=AlleData, datoFra = min(koblingsPID$Dato))
#dataDump <- finnReoperasjoner(RegData = dataDump)

#DataUt <-


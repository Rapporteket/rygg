##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)

setwd('../data')
setwd('C:/Users/lro2402unn/RegistreGIT/rygg')

sship::dec("c://Users/lro2402unn/RegistreGIT/data/rygg17fd8d901.sql.gz__20260305_110853.tar.gz",
keyfile = "c://Users/lro2402unn/.ssh/id_rsa", target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/rygg17fd8d901.sql;


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)
library(rygg)

RegDataV3AVN <- rapbase::loadRegData(registryName = 'data',
                                     query='SELECT * FROM allevarnum')

RegDataRaa <- RyggRegDataV2V3(datoFra = '2007-01-01')
RegData <- RyggPreprosess(RegData =RegDataRaa)

tapply(RegData$TidlOprAntall, RegData$Aar, FUN = max, na.rm=T)
table(RegData$Aar, RegData$HovedInngrep)

test <- RegData[ ,c(grep(pattern = 'dato',names(RegData), ignore.case = T))]
#FormatTrøbbel: DECEASED_DATE, REGISTERED_DATE, InnlagtDato,
head(test)
head(RegData$OpDato)

RyggFigAndelerGrVar(RegData=RegData,  valgtVar='degSponFusj1op',
                    datoFra = '2024-01-01', datoTil = '2024-12-31',
                    outfile = 'degSponFusj1op_2024.pdf')

remotes::install_github('Rapporteket/rapbase', ref = 'main')
# Generelt: mce.PATIENT_ID kobles til patient.ID


test <- RyggRegDataV2V3(datoFra = '2000-01-01')
names(RegDataV2)[grep('dato', names(RegDataV2), ignore.case = T)]
liste <- unique(test[,c('SykehusNavn', 'AvdRESH')])
liste[order(liste$SykehusNavn),]
table(liste$SykehusNavn)

RyggData <- RyggPreprosess(RyggRegDataV2V3(datoFra = '2000-01-01'))
test <- RyggData[,c("DodsDato", "OpDato", "PasientID", "ASA", "Alder")]

reshID <- 110633
4211878
Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/rygg/api/centre-information')
TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits

setwd('../data')
sship::dec('rygg16609ecfb.sql.gz__20251113_123205.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
setwd('../rygg')

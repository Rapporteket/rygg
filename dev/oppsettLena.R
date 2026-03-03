##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)

setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/rygg1518b6c9.sql.gz__20260303_081824.tar.gz",
keyfile = "c://Users/lro2402unn/.ssh/id_rsa", target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/rygg1518b6c9.sql;


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)
library(rygg)
RegData <- RyggPreprosess(RegData =RegDataV3)

RegDataV3AVN <- rapbase::loadRegData(registryName = 'data',
                                     query='SELECT * FROM allevarnum')

RegData <- RyggHentRegDataV3(medOppf = 0)
RegData <- RyggPreprosess(RegData = RegData)
sort(names(RegData))
setdiff(names(RegData), names(RegDataV3AVN))
setdiff(names(RegDataV3AVN), names(RegData))

RegData <- RyggRegDataSQLV2V3(datoFra = '2023-01-01')

RyggFigAndelerGrVar(RegData=RegData,  valgtVar='degSponFusj1op',
                    datoFra = '2024-01-01', datoTil = '2024-12-31',
                    outfile = 'degSponFusj1op_2024.pdf')

remotes::install_github('Rapporteket/rapbase', ref = 'main')
# Generelt: mce.PATIENT_ID kobles til patient.ID


test <- RyggRegDataSQLV2V3(datoFra = '2000-01-01')

liste <- unique(test[,c('SykehusNavn', 'AvdRESH')])
liste[order(liste$SykehusNavn),]
table(liste$SykehusNavn)

RyggData <- RyggPreprosess(RyggRegDataSQLV2V3(datoFra = '2000-01-01'))
test <- RyggData[,c("DodsDato", "InnDato", "PasientID", "ASA", "Alder")]

setwd('../data')
sship::dec('rygg16609ecfb.sql.gz__20251113_123205.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
setwd('../rygg')

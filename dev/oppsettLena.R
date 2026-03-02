##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)

setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/rygg1b1e973b.sql.gz__20260226_101925.tar.gz",
keyfile = "c://Users/lro2402unn/.ssh/id_rsa", target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/rygg1b1e973b.sql;


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)
library(rygg)


RegDataV3AVN <- rapbase::loadRegData(registryName = 'data',
                                     query='SELECT * FROM allevarnum')
RegData <- RyggHentRegDataV3()
RegData <- RyggPreprosess(RegData = RegData)
sort(names(RegData))
setdiff(names(RegData), names(RegDataV3AVN))
setdiff(names(RegDataV3AVN), names(RegData))


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

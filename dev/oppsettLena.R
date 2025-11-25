##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)
library(rygg)



remotes::install_github('Rapporteket/rapbase', ref = 'main')
# Generelt: mce.PATIENT_ID kobles til patient.ID


test <- RyggRegDataSQLV2V3(datoFra = '2000-01-01')

liste <- unique(test[,c('SykehusNavn', 'AvdRESH')])
liste[order(liste$SykehusNavn),]
table(liste$SykehusNavn)

RyggData <- RyggPreprosess(RyggRegDataSQLV2V3(datoFra = '2000-01-01'))
test <- RyggData[,c("AvDodDato", "InnDato", "PasientID", "ASA", "Alder")]

setwd('../data')
sship::dec('rygg16609ecfb.sql.gz__20251113_123205.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
setwd('../rygg')

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)


setwd('../data')
setwd('../rygg')
# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

remotes::install_github('Rapporteket/rapbase', ref = 'main')

test <- RyggRegDataSQLV2V3(datoFra = '2019-01-01')
liste <- unique(test[,c('SykehusNavn', 'AvdRESH')])
liste[order(liste$SykehusNavn),]
table(liste$SykehusNavn)
source("dev/sysSetenv.R")
library(rygg)
RyggData <- RyggPreprosess(RyggRegDataSQLV2V3(datoFra = '2020-01-01'))
alleResh <- unique(RyggData$ReshId)

for (resh in alleResh) {
  #reshID <- alleResh[1]
  reshID <- resh
  print(reshID)
#  knitr::knit2pdf(input='inst/RyggMndRapp.Rnw')
  test <- c(test, testing(RegData=RyggData, reshID=reshID))
}

sship::dec('rygg16609ecfb.sql.gz__20251113_123205.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")

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

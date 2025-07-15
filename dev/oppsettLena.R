##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)

test <- rygg::RyggRegDataSQLV2V3()

setwd('../data')
setwd('../rygg')
# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

remotes::install_github('Rapporteket/rapbase', ref = 'main')


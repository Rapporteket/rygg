##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

rygg::kjorRyggApp()

test <- rygg::RyggRegDataSQLV2V3()

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

remotes::install_github('Rapporteket/rapbase', ref = 'main')


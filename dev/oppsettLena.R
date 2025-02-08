
Sys.setenv(FALK_EXTENDED_USER_RIGHTS= "[{\"A\":86,\"R\":\"SC\",\"U\":110734},{\"A\":86,\"R\":\"LU\",\"U\":110734},{\"A\":86,\"R\":\"LU\",\"U\":108172},{\"A\":86,\"R\":\"LU\",\"U\":103575}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nger/data-raw/config")
# Sys.unsetenv("MYSQL_PORT_LOG")
Sys.setenv(MYSQL_DB_DATA="ngerReportDataStaging")
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

nger::kjor_NGERapp()

dum <- NGERRegDataSQL(datoFra = '2022-01-01')
RegData <- NGERPreprosess(dum)
rm('RegData')

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

nordicscir::kjor_NSapper(register='nordicscir', browser = TRUE)

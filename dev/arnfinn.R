devtools::install("../rapbase/.", upgrade = FALSE)
devtools::install("../nakke/.", upgrade = FALSE)
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/rygg348a4f65b8b0.sql.gz__20250213_134101.tar.gz", keyfile = "p://.ssh/id_rsa", target_dir = "c://Users/ast046/Downloads/.")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor
Sys.setenv(R_RAP_CONFIG_PATH="c://Users/ast046/repo/rapporteket/rygg/dev/config")

rygg::kjorRyggApp()

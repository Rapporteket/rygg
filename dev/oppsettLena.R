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
# OK:  "PasientID"               "AvdRESH"                 "ForlopsID"               "MceType"
[5] "HelseRegion"             "Kjonn"                   "PasientDod"              "DodsDato"
[9] "HoydeMangler"            "VektMangler"             "EqgangeV3Pre"            "EqperstV3Pre"
[13] "EqvanlgjV3Pre"           "EqsmerteV3Pre"           "EqangstV3Pre"            "EQ5DV3Pre"
[17] "HelsetilstPreMangler"    "Morsmal"                 "Ferdig1a"                "ForstLukketAVPreOp"
[21] "ForstLukketPreOp"        "OprProlap"               "OpProOsteotomi"          "OpFusjonPerkutan"
[25] "OpFusjonUtenDekomprV3"   "OpKileOsteotomi"         "OpAnnenOsteotomi"        "OpAnnenOstetosyntSpes"
[29] "FusjonKirPlfV3"          "FusjonKirPlfInstrV3"     "FusjonKirPlfIkkeInstrV3" "LiggetidPostOp"
[33] "DodsfallOpphold"         "MedForstLukketAV"        "MedForstLukket"          "Ferdig2a"
[37] "ForstLukketAVMed"        "ForstLukketMed"          "Utfdato3mnd"             "KpBlod3Mnd"
[41] "KpUVI3Mnd"               "KpLungebet3Mnd"          "KpDVT3Mnd"               "KpLE3Mnd"
[45] "KpInfOverfla3Mnd"        "KpInfDyp3Mnd"            "EqgangeV33mnd"           "EqperstV33mnd"
[49] "EqvanlgjV33mnd"          "EqsmerteV33mnd"          "EqangstV33mnd"           "EQ5DV33mnd"
[53] "Arbstatus3mndV3"         "Ferdigstilt1b3mnd"       "ForstLukketAV3mnd"       "ForstLukket3mnd"
[57] "Utfdato12mnd"            "EqgangeV312mnd"          "EqperstV312mnd"          "EqvanlgjV312mnd"
[61] "EqsmerteV312mnd"         "EqangstV312mnd"          "EQ5DV312mnd"             "Helsetilst12mnd"
[65] "Arbstatus12mndV3"        "Ferdigstilt1b12mnd"      "ForstLukketAV12mnd"      "ForstLukket12mnd"
RegData$He

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

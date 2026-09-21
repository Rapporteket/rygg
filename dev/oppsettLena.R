##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)

setwd('../data')
setwd('C:/Users/lro2402unn/RegistreGIT/rygg')

sship::dec("c://Users/lro2402unn/RegistreGIT/data/nger136b6fd74.sql.gz__20260910_142843.tar.gz",
keyfile = "c://Users/lro2402unn/.ssh/id_rsa", target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
# source c://Users/lro2402unn/RegistreGIT/data/nger136b6fd74.sql;


source("dev/sysSetenv.R")
rygg::kjorRyggApp(browser = TRUE)
library(rygg)

RegDataRaa <- RyggRegDataV2V3(datoFra = '2019-01-01')
RegData <- RyggPreprosess(RegData =RegDataRaa)

plot(RegData$OpDato, RegData$Variabel,
     xlab = 'Operasjonsdato',
     ylab = 'Dager',
     main = 'Dager fra operasjon til besvart 3mnd-skjema',
     ylim = c(0,1000))
range(RegData$OpDato, na.rm = T)
#oversikt over hvordan tidspunkt for besvarelse fordeler seg etter skjemautsending.
#For både 3 og 12 måneder: scatterplot eller tilsvarende som viser hvor lang tid
#etter skjemautsending at skjemaet blir besvart?

LegeSkjema <- hentDataTabellV3()
Skjema3mnd <- hentDataTabellV3(tabellnavn = "patientfollowup3")
Lege3mnd <- merge(LegeSkjema,
                  Skjema3mnd, by = "MCEID", all = F, suffixes = c("", "_3mnd"))

test <- Lege3mnd[ ,c("OpDato", 'TSCREATED', 'TSUPDATED', 'TSCREATED_3mnd',
                     'TSUPDATED_3mnd', 'FIRST_TIME_CLOSED_3mnd', 'UtfyltDato3mnd')]
RegDataRaa <- RyggRegDataV2V3(datoFra = '2025-01-01')
RegData <- RyggPreprosess(RegData =RegDataRaa)
RegData <- RyggUtvalgEnh(RegData=RegData, datoTil='2025-12-31')$RegData
table(RegData$EndoSkopTilg)

# trombProfyl, trombProfylLettKI
RyggFigAndelerGrVar(RegData=RegData, valgtVar='trombProfylLettKI', erMann='', preprosess = 0)


# Aleris Drammen (4211881)
# Aleris Drammen (107240) V2

unique(RegDataV3[grep('Aleris', RegDataV3$SykehusNavn) ,c("SykehusNavn", "ReshId")])


test <- RegData[ ,c(grep(pattern = 'dato',names(RegData), ignore.case = T))]
#FormatTrøbbel: DECEASED_DATE, REGISTERED_DATE, InnlagtDato,
head(test)
head(RegData$OpDato)

RyggFigAndelerGrVar(RegData=RegData,  valgtVar='degSponFusj1op',
                    datoFra = '2024-01-01', datoTil = '2024-12-31',
                    outfile = 'degSponFusj1op_2024.pdf')

remotes::install_github('Rapporteket/rapbase', ref = 'main')
# Generelt: mce.PATIENT_ID kobles til patient.ID


test <- RyggRegDataV2V3(datoFra = '2000-01-01')
names(RegDataV2)[grep('dato', names(RegDataV2), ignore.case = T)]
liste <- unique(test[,c('SykehusNavn', 'AvdRESH')])
liste[order(liste$SykehusNavn),]
table(liste$SykehusNavn)

RyggData <- RyggPreprosess(RyggRegDataV2V3(datoFra = '2000-01-01'))
test <- RyggData[,c("DodsDato", "OpDato", "PasientID", "ASA", "Alder")]

reshID <- 110633
4211878
Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/rygg/api/centre-information')
TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits

setwd('../data')
sship::dec('rygg16609ecfb.sql.gz__20251113_123205.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
           target_dir = "c://Users/lro2402unn/RegistreGIT/data/.")
setwd('../rygg')

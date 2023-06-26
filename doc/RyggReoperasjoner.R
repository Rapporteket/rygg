#Alle data
AlleDataSml <- read.table(file = 'C:/Registerdata/rygg/RyggV2V3_2023_01_01.csv', sep=';', header=T) #'UTF-8', stringsAsFactors = FALSE, na.strings = "NULL",
AlleData <- read.table(file = 'C:/Registerdata/rygg/RyggV2V3_2023_01_03.csv', sep=';', header=T) #'UTF-8', stringsAsFactors = FALSE, na.strings = "NULL",
#AlleData$OpDato <- as.Date(AlleData$OpDato, format='%d.%m.%Y')
AlleData <- AlleData[ ,-which(names(AlleData) %in% fritxtVar)]
#dataDump <- finnReoperasjoner(RegData = AlleData)

N2 <- sort(names(AlleData))
N1 <- sort(names(AlleDataSml))
setdiff(sort(names(AlleData)), sort(names(AlleDataSml)))

# Idetifisere alle med >1 operasjon OG de med NyRyggOpr3mnd=1 (V3). Se også på Reop90 for V2
# Reoperasjon innen 90d regnes som en komplikasjon til første inngrep.
# Vi skal derfor markere at første operasjon har en reoperasjon. Den markeres altså ikke på reoperasjonen, men på den foregående.
# Variabler: ErReop, BlittReop eller noe sånt.

RegData <- rygg::finnReoperasjoner(AlleData)
test <- FusjFlereOp[ ,c('OpDato', 'PID', 'OpNr', "DagerNesteOp",
                         "NyRyggOpr3mnd", "Reop90d", "Reop90dEtterOp")]
library(magrittr)
library(tidyverse)

#Se nærmere på fusjonskirurgi. Hvilke typer operasjon ETTER fusjon.
PIDflereop <- unique(RegData$PID[RegData$OpNr>1])
PIDfusj <- unique(RegData$PID[RegData$HovedInngrepV2V3==5])
PIDfusjFlereOp <- intersect(PIDflereop, PIDfusj)

FusjFlereOp <- RegData[RegData$PID %in% PIDfusjFlereOp, ]
write.csv2(FusjFlereOp, file = 'FusjFlereOp.csv')

#Finne de som har fusjon før annen operasjon
Nok å Fjerne de som har fusj bare i siste op?


table(table(FusjFlereOp$PID))
RegData <- AlleData #%>%
# filter(PID %in% names(table(PID)[table(PID)>1]))



MedNyeVar <- FlereOp %>%
  by(PID) %>%
  dplyr::summarise(
    AntOp = n())

    ,OpDato = OpDato
    ,ReinnTidDum = difftime(sort(OpDato)[2:AntOp],
                           OpDato[order(OpDato)][1:(AntOp-1)],
                           units = "days"))
sum(is.na(FlereOp$OpDato))

names(table(MedNyeVar$AntOp))




# Rydding i reoperasjoner rygg:
#   Målsetning:
#    Identifisere alle som er operert med fusjonskirurgi (HovedInngrepV2V3=5)
PIDopFusj <- unique(AlleData$PID[AlleData$HovedInngrepV2V3==5])
FusjData <- AlleData %>% filter(PID %in% PIDopFusj)             #HovedInngrepV2V3==5)
#    angi hvilken type operasjon som ble utført (HovedInngrepV2V3= ?).
table(FusjData$HovedInngrepV2V3)
# og som senere er operert på nytt, Dvs. en ny operasjon ETTER at operert med fusjon (HovedInngrepV2V3==5)?

?annet enn:
table(PID)[table(PID)>1]
# Fremgangsmåte:
#   Dette bør nok gjøres separat i Versjon 2.0 og 3.0
# En pasient (PID) kan ha flere rader (operasjoner: OpDato) i rapporteket.
# •	Identifisere alle med PID frekvens >1
# •	Hos alle med PID>1: nummere (rangere) inngrep basert på oprerasjonsdato (tidligste dato=1, neste operasjonsdato lik 2 osv.)
# •	Definere operasjoner som er reoperasjon innen 90 dager. Disse skal kategoriseres merkes som reoperert innen 90 dager (Reop90d=1) knyttet til foregående operasjon.



# Sjekk:
#   Blir det haket av for Reop90d=1 (postoperativt skjema 3Mnd., Versjon 3.0)?
#   Hvilken variabel???
#
# Andre tema:
#   •	Identifisere de som er reoperert innen 3-12 mnd. De vi ikke få 12 mnd, men starte på ny case.
# •	Identifisere alle med PID=1 som er tidligere operert (TidlOpr=1).





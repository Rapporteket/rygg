#-------------- Behandling av reoperasjoner etter prolaps---------------
#Ønsker å se på om gjentatte reoperasjoner for prolaps behandles med fusjon eller ny prolapskirugi.
# 01.01.2010- 31.12.2024.
# Lagre totaldata
# Legg på operasjonsnummer

library(rygg)
source("dev/sysSetenv.R")
#Dato for nedlasting fra Rapporteket: 11.mai 2026
RegDataRaa <- RyggRegDataV2V3(datoFra = '2010-01-01') #
RegData <- RyggPreprosess(RegData =RegDataRaa)
RegData <- RyggUtvalgEnh(RegData=RegData, datoTil='2024-12-31')$RegData
RegData <- rygg::finnReoperasjoner(RegData)
RegDataAlle <- RegData #80448 operasjoner
write.table(RegDataAlle, file = '../data/NKR/Rygg2010_24_uttr2026-05-11.csv', row.names = F, col.names = T, sep = ';')

# Pasienter som har minst en prolapsoperasjon (HovedInngrepV2V3 = 1)
pasMpro <- unique(RegData$PasientID[which(RegData$HovedInngrepV2V3==1)]) #30884 pasienter
RegData <- RegData[RegData$PasientID %in% pasMpro, ] #36659 operasjoner

#Lagres som Rygg_utv1 og ekskluderte som RegData_ekskl1
write.table(RegData, file = '../data/NKR/Rygg_utv1.csv', row.names = F, col.names = T, sep = ';')
RegData_ekskl1 <- RegDataAlle[!(RegDataAlle$PasientID %in% pasMpro), ] #43789 operasjoner
write.table(RegData_ekskl1, file = '../data/NKR/Rygg_ekskl1.csv', row.names = F, col.names = T, sep = ';')

#OpNr som er første prolapsoperasjon HovedInngrepV2V3 = 1 og
#antall (AntNivOpr = 1 eller DekompAntNivaa= 1 eller summen av OpTh12L10+OpL1L2 +OpL23+ OpL34+ OpL45+ OpL5S1=1).
#Dvs bare operert i ett nivå
#summen av OpTh12L10+OpL1L2 +OpL23+ OpL34+ OpL45+ OpL5S1=1) er nå def som AntNivOpr i V3.
#test <- RegData[ ,c('OpTh12L1', 'OpL1L2', 'OpL23', 'OpL34', 'OpL45', 'OpL5S1', "AntNivOpr", 'DekompAntNivaa')]

# Fjern alle OpNr for hver pasient (PID) som kommer før første prolapsoperasjon. Ta vare på disse i egen fil («ekskludert_2»).

# Tar bort alle pasienter som ikke har en operasjon i ett nivå. Hvis de ikke har noen med ett nivå, har de heller ikke ett nivå i første.
indEttNiv <- unique(which(RegData$AntNivOpr == 1),  which(RegData$DekompAntNivaa == 1)) #33284 operasjoner
pasPro1nivaa <- unique(RegData$PasientID[intersect(which(RegData$HovedInngrepV2V3==1), indEttNiv)]) #28555 pasienter
RegData <- RegData[RegData$PasientID %in% pasPro1nivaa, ] #34012 operasjoner
#Ant pasienter uten minst en prolapsoperasjon i ett nivå: 30884-28555=2329 pasienter

test <- RegData[,c('PasientID', 'HovedInngrepV2V3', 'AntNivOpr', 'DekompAntNivaa', 'OpNr')]

PasOpNr <- RegData |>
  dplyr::group_by(PasientID) |>
  dplyr::summarise(
    ForstePro = min(OpNr[HovedInngrepV2V3==1]), #Første prolapsoperasjon
    ForstePro1 = min(OpNr[HovedInngrepV2V3==1 & (AntNivOpr == 1 | DekompAntNivaa == 1)], na.rm = T),
    ProNrIndex = ifelse(ForstePro==ForstePro1, ForstePro1, 0), #OpNr for de som har første prolapsoperasjon i bare ett nivå.
    N = dplyr::n()
  )
# test[test$PasientID==16820, ]
# test[test$PasientID==59760, ]
# test[test$PasientID==4106, ]

#Tore: Finn første prolapsoperasjon (pasienter som ikke har en første prolapsoperasjon etterfulgt av en ny operasjon skal fjernes)
#De som nå står igjen  med en første  prolapsoperasjon  kun i ett nivå skal med videre

#Fjerner pasienter som har har første prolapsoperasjon i flere nivå
RegData <- RegData[RegData$PasientID %in% PasOpNr$PasientID[PasOpNr$ProNrIndex>0], ] # 33694 operasjoner

#Fjerner operasjoner før første prolapsoperasjon på ett nivå:
RegData <- merge(RegData, PasOpNr[ ,c('PasientID', "ProNrIndex", 'N')], by = 'PasientID')
RegData <- RegData[RegData$OpNr>=RegData$ProNrIndex, ] # 33001 operasjoner
#test <- RegData[,c('PasientID', 'HovedInngrepV2V3', 'AntNivOpr', 'DekompAntNivaa', 'OpNr','ProNrIndex', 'NrOk')]

#Fjerner operasjoner hvor pasienten ikke har noen operasjon etter prolapsoperasjon i ett nivå
RegData <- RegData[RegData$ProNrIndex < RegData$N, ] #8447 operasjoner, 3862 pasienter

#Legger på nye operasjonsnummer:
RegData$OpNrGml <- RegData$OpNr
RegDataSort <-RegData[order(RegData$PasientID, RegData$OpDato), ]
RegDataSort$OpNr <- ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=seq_along)

write.table(RegDataSort, file = '../data/NKR/Rygg_prolapsReop.csv', row.names = F, col.names = T, sep = ';')

test <- RegDataSort[,c('PasientID', 'HovedInngrepV2V3', 'AntNivOpr', 'DekompAntNivaa','ProNrIndex','OpNr', "OpNrGml")]

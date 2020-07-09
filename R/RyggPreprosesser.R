#' Preprosesser data fra Degenerativ Rygg
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @inheritParams RyggFigAndeler
#'
#' @export

RyggPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer: Det skal kun leveres ferdigstilte skjema til RapportUttrekk
	#Kjønnsvariabel:Kjonn 1:mann, 2:kvinne
  #Kjonn Mangler!!
  RegData$ErMann <- RegData$Kjonn
  RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')
	RegData$Aar <- lubridate::year(RegData$InnDato)

	#Endre variabelnavn:
	#names(RegData)[which(names(RegData) == 'OpAar')] <- 'Aar'
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'
	# if ('FistTimeClosed' %in% names(RegData)) {
	#   RegData <- dplyr::rename(RegData, 'FirstTimeClosed' = 'FistTimeClosed')}
	#if ('TdllOpAnnetNiv' %in% names(RegData)) {
	#  RegData <- dplyr::rename(RegData, 'TidlOpAnnetNiv' = 'TdllOpAnnetNiv')}
	#RegData <- dplyr::rename(RegData, 'LiggetidPostOp' = 'surgeonform_LIGGEDOEGN_POSTOPERATIV',
	#                         'Liggedogn' = 'surgeonform_LIGGEDOEGN_TOTALT')



	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'

	# Nye variable:
	#RegData$MndNum1 <- RegData$InnDato$mon +1
	RegData$MndNum <- as.numeric(format(RegData$InnDato, '%m'))
	RegData$MndAar <- format(RegData$InnDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	#?Trenger kanskje ikke de over siden legger på tidsenhet når bruk for det.
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$MedForstLukket), RegData$UtskrivelseDato,units = 'days'))
	variable <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpUVI3Mnd',
	              'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd')
	RegData$Kp3Mnd <- NULL
	RegData$Kp3Mnd[rowSums(RegData[ ,c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpUVI3Mnd',
	                                   'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd')],
	                       na.rm = T) > 0] <- 1


	#1:4,9 c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
	#TidlIkkeOp, TidlOpAnnetNiv, TidlOpsammeNiv
	RegData$TidlOpr <- 9
	RegData$TidlOpr[RegData$TidlIkkeOp==1] <- 4
	RegData$TidlOpr[RegData$TidlOpsammeNiv==1] <- 1
  RegData$TidlOpr[RegData$TidlOpAnnetNiv==1] <- 2
  RegData$TidlOpr[RegData$TidlOpsammeNiv==1 & RegData$TidlOpAnnetNiv==1] <- 3
	#  table(RegData$TidlOpr)
#Data <- RegData[,c('TidlIkkeOp', 'TidlOpAnnetNiv', 'TidlOpsammeNiv')]

	#Formatering
	RegData$ShNavn <- as.character(RegData$ShNavn)



#Legge til underkategori for hovedkategori.
ny <- kategoriserInngrep(RegData=RegData)
RegData <- ny$RegData

#--------Definasjon av diagnosegrupper prolaps og spinal stenose V3
# COMPUTE filter_$=(HovedInngrepV2V3 = 4 or (RfSentr = 1 or RfLateral = 1 or RfForaminalSS = 1)
#                   & (OpDeUlamin = 1 or OpDeUlaminTilgang > 0 or OpLaminektomi  = 1)
#                   & (HovedInngrepV2V3 = 2 or HovedInngrepV2V3 = 3
#                                            or HovedInngrepV2V3 = 5 or HovedInngrepV2V3 = 7) ).
RegData$LSSopr <- ifelse(RegData$HovedInngrepV2V3 == 4
                         | (RegData$RfSentr == 1 | RegData$RfLateral == 1 | RegData$RfForaminalSS == 1)
                         & (RegData$OpDeUlamin == 1 | RegData$OpDeUlaminTilgang %in% 1:3 | RegData$OpLaminektomi == 1)
                         & (RegData$HovedInngrepV2V3 %in% c(2,3,5,7)),
                         1, 0)

#*Definisjon av prolapsgruppen, dekompresjon, kvalitetssikre, først gr uten fusjon..
#COMPUTE filter_$=(HovedInngrepV2V3 = 1 &  LSS_opr = 0).
# 1  'Ja operert med dekopressjon for prolaps'
# 0 ' Nei ikke operert med dekopressjon for prolaps'.
RegData$ProlapsDekr <- ifelse(RegData$HovedInngrepV2V3 == 1 &  RegData$LSSopr == 0, 1, 0)

#*Definisjon av prolapsgruppen,med fusjon.
# 1  'Ja operert med fusjon for prolaps'
# 0 ' Nei ikke operert med fusjon for prolaps'.
# COMPUTE filter_$=((Prolaps_dekr = 0 & LSS_opr = 0) & (HovedInngrepV2V3 = 5 & OprProlap > 0) &
#                     (RfDegskol =  0 & RfSpondtypeDegen = 0 & RfSpondtypeIsmisk = 0)).
RegData$ProlapsFusjonert <-
  ifelse((RegData$ProlapsDekr == 0 & RegData$LSSopr == 0) &
           (RegData$HovedInngrepV2V3 == 5 & RegData$OprProlap > 0) &
           (RegData$RfDegskol ==  0 & RegData$RfSpondtypeDegen == 0 & RegData$RfSpondtypeIsmisk == 0),
         1, 0)

#*Definisjon av prolapsgruppen, prolapsopr med og uten fusjon.
#COMPUTE filter_$=(Prolaps_dekr = 1 or Prolaps_fusjonert = 1).
# VARIABLE LABELS  Prolapsopr_alle 'både dekr og fusjon'.
# 1  'Ja  alle typer prolapsoperason'
# 0 ' Nei ikke operert for prolaps'.
RegData$ProlapsoprAlle <- ifelse(RegData$ProlapsDekr == 1 | RegData$ProlapsFusjonert == 1, 1, 0)


# DO IF (LSS_opr = 0 & Prolapsopr_alle = 0   & OpDeUlamin = 1).
# RECODE LSS_opr (0=1).
utvalg <- which(RegData$LSSopr == 0 & RegData$ProlapsoprAlle == 0   & RegData$OpDeUlamin == 1)
RegData$LSSopr[utvalg] <- 1

  return(invisible(RegData))
}


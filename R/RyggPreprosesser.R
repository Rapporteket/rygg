#' Preprosesser data fra Degenerativ Rygg
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @inheritParams RyggFigAndeler
#'
#' @export

RyggPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer: Kun ferdigstilte skjema i V2
  #V3: Alle legeskjema ferdigstilt.
	#Kjønnsvariabel:Kjonn 1:mann, 2:kvinne
  RegData$ErMann <- RegData$Kjonn
  RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')

	#Endre variabelnavn:
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	RegData$ShNavn <- as.character(RegData$ShNavn)

	# Nye variable:
	RegData$Aar <- lubridate::year(RegData$InnDato)
	RegData$MndNum <- as.numeric(format(RegData$InnDato, '%m'))
	RegData$MndAar <- format(RegData$InnDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	#?Trenger kanskje ikke de over siden legger på tidsenhet når bruk for det.
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$MedForstLukket), RegData$UtskrivelseDato,units = 'days'))
	# RegData$Versjon <- 2
	# RegData$Versjon[RegData$OpDato < '2009-09-01'] <- 1
	# RegData$Versjon[RegData$OpDato >= '2019-01-01'] <- 3

	  return(invisible(RegData))
}


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
	#RegData$AvdodDato <- as.Date(RegData$AvdodDato, format="%Y-%m-%d")

	#Endre variabelnavn:
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	RegData$ShNavn <- as.character(RegData$ShNavn)

	#Tomme sykehusnavn får resh som navn:
	indTom <- which(is.na(RegData$ShNavn) | RegData$ShNavn == '')
	RegData$ShNavn[indTom] <- RegData$ReshId[indTom]

	#Sjekker om alle resh har egne enhetsnavn
	dta <- unique(RegData[ ,c('ReshId', 'ShNavn')])
	duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
	duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn)>1)])

	if (length(c(duplSh, duplResh)) > 0) {
	  ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
	  RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind],' (', RegData$ReshId[ind], ')')
	}


	# Nye variable:
	RegData$Aar <- lubridate::year(RegData$InnDato)
	RegData$MndNum <- as.numeric(format(RegData$InnDato, '%m'))
	RegData$MndAar <- format(RegData$InnDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	#?Trenger kanskje ikke de over siden legger på tidsenhet når bruk for det.
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$MedForstLukket), as.Date(RegData$UtskrivelseDato), units = 'days'))
	RegData$DiffUtfOp <- as.numeric(difftime(as.Date(RegData$UtfyltDato), as.Date(RegData$OpDato), units = 'days'))
	#RegData$DiffUtfLukket <- as.numeric(difftime(as.Date(RegData$MedForstLukket), as.Date(RegData$UtfyltDato), units = 'days'))
	#RegData$DiffLukketOp <- as.numeric(difftime(as.Date(RegData$MedForstLukket), as.Date(RegData$OpDato), units = 'days'))
	#RegData$Overlevelse <- as.numeric(RegData$AvdodDato - RegData$InnDato)
	RegData$Dod30 <- 0
	RegData$Dod30[which(as.numeric(as.Date(RegData$AvdodDato) - as.Date(RegData$InnDato)) < 30)] <- 1
	RegData$Dod365 <- 0
	RegData$Dod365[which(as.numeric(as.Date(RegData$AvdodDato) - as.Date(RegData$InnDato)) < 365)] <- 1

	  return(invisible(RegData))
}


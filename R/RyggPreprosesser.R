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
  #Mangler:	 RegData$ErMann <- RegData$Kjonn
  #Mangler:	 RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.POSIXlt(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')
	RegData$Aar <- lubridate::year(RegData$InnDato)

	#Endre variabelnavn:
	#names(RegData)[which(names(RegData) == 'OpAar')] <- 'Aar'
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'


	# Nye variable:
	#Trenger kanskje ikke disse siden legger på tidsenhet når bruk for det.
	RegData$MndNum <- RegData$InnDato$mon +1
	RegData$MndAar <- format(RegData$InnDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'
	class(RegData$ReshId) <- 'numeric'

	#Formatering
	RegData$Morsmal <- factor(RegData$Morsmal, levels=1:3)
#Mangler:	RegData$HovedInngrep <- factor(RegData$HovedInngrep, levels=0:7)
#Mangler:		RegData$Inngrep <- factor(RegData$Inngrep, levels=0:19)
	RegData$SivilStatus <- factor(RegData$SivilStatus, levels=1:3)
	#RegData$ASA <- factor(RegData$ASA, levels=1:4)
	RegData$Utd <- factor(RegData$Utd, levels=1:5)
	RegData$ArbstatusPre <- factor(RegData$ArbstatusPre, levels=1:10)
	RegData$UforetrygdPre <- factor(RegData$UforetrygdPre, levels=1:4)
	RegData$ErstatningPre <- factor(RegData$ErstatningPre, levels=1:4)
	RegData$SymptVarighRyggHof <- factor(RegData$SymptVarighRyggHof, levels=1:5)
	RegData$SympVarighUtstr <- factor(RegData$SympVarighUtstr, levels=1:5)
	RegData$OpKat <- factor(RegData$OpKat, levels=1:3)


#Legge til underkategori for hovedkategori.
#	if (is.na(match("Inngrep", names(opdata))) != 'TRUE') {	#Hvis har variabelen Inngrep
#	      #if (match("Inngrep", names(opdata))) {	#Hvis har variabelen Inngrep
#
#	      #Dataramme av hovedkategorier og underkategorier
#	      gr_nr <- c(0:19)
#	      txt <- c('Annet','Mikro','Makro','Tubekirurgi','Udefinert','Mikro','Makro','Tubekirurgi',
#	               'Udefinert','Laminektomi', 'Interspinøst impl.','PLF','PLIF','TLIF','ALIF',
#	               'Udefinert fusjon', 'Skiveprotese','Fjern interspinøst impl.','Fjerne ostemat.',
#	               'Revisjon ostemat.')
#	      hgr <- c(0,1,1,1,1,2,2,2,2,3,4,5,5,5,5,5,6,7,7,7)
#	      kat <- data.frame(hgr, hkatnavn[hgr+1], gr_nr, txt)
#	      underkattxt <- ''
#	      underkat_num <- ''
#
#	      #Velge ut riktige underkategorier:
#	      if (hovedkat != 99) {
#	            underkat_num <- kat$gr_nr[kat$hgr==hovedkat]
#	            opdata_ok <- opdata[which(!is.na(match(opdata$Inngrep,underkat_num))),]
#	            opdata <- opdata_ok
#	            underkattxt <- as.character(kat$txt[underkat_num+1])
#	      }
#	      names(kat) <- c('Hnr', 'Hnavn', 'Unr', 'Unavn')
#	      utdata <- list(opdata, hkattxt, underkattxt, underkat_num, kat)
#	      names(utdata) <- c('data','txt','ukattxt','underkat', 'inngrHinngr')


  return(invisible(RegData))
}


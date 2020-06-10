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
  RegData$ErMann <- 9
  #RegData$ErMann <- RegData$Kjonn
  #RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')
	RegData$Aar <- lubridate::year(RegData$InnDato)

	#Endre variabelnavn:
	#names(RegData)[which(names(RegData) == 'OpAar')] <- 'Aar'
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'
	if ('FistTimeClosed' %in% names(RegData)) {
	  RegData <- dplyr::rename(RegData, 'FirstTimeClosed' = 'FistTimeClosed')}
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
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$FirstTimeClosed), RegData$UtskrivelseDato,units = 'days'))

	#1:4,9 c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
	#TidlIkkeOp, TidlOpAnnetNiv, TidlOpsammeNiv
	RegData$TidlOpr <- 9
	RegData$TidlOpr[RegData$TidlOpsammeNiv==1] <- 1
  RegData$TidlOpr[RegData$TidlOpAnnetNiv==1] <- 2
  RegData$TidlOpr[RegData$TidlOpsammeNiv==1 & RegData$TidlOpAnnetNiv==1] <- 3
	RegData$TidlOpr[RegData$TidlIkkeOp==1] <- 4
	#  table(RegData$TidlOpr)
#Data <- RegData[,c('TidlIkkeOp', 'TidlOpAnnetNiv', 'TidlOpsammeNiv')]

	#Formatering
	RegData$ShNavn <- as.character(RegData$ShNavn)
#	RegData$Morsmal <- factor(RegData$Morsmal, levels=1:3)
#Mangler:	RegData$HovedInngrep <- factor(RegData$HovedInngrep, levels=0:7)
#Mangler:		RegData$Inngrep <- factor(RegData$Inngrep, levels=0:19)
	#RegData$SivilStatus <- factor(RegData$SivilStatus, levels=1:3)
	#RegData$ASA <- factor(RegData$ASA, levels=1:4)
#	RegData$Utd <- factor(RegData$Utd, levels=1:5)
	#RegData$ArbstatusPre <- factor(RegData$ArbstatusPre, levels=1:10)
#	RegData$UforetrygdPre <- factor(RegData$UforetrygdPre, levels=1:4)
	#RegData$ErstatningPre <- factor(RegData$ErstatningPre, levels=c(0:3,9)
	# RegData$SymptVarighRyggHof <- factor(RegData$SymptVarighRyggHof, levels=c(1:5,9))
	# RegData$SympVarighUtstr <- factor(RegData$SympVarighUtstr, levels=c(1:5,9))


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


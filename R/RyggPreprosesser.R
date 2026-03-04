
tilpassV2data <- function(RegDataV2){

  #--------- Arbstatus3mnd OG Arbstatus12mnd V2:
  # De som har verdi 11 settes TIL MANGLENDE
  # 2: Hjemmeværende - bare i V2 - settes tom
  # 7:Delvis sykemeldt - V2: 7+8,
  # 8: Arbeidsavklaring - V2:9
  # 9: Ufør - V2: 10,

  RegDataV2$ArbstatusPreV2V3 <- as.numeric(RegDataV2$ArbstatusPre)
  RegDataV2$Arbstatus3mndV2V3 <- as.numeric(RegDataV2$Arbstatus3mnd)
  RegDataV2$Arbstatus12mndV2V3 <- as.numeric(RegDataV2$Arbstatus12mnd)
  RegDataV2$ArbstatusPreV2V3 <- plyr::mapvalues(RegDataV2$ArbstatusPreV2V3, from = c(2,8,9,10), to = c(NA,7,8,9))
  RegDataV2$Arbstatus3mndV2V3 <- plyr::mapvalues(RegDataV2$Arbstatus3mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))
  RegDataV2$Arbstatus12mndV2V3 <- plyr::mapvalues(RegDataV2$Arbstatus12mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))

  RegDataV2 <- RegDataV2[ , -which(names(RegDataV2) %in%
                                     c('ArbstatusPre', 'Arbstatus3mnd', 'Arbstatus12mnd'))]

  #SykemeldVarighPre V2-numerisk, V3 - 1: <3mnd, 2:3-6mnd, 3:6-12mnd, 4:>12mnd, 9:Ikke utfylt
  RegDataV2$SykemeldVarighPreV3 <- as.numeric(cut(as.numeric(RegDataV2$SykemeldVarighPre),
                                                  breaks=c(-Inf, 90, 182, 365, Inf),
                                                  right = FALSE, labels=c(1:4)))
  RegDataV2$SykemeldVarighPreV3[is.na(RegDataV2$SykemeldVarighPreV3)] <- 9

  RegDataV2$AntibiotikaV3 <-  plyr::mapvalues(as.numeric(RegDataV2$Antibiotika), from = c(0, 1, NA), to = c(0,1,9))

  #V2: Kode 1:4,NA: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
  #V3: [0,1,2,3,9]	["Nei","Ja","Planlegger","Innvilget","Ikke utfylt"]
  RegDataV2$ErstatningPre <- plyr::mapvalues(as.numeric(RegDataV2$ErstatningPre), from = c(2,3,4,NA), to = c(0,2,3,9))
  RegDataV2$UforetrygdPre <- plyr::mapvalues(as.numeric(RegDataV2$UforetrygdPre), from = c(2,3,4,NA), to = c(0,2,3,9))

  #Ønsker tom for manglende RegDataV2$SmBePre[is.na(RegDataV2$SmBePre)] <- 99 #99: Ikke utfylt i V3, NA i V2
  #Ønsker tom for manglende RegDataV2$SmRyPre[is.na(RegDataV2$SmRyPre)] <- 99 #99: Ikke utfylt i V3, NA i V2
  RegDataV2$OpIndPareseGrad[is.na(RegDataV2$OpIndPareseGrad)] <- 9
  RegDataV2$Roker[is.na(RegDataV2$Roker)] <- 9
  RegDataV2$Morsmal[is.na(RegDataV2$Morsmal)] <- 9
  RegDataV2$Utd[is.na(RegDataV2$Utd)] <- 9
  RegDataV2$KpInf3Mnd[RegDataV2$KpInf3Mnd==0] <- NA #Tilpasning til V3
  RegDataV2$Versjon <- 'V2'


  RegDataV2$AvdNavn <- plyr::revalue(RegDataV2$AvdNavn, c( #Gammelt navn V2 - nytt navn (V3)
    'Aleris, Bergen' = 'Aleris Bergen',
    'Aleris, Oslo' = 'Aleris Oslo',
    'Larvik' = 'Tønsberg',
    'Oslofjordklinikken Øst' = 'Oslofjordklinikken',
    'Teres Colloseum, Oslo' = 'Aleris Oslo',
    'Teres Colloseum, Stavanger'  = 'Aleris Stavanger',
    'Teres, Bergen' = 'Aleris Bergen',
    'Teres, Drammen' =  'Aleris Drammen'  ,
    'Ulriksdal' = 'Volvat',
    'UNN, nevrokir' = 'Tromsø')
  )

  # RegDataV2$AvdReshID <- plyr::revalue(RegDataV2$AvdReshID,  #Gammelt navn V2 - nytt navn (V3), dvs. gmlresh	nyresh
  #Mappes om i preprosess:  c('107511' =	'999975')) #Aleris Oslo
  # Disse ikke med i ny versjon: '107137' =	'107508', #Aleris Bergen '999999' =	'110771') #Volvat

  # Variabler med samme innhold i V2 og V3, men avvikende variabelnavn.
  # (navnV3 = navnV2) dvs. nytt navn, V3 = gammelt navn, V2
  RegDataV2 <- dplyr::rename(RegDataV2,
                             AlderVedOpr = Alder,
                             EQ5DV212mnd = EQ5D12mnd,
                             EQ5DV23mnd = EQ5D3mnd,
                             AvdRESH = AvdReshID,
                             Bydelskode = Bydelkode,
                             Bydelsnavn = Bydelsted,
                             KommuneNr = Kommunenr, #Kommunenavn ikke med i V2
                             KpInfDyp12mnd = KpInfDyp12Mnd,
                             # ForlopsID = MCEID,
                             #PIDV2 = PID, #Heter PasientID i V3. NB: Må ikke slås sammen.
                             PasientID = PATIENT_ID,
                             RokerV2 = Roker,
                             #Region = HelseRegion #Navn må evt. mappes om i ettertid. Private bare i V2.
                             SykehusNavn = AvdNavn,
                             Status3mnd = Utfylt3Mnd,
                             Status12mnd = Utfylt12Mnd,
                             SykDprebetesMellitus = SykdDiabetesMellitus
  )


  #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
  RegDataV2$SivilStatusV3 <- plyr::mapvalues(as.numeric(RegDataV2$SivilStatus),
                                             from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))
}


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
  RegData$ErMann <- RegData$GENDER
  RegData$ErMann[which(RegData$GENDER == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')

	#Endre variabelnavn:
	names(RegData)[which(names(RegData) == 'AlderVedOpr')] <- 'Alder'

	#Variabel som identifiserer avdelinga
	RegData$SykehusNavn <- trimws(RegData$SykehusNavn)
	indAleris <- which(RegData$ReshId %in% c(999975, 107511))
	RegData$SykehusNavn[indAleris] <- 'Aleris Oslo'
	RegData$ReshId[indAleris] <- 107511
	RegData$ShNavn <- RegData$SykehusNavn
	# names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'


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
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$ForstLukketLege),
	                                            as.Date(RegData$UtskrivelseDato), units = 'days'))
	RegData$DiffUtfOp <- as.numeric(difftime(as.Date(RegData$UtfyltDato),
	                                         as.Date(RegData$OpDato), units = 'days'))
	RegData$Dod30 <- 0
	RegData$Dod30[which(as.numeric(as.Date(RegData$DodsDato) - as.Date(RegData$InnDato)) < 30)] <- 1
	RegData$Dod365 <- 0
	RegData$Dod365[which(as.numeric(as.Date(RegData$DodsDato) - as.Date(RegData$InnDato)) < 365)] <- 1

	  return(invisible(RegData))
}


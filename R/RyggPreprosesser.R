
#' Justere V2-data
#'
#' @param RegDataV2
#'
#' @return dataramme med tilpassede V2-data
#' @export
#'

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
  RegDataV2 <- RegDataV2 |> dplyr::mutate(ArbstatusPre=NULL, Arbstatus3mnd=NULL, Arbstatus12mnd=NULL)
  RegDataV2$ArbstatusPreV2V3 <- dplyr::replace_values(RegDataV2$ArbstatusPreV2V3, from = c(2,8,9,10), to = c(NA,7,8,9))
  RegDataV2$Arbstatus3mndV2V3 <- dplyr::replace_values(RegDataV2$Arbstatus3mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))
  RegDataV2$Arbstatus12mndV2V3 <- dplyr::replace_values(RegDataV2$Arbstatus12mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))

  #SykemeldVarighPre V2-numerisk, V3 - 1: <3mnd, 2:3-6mnd, 3:6-12mnd, 4:>12mnd, 9:Ikke utfylt
  RegDataV2$SykemeldVarighPreV3 <- as.numeric(cut(as.numeric(RegDataV2$SykemeldVarighPre),
                                                  breaks=c(-Inf, 90, 182, 365, Inf),
                                                  right = FALSE, labels=c(1:4)))
  RegDataV2$SykemeldVarighPreV3[is.na(RegDataV2$SykemeldVarighPreV3)] <- 9

  RegDataV2$AntibiotikaV3 <-  dplyr::replace_values(as.numeric(RegDataV2$Antibiotika), from = c(0, 1, NA), to = c(0,1,9))

  #V2: Kode 1:4,NA: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
  #V3: [0,1,2,3,9]	["Nei","Ja","Planlegger","Innvilget","Ikke utfylt"]
  RegDataV2$ErstatningPre <- dplyr::replace_values(as.numeric(RegDataV2$ErstatningPre), from = c(2,3,4,NA), to = c(0,2,3,9))
  RegDataV2$UforetrygdPre <- dplyr::replace_values(as.numeric(RegDataV2$UforetrygdPre), from = c(2,3,4,NA), to = c(0,2,3,9))

  # Variabler med samme innhold i V2 og V3, men avvikende variabelnavn.
  # (navnV3 = navnV2) dvs. nytt navn, V3 = gammelt navn, V2
  RegDataV2 <- dplyr::rename(RegDataV2,
                             AlderVedOpr = Alder,
                             EQ5DV212mnd = EQ5D12mnd,
                             EQ5DV23mnd = EQ5D3mnd,
                             ReshId = AvdReshID,
                             Bydelskode = Bydelkode,
                             Bydelsnavn = Bydelsted,
                             KommuneNr = Kommunenr, #Kommunenavn ikke med i V2
                           #  KpInf3mnd = KpInf3Mnd,
                           #  KpInfDyp12mnd = KpInfDyp12Mnd,
                             PasientID = PATIENT_ID,
                             RokerV2 = Roker,
                             SykehusNavn = AvdNavn,
                             Status3mnd = Utfylt3Mnd,
                             Status12mnd = Utfylt12Mnd,
                             SykDprebetesMellitus = SykdDiabetesMellitus
  )
  EndreNavnInd <- grep('3Mnd', names(RegDataV2))
  names(RegDataV2)[EndreNavnInd] <- gsub("3Mnd", "3mnd", names(RegDataV2)[EndreNavnInd])

  RegDataV2$SykehusNavn <- dplyr::replace_values(RegDataV2$SykehusNavn,   #Gammelt navn V2 - nytt navn (V3)
                                             'Aleris, Bergen' ~ 'Aleris Bergen',
                                             'Aleris, Oslo' ~ 'Aleris Oslo',
                                             'Larvik' ~ 'Tønsberg',
                                             'Oslofjordklinikken Øst' ~ 'Oslofjordklinikken',
                                             'Teres Colloseum, Oslo' ~ 'Aleris Oslo',
                                             'Teres Colloseum, Stavanger'  ~ 'Aleris Stavanger',
                                             'Teres, Bergen' ~ 'Aleris Bergen',
                                             'Teres, Drammen' ~  'Aleris Drammen'  ,
                                             'Ulriksdal' ~ 'Volvat',
                                             'UNN, nevrokir' ~ 'Tromsø'
  )

  RegDataV2$ReshId <- dplyr::replace_values(RegDataV2$ReshId, '107240' ~ '4211881')

   #Ønsker tom for manglende RegDataV2$SmBePre[is.na(RegDataV2$SmBePre)] <- 99 #99: Ikke utfylt i V3, NA i V2
  #Ønsker tom for manglende RegDataV2$SmRyPre[is.na(RegDataV2$SmRyPre)] <- 99 #99: Ikke utfylt i V3, NA i V2
  RegDataV2$OpIndPareseGrad[is.na(RegDataV2$OpIndPareseGrad)] <- 9
  RegDataV2$Roker[is.na(RegDataV2$Roker)] <- 9
  RegDataV2$Morsmal[is.na(RegDataV2$Morsmal)] <- 9
  RegDataV2$Utd[is.na(RegDataV2$Utd)] <- 9
  RegDataV2$KpInf3mnd[RegDataV2$KpInf3mnd==0] <- NA #Tilpasning til V3
  RegDataV2$Versjon <- 'V2'

  #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
  RegDataV2$SivilStatusV3 <-dplyr::replace_values(as.numeric(RegDataV2$SivilStatus),
                                             from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

  table(RegDataV2$SivilStatusV3GML, useNA = 'a')
  return(RegDataV2)
}




#' Justere V3-data
#'
#' @param RegDataV3
#'
#' @return dataramme med tilpassede V3-data
#' @export
#'
tilpassV3data <- function(RegDataV3){
  #-----Tilrettelegg V3-data-------------------------


  #I perioden 2019-21 ble ikke dyp og overfladisk sårinfeksjon registrert.
  RegDataV3[which(RegDataV3$OpDato >= '2019-01-01' & RegDataV3$OpDato <= '2021-12-31'),
            c('KpInfOverfla3mnd', 'KpInfDyp3mnd', 'KpInfDyp12mnd')] <- NA


  #Ønsker tom for manglende
  RegDataV3$SmBePre[RegDataV3$SmBePre == 99] <- NA #99: Ikke utfylt i V3, NA i V2
  RegDataV3$SmRyPre[RegDataV3$SmRyPre == 99] <- NA #99: Ikke utfylt i V3, NA i V2

  #Ny ARBEIDSSTATUS-variabel, basert på V2 og V3:
  #Arbstatus3mndV3 - for verdi 11, 99 eller NA hvor sykemeldingsgrad angitt: verdien 7.
  RegDataV3$Arbstatus3mndV3[which(RegDataV3$Arbstatus3mndV3 %in% c(11,99))] <- NA
  ind7_3mnd <- which(is.na(RegDataV3$Arbstatus3mndV3) & RegDataV3$SykemeldPros3mnd > 0) #17 per 1.mars 2024
  RegDataV3$Arbstatus3mndV3[ind7_3mnd] <- 7

  RegDataV3$Arbstatus12mndV3[which(RegDataV3$Arbstatus12mndV3 %in% c(10,99))] <- NA
  ind7_12mnd <- which( is.na(RegDataV3$Arbstatus12mndV3) & RegDataV3$SykemeldPros12mnd>0) #6 per 1.mars 2024
  RegDataV3$Arbstatus12mndV3[ind7_12mnd] <- 7

  # 1: I arbeid - V3- 1+2
  RegDataV3$ArbstatusPreV2V3 <- dplyr::replace_values(RegDataV3$ArbstatusPreV3, from = c(2, 99), to = c(1, NA))
  RegDataV3$Arbstatus3mndV2V3 <- dplyr::replace_values(RegDataV3$Arbstatus3mndV3, from = 2, to = 1)
  RegDataV3$Arbstatus12mndV2V3 <- dplyr::replace_values(RegDataV3$Arbstatus12mndV3, from = 2, to = 1)

  RegDataV3 <- dplyr::mutate(RegDataV3,
                      ArbstatusPreV3 = NULL, Arbstatus3mndV3=NULL, Arbstatus12mndV3=NULL)

  #Legge til underkategori for hovedkategori.
  RegDataV3 <- kategoriserInngrep(RegData=RegDataV3)$RegData

  #Definasjon av diagnosegrupper prolaps og spinal stenose
  RegDataV3 <- defProSS(RegDataV3)

  RegDataV3$Kp3mnd <- NULL
  RegDataV3$Kp3mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3mnd','KpInfDyp3mnd', 'KpUVI3mnd',
                                         'KpLungebet3mnd', 'KpBlod3mnd','KpDVT3mnd','KpLE3mnd')],
                           na.rm = T) > 0] <- 1
  RegDataV3$KpInf3mnd <- NULL
  RegDataV3$KpInf3mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3mnd','KpInfDyp3mnd')], na.rm = T) > 0] <- 1


  #TidlOp. V2: 1:4,9 c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
  #TidlIkkeOp, TidlOpAnnetNiv, TidlOpsammeNiv
  RegDataV3$TidlOpr <- ifelse(RegDataV3$TidlIkkeOp==1, 4, 9)
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1] <- 1
  RegDataV3$TidlOpr[RegDataV3$TidlOpAnnetNiv==1] <- 2
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1 & RegDataV3$TidlOpAnnetNiv==1] <- 3

  RegDataV3$OpMikro <- dplyr::replace_values(RegDataV3$OpMikroV3,
                                       from = c(0,1,2,3,9), to = c(0,1,1,1,0))
  RegDataV3$OpAndreEndosk <- dplyr::replace_values(RegDataV3$OpMikroV3,
                                             from = c(0,1,2,3,9), to = c(0,0,0,1,0)) #OpMikroV3= 3 OR
  RegDataV3$OpAndreEndosk[with(RegDataV3, which(EndoSkopTilg == 1 | EndoSkopTilg == 2 |
                                                   EndoSkopTekn == 1 | EndoSkopTekn == 2))] <- 1

  RegDataV3$ForstLukketLege <- as.character(as.Date(RegDataV3$ForstLukketLege))
  #Kobling med NA fungerer ikke for datotid-var

  RegDataV3$Versjon <- 'V3'

return(RegDataV3)
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
  RegData$ErMann <- RegData$Kjonn
  RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$OpDato <- as.Date(RegData$OpDato, format="%Y-%m-%d") #, tz='UTC')
  RegData$InnlagtDato <- as.Date(RegData$InnlagtDato)
  RegData$BlodfortynnendeSepDato <- as.Date(RegData$BlodfortynnendeSepDato)
  RegData$UtskrivelseDato <- as.Date(RegData$UtskrivelseDato)
  RegData$UtfyltDatoLege <- as.Date(RegData$UtfyltDatoLege, format="%Y-%m-%d")
  RegData$UtfyltDatoPas <- as.Date(RegData$UtfyltDatoPas)
  RegData$UtfyltDato3mnd <- as.Date(RegData$UtfyltDato3mnd)
	RegData$UtfyltDato12mnd <- as.Date(RegData$UtfyltDato12mnd)
	RegData$DatoFodt <- as.Date(RegData$DatoFodt)

	#RegData$dato
	RegData$ProsKode1 <- substr(RegData$ProsKode1, 1, 5)
	RegData$ProsKode2 <- substr(RegData$ProsKode2, 1, 5)

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
	RegData$Aar <- lubridate::year(RegData$OpDato)
	RegData$MndNum <- as.numeric(format(RegData$OpDato, '%m'))
	RegData$MndAar <- format(RegData$OpDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	#?Trenger kanskje ikke de over siden legger på tidsenhet når bruk for det.
	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$ForstLukketLege),
	                                            as.Date(RegData$UtskrivelseDato), units = 'days'))
	RegData$DiffUtfOp <- as.numeric(difftime(as.Date(RegData$UtfyltDatoPas),
	                                         as.Date(RegData$OpDato), units = 'days'))
	RegData$Dod30 <- 0
	RegData$Dod30[which(as.numeric(as.Date(RegData$DodsDato) - as.Date(RegData$OpDato)) < 30)] <- 1
	RegData$Dod365 <- 0
	RegData$Dod365[which(as.numeric(as.Date(RegData$DodsDato) - as.Date(RegData$OpDato)) < 365)] <- 1

	  return(invisible(RegData))
}


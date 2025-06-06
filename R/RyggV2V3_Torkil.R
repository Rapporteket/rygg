#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg og kobler samme versjon 2 og versjon 3.
#' Registeret ønsker også en versjon hvor variabler som bare er i versjon 2 er med i det
#' felles uttrekket.
#'
#' @return RegData, dataramme med data f.o.m. 2007.
#' @export

RyggDataV2V3 <- function(){

    RegDataV2 <- rapbase::loadRegData(registryName="rygg",
                                    query='SELECT * FROM uttrekk_rapport_from_tore')

  RegDataV3AVN <- rapbase::loadRegData(registryName="rygg",
                                     query='SELECT * FROM allevarnum')
  RegDataV3Forl <- rapbase::loadRegData(registryName="rygg",
                                       query='SELECT * FROM forlopsoversikt')
  varForl <- c("ForlopsID", "Kommune", "Kommunenr", "Fylkenr",     #Fylke er med i AVN
                "Avdod", "AvdodDato", "BasisRegStatus", "KryptertFnr")
  RegDataV3 <- merge(RegDataV3AVN[ ,-which(names(RegDataV3AVN)=='DodsDato')],
                     RegDataV3Forl[ ,varForl], by='ForlopsID',
                     all.x = TRUE, all.y = FALSE)

    #I perioden 2019-21 ble ikke dyp og overfladisk sårinfeksjon registrert.
    indIkkeSaarInf <- which(RegDataV3$OpDato >= '2019-01-01' & RegDataV3$OpDato <= '2021-12-31')
    RegDataV3$KpInfOverfla3Mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfOverfla12mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp3Mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp12mnd[indIkkeSaarInf] <- NA


  #-----Tilrettelegging av V2-data-------------------------
if (kunV3 == 0) {
   RegDataV2$PID <- paste0(RegDataV2$PID, 'V2')

   # Arbstatus3mnd OG Arbstatus12mnd V2:
   # De som har verdi 11 settes TIL MANGLENDE
  # 2: Hjemmeværende - bare i V2 - settes tom
   # 7:Delvis sykemeldt - V2: 7+8,
   # 8: Arbeidsavklaring - V2:9
   # 9: Ufør - V2: 10,
# Torkil, vi justerer Abeidsstatus-variabelen både i V2 og V3 for å få en gjennomgående variabel.
   # Arbeidsstatus i V2 har avvikende kategorisering ift tilsvarende variabel i V3 slik at den ikke kan migreres inn i ArbeidsstatusV3.
   RegDataV2$ArbstatusPreV2V3 <- as.numeric(RegDataV2$ArbstatusPre)
   RegDataV2$Arbstatus3mndV2V3 <- as.numeric(RegDataV2$Arbstatus3mnd)
   RegDataV2$Arbstatus12mndV2V3 <- as.numeric(RegDataV2$Arbstatus12mnd)
   RegDataV2$ArbstatusPreV2V3 <- plyr::mapvalues(RegDataV2$ArbstatusPreV2V3, from = c(2,8,9,10), to = c(NA,7,8,9))
   RegDataV2$Arbstatus3mndV2V3 <- plyr::mapvalues(RegDataV2$Arbstatus3mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))
   RegDataV2$Arbstatus12mndV2V3 <- plyr::mapvalues(RegDataV2$Arbstatus12mndV2V3, from = c(2,8,9,10,11), to = c(NA,7,8,9,NA))


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
  RegDataV2$OpIndPareseGrad[is.na(RegDataV2$OpIndPareseGrad)] <- 9
  RegDataV2$Roker[is.na(RegDataV2$Roker)] <- 9
  RegDataV2$Morsmal[is.na(RegDataV2$Morsmal)] <- 9
  RegDataV2$Utd[is.na(RegDataV2$Utd)] <- 9
  RegDataV2$KpInf3Mnd[RegDataV2$KpInf3Mnd==0] <- NA #Tilpasning til V3
  RegDataV2$Versjon <- 'V2'

RegDataV2$AvdReshID <- plyr::revalue(RegDataV2$AvdReshID,  #Gammel resh V2 - ny resh (V3)
                                     c('107137' =	'107508', #Aleris Bergen
                                       '107511' =	'999975', #Aleris Oslo
                                       '999999' =	'110771') #Volvat
)

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
                             #PIDV2 = PID, #Heter PasientID i V3. NB: Må ikke slås sammen.
                             RokerV2 = Roker,
                             #Region = HelseRegion #Navn må evt. mappes om i ettertid. Private bare i V2.
                             SykehusNavn = AvdNavn,
                             Ferdigstilt1b3mnd = Utfylt3Mnd,
                             Ferdigstilt1b12mnd = Utfylt12Mnd,
                             SykDprebetesMellitus = SykdDiabetesMellitus
  )


  #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
  RegDataV2$SivilStatusV3 <- plyr::mapvalues(as.numeric(RegDataV2$SivilStatus), from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

}
  #-----Tilrettelegging av V3-data-------------------------
  RegDataV3$PID <- RegDataV3$PasientID #PID vil kobles med variabel PID fra V2 og tilpasses, ønsker å beholde PasientID fra V3
  #Navneendring av V3:
  RegDataV3 <- dplyr::rename(RegDataV3,
                             OpProlap = OprProlap #Siden Alle andre heter Op..
                             ) #PIDV3 = PasientID)
  #Ønsker tom for manglende
  RegDataV3$SmBePre[RegDataV3$SmBePre == 99] <- NA #99: Ikke utfylt i V3, NA i V2
  RegDataV3$SmRyPre[RegDataV3$SmRyPre == 99] <- NA #99: Ikke utfylt i V3, NA i V2



  #Ny arbedisstatus-variabel, basert på V2 og V3:
  #Arbstatus3mndV3 - de med verdi 11 eller 99 eller tom som har oppgitt sykemeldingsgrad., Skal ha verdien 7.
  RegDataV3$Arbstatus3mndV3[which(RegDataV3$Arbstatus3mndV3 %in% c(11,99))] <- NA
  ind7_3mnd <- which(is.na(RegDataV3$Arbstatus3mndV3) & RegDataV3$SykemeldPros3mnd>0) #17 per 1.mars 2024
  RegDataV3$Arbstatus3mndV3[ind7_3mnd] <- 7

  RegDataV3$Arbstatus12mndV3[which(RegDataV3$Arbstatus12mndV3 %in% c(10,99))] <- NA
  ind7_12mnd <- which( is.na(RegDataV3$Arbstatus12mndV3) & RegDataV3$SykemeldPros12mnd>0) #6 per 1.mars 2024
  RegDataV3$Arbstatus12mndV3[ind7_12mnd] <- 7

  # 1: I arbeid - V3- 1+2
  RegDataV3$ArbstatusPreV2V3 <- RegDataV3$ArbstatusPreV3
  RegDataV3$Arbstatus3mndV2V3 <- RegDataV3$Arbstatus3mndV3
  RegDataV3$Arbstatus12mndV2V3 <- RegDataV3$Arbstatus12mndV3
  RegDataV3$ArbstatusPreV2V3 <- plyr::mapvalues(RegDataV3$ArbstatusPreV2V3, from = c(2, 99), to = c(1, NA))
  RegDataV3$Arbstatus3mndV2V3 <- plyr::mapvalues(RegDataV3$Arbstatus3mndV2V3, from = 2, to = 1)
  RegDataV3$Arbstatus12mndV2V3 <- plyr::mapvalues(RegDataV3$Arbstatus12mndV2V3, from = 2, to = 1)


  #Legge til underkategori for hovedkategori.
  ny <- rygg::kategoriserInngrep(RegData=RegDataV3)
  RegDataV3 <- ny$RegData

  #------------------Følgende kan kanskje fjeres fra din jobb...:

  #--------Definasjon av diagnosegrupper prolaps og spinal stenose V3
  # COMPUTE filter_$=(HovedInngrepV2V3 = 4 or (RfSentr = 1 or RfLateral = 1 or RfForaminalSS = 1)
  #                   & (OpDeUlamin = 1 or OpDeUlaminTilgang > 0 or OpLaminektomi  = 1)
  #                   & (HovedInngrepV2V3 = 2 or HovedInngrepV2V3 = 3
  #                                            or HovedInngrepV2V3 = 5 or HovedInngrepV2V3 = 7) ).
  RegDataV3$LSSopr <- ifelse(RegDataV3$HovedInngrepV2V3 == 4
                           | (RegDataV3$RfSentr == 1 | RegDataV3$RfLateral == 1 | RegDataV3$RfForaminalSS == 1)
                           & (RegDataV3$OpDeUlamin == 1 | RegDataV3$OpDeUlaminTilgang %in% 1:3 | RegDataV3$OpLaminektomi == 1)
                           & (RegDataV3$HovedInngrepV2V3 %in% c(2,3,5,7)),
                           1, 0)

  #*Definisjon av prolapsgruppen, dekompresjon, kvalitetssikre, først gr uten fusjon..
  #COMPUTE filter_$=(HovedInngrepV2V3 = 1 &  LSS_opr = 0).
  # 1  'Ja operert med dekopressjon for prolaps'
  # 0 ' Nei ikke operert med dekopressjon for prolaps'.
  RegDataV3$ProlapsDekr <- ifelse(RegDataV3$HovedInngrepV2V3 == 1 &  RegDataV3$LSSopr == 0, 1, 0)

  #*Definisjon av prolapsgruppen,med fusjon.
  # 1  'Ja operert med fusjon for prolaps'
  # 0 ' Nei ikke operert med fusjon for prolaps'.
  # COMPUTE filter_$=((Prolaps_dekr = 0 & LSS_opr = 0) & (HovedInngrepV2V3 = 5 & OpProlap > 0) &
  #                     (RfDegskol =  0 & RfSpondtypeDegen = 0 & RfSpondtypeIsmisk = 0)).
  RegDataV3$ProlapsFusjonert <-
    ifelse((RegDataV3$ProlapsDekr == 0 & RegDataV3$LSSopr == 0) &
             (RegDataV3$HovedInngrepV2V3 == 5 & RegDataV3$OpProlap > 0) &
             (RegDataV3$RfDegskol ==  0 & RegDataV3$RfSpondtypeDegen == 0 & RegDataV3$RfSpondtypeIsmisk == 0),
           1, 0)

  #*Definisjon av prolapsgruppen, prolapsopr med og uten fusjon.
  #COMPUTE filter_$=(Prolaps_dekr = 1 or Prolaps_fusjonert = 1).
  # VARIABLE LABELS  Prolapsopr_alle 'både dekr og fusjon'.
  # 1  'Ja  alle typer prolapsoperason'
  # 0 ' Nei ikke operert for prolaps'.
  RegDataV3$ProlapsoprAlle <- ifelse(RegDataV3$ProlapsDekr == 1 | RegDataV3$ProlapsFusjonert == 1, 1, 0)


  # DO IF (LSS_opr = 0 & Prolapsopr_alle = 0   & OpDeUlamin = 1).
  # RECODE LSS_opr (0=1).
  utvalg <- which(RegDataV3$LSSopr == 0 & RegDataV3$ProlapsoprAlle == 0   & RegDataV3$OpDeUlamin == 1)
  RegDataV3$LSSopr[utvalg] <- 1

  RegDataV3$Kp3Mnd <- NULL
  RegDataV3$Kp3Mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpUVI3Mnd',
                                         'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd')],
                           na.rm = T) > 0] <- 1
  RegDataV3$KpInf3Mnd <- NULL
  RegDataV3$KpInf3Mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3Mnd','KpInfDyp3Mnd')], na.rm = T) > 0] <- 1


  #TidlOp. V2: 1:4,9 c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
  #TidlIkkeOp, TidlOpAnnetNiv, TidlOpsammeNiv
  RegDataV3$TidlOpr <- 9
  RegDataV3$TidlOpr[RegDataV3$TidlIkkeOp==1] <- 4
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1] <- 1
  RegDataV3$TidlOpr[RegDataV3$TidlOpAnnetNiv==1] <- 2
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1 & RegDataV3$TidlOpAnnetNiv==1] <- 3

  RegDataV3$OpMikro <- plyr::mapvalues(RegDataV3$OpMikroV3, from = c(0,1,2,3,9), to = c(0,1,1,1,0))
  RegDataV3$OpAndreEndosk <- plyr::mapvalues(RegDataV3$OpMikroV3, from = c(0,1,2,3,9), to = c(0,0,0,1,0))

if (kunV3 == 0){
  #Variabler i V2 som ikke er i V3.
  RegDataV3$RokerV2 <- plyr::mapvalues(RegDataV3$RokerV3, from = 2, to = 0)
  VarV2 <- names(RegDataV2) #sort
  VarV3 <- names(RegDataV3) #sort

  V2ogV3 <- intersect(VarV2, VarV3)
  V3ikkeV2 <- setdiff(VarV3, V2ogV3)
  V2ikkeV3 <- setdiff(VarV2, V2ogV3)

      #RegDataV3$AvdodDato <- as.Date(RegDataV3$AvdodDato)
    RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
    RegDataV3[, V2ikkeV3] <- NA
    RegDataV2V3 <- rbind(RegDataV2,
                         RegDataV3)

}

  #Mars 2021: KpInf-variabler, 3mnd er navngitt ..3Mnd i begge versjoner. Endrer navngiving
  EndreNavnInd <- grep('3Mnd', names(RegDataV2V3))
  names(RegDataV2V3)[EndreNavnInd] <- gsub("3Mnd", "3mnd", names(RegDataV2V3)[EndreNavnInd])

  #En desimal
  RegDataV2V3$BMI <- round(RegDataV2V3$BMI,1)
  RegDataV2V3$OswTotPre <- round(RegDataV2V3$OswTotPre,1)
  RegDataV2V3$OswTot3mnd <- round(RegDataV2V3$OswTot3mnd,1)
  RegDataV2V3$OswTot12mnd <- round(RegDataV2V3$OswTot12mnd,1)

  return(RegDataV2V3)
}



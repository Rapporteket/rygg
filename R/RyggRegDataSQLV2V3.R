#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg og kobler samme versjon 2 og versjon 3.
#' Registeret ønsker også en versjon hvor variabler som bare er i versjon 2 er med i det
#' felles uttrekket. (?Lager en egen versjon for dette.)
#'
#' @param alleVarV3 0: IKKE I BRUK fjerner variabler som ikke er i bruk på Rapporteket ,
#'                  1: har med alle variabler fra V3 (foreløpig er dette standard)
#' @param alleVarV2 0: Bare variabler som også finnes i V3 med (standard),
#'                  1: har med alle variabler fra V2
#' @param datoFra P.t ikke i bruk
#' @param datoTil P.t ikke i bruk
#'
#' @return RegData, dataramme med data f.o.m. 2007.
#' @export

RyggRegDataSQLV2V3 <- function(datoFra = '2007-01-01', datoTil = '2099-01-01',
                               alleVarV3=1, alleVarV2=0){
#NB: datovalg har ingen effekt foreløpig!! - bør hente alle for å få f.eks. operasjonsnummer riktig...
#?Legg inn sjekk på at ikke trenger å koble hvis: if (datoFra < '2019-01-01'){

  kunV3 <- ifelse(datoFra >= '2020-01-01' & !is.na(datoFra), 1, 0)

  if (kunV3 == 0) {
    RegDataV2 <- rapbase::loadRegData(registryName="rygg",
                                    query='SELECT * FROM Uttrekk_Rapport_FROM_TORE')}
  RegDataV3AVN <- rapbase::loadRegData(registryName="rygg",
                                     query='SELECT * FROM AlleVarNum')
  #test <- RegDataV3[ ,c("DodsDato", 'AvdodDato', 'Avdod')]
  RegDataV3Forl <- rapbase::loadRegData(registryName="rygg",
                                       query='SELECT * FROM ForlopsOversikt')
  varForl <- c("ForlopsID", "Kommune", "Kommunenr", "Fylkenr",     #Fylke er med i AVN
                "Avdod", "AvdodDato", "BasisRegStatus", "KryptertFnr")
   #varBegge <- intersect(sort(names(RegDataV3AV)), sort(names(RegDataV3For)))
  RegDataV3 <- merge(RegDataV3AVN, RegDataV3Forl[ ,varForl], by='ForlopsID', all.x = TRUE, all.y = FALSE)

  ePROMadmTab <- rapbase::loadRegData(registryName="rygg",
                                   query='SELECT * FROM proms')
  ePROMvar <- c("MCEID", "TSSENDT", "TSRECEIVED", "NOTIFICATION_CHANNEL", "DISTRIBUTION_RULE",
                'REGISTRATION_TYPE')
  # «EpromStatus» er definert av oss, og den som er viktigst med tanke på svarprosent. Det er altså verdi 3 her som betyr at pasienten har besvart. OBS at den skiller seg litt fra tilsvarende variabel i Hemit-definisjonen. Denne er for deg og oss definert slik:
  # 0 = Created, 1 = Ordered, 2 = Expired, 3 = Completed, 4 = Failed
  ind3mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                         c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'))

  ind12mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                      c('PATIENTFOLLOWUP12', 'PATIENTFOLLOWUP_12_PiPP', 'PATIENTFOLLOWUP_12_PiPP_REMINDER'))

  ePROM3mnd <- ePROMadmTab[intersect(ind3mnd, which(ePROMadmTab$STATUS==3)), ePROMvar] #STATUS==3 completed
  names(ePROM3mnd) <- paste0(ePROMvar, '3mnd')
  ePROM12mnd <- ePROMadmTab[intersect(ind12mnd, which(ePROMadmTab$STATUS==3)), ePROMvar]
  names(ePROM12mnd) <- paste0(ePROMvar, '12mnd')

    indIkkeEprom3mnd <-  which(!(RegDataV3$ForlopsID %in% ePROMadmTab$MCEID[ind3mnd]))
    indIkkeEprom12mnd <-  which(!(RegDataV3$ForlopsID %in% ePROMadmTab$MCEID[ind12mnd]))
    #indEprom <-  which((RegDataV3$ForlopsID %in% ePROMadmTab$MCEID[ind3mnd]))
    RegDataV3$Ferdig1b3mndGML <- RegDataV3$Ferdigstilt1b3mnd
    RegDataV3$Ferdigstilt1b3mnd <- 0
    RegDataV3$Ferdigstilt1b3mnd[RegDataV3$ForlopsID %in% ePROM3mnd$MCEID] <- 1
    RegDataV3$Ferdigstilt1b3mnd[intersect(which(RegDataV3$Ferdig1b3mndGML ==1), indIkkeEprom3mnd)] <- 1

    RegDataV3$Ferdig1b12mndGML <- RegDataV3$Ferdigstilt1b12mnd
    RegDataV3$Ferdigstilt1b12mnd <- 0
    RegDataV3$Ferdigstilt1b12mnd[RegDataV3$ForlopsID %in% ePROM12mnd$MCEID] <- 1
    RegDataV3$Ferdigstilt1b12mnd[intersect(which(RegDataV3$Ferdig1b12mndGML ==1), indIkkeEprom12mnd)] <- 1

    # table(RegDataV3$Ferdig1b3mndGML)
    # table(RegDataV3$Ferdigstilt1b12mnd)
    #
    # table(RegDataV3[ ,c('Aar', 'Ferdigstilt1b12mnd')])
    # table(RegDataV3$Aar, !is.na(RegDataV3$OswTot12mnd))

   # RegDataV3$Aar <- lubridate::year(RegDataV3$OpDato)
   # test <- RegDataV3[which(RegDataV3$Aar==2021 & RegDataV3$Ferdigstilt1b3mnd==1), c("OpDato", "Utfdato3mnd", "ForstLukket3mnd")]
   # test$forsinkelse3mnd <- as.Date(test$Utfdato3mnd) - as.Date(test$OpDato)
   # mean(test$forsinkelse3mnd)

    #I perioden 2019-21 ble ikke dyp og overfladisk sårinfeksjon registrert.
    indIkkeSaarInf <- which(RegDataV3$OpDato >= '2019-01-01' & RegDataV3$OpDato <= '2021-12-31')
    RegDataV3$KpInfOverfla3Mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfOverfla12mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp3Mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp12mnd[indIkkeSaarInf] <- NA

  if (alleVarV3 == 0) { #Tar bort noen variabler for å spare tid
    #!DENNE MÅ GÅS GJENNOM. SER UT TIL AT NOEN NØDVENDIGE VARIABLER FJERNES
  fjernesV3 <- c("Adresse", "Adressetype")
      # ,"AntibiotikaAntDogn", "AntibiotikaAntDoser", "AntibiotikaDose", "AntibiotikaEvtAntDogn",
      # "AntibiotikaKunOprDag", "BenAutogrType", "BenAutoHofte", "BenAutoLokalt", "BenBank", "BenSubstitutt",
      # "BlodfortynnendeFast", "BlodfortynnendePreop", "BlodfortynnendeSepDato", "BlodfortynnendeSpes",
      # "CaudaAntDogn", "CaudaAntTimer", "CaudaAntUker", "CaudaEnUkeTilTreMnd", "CaudaOverTreMnd",
      # "CaudaUnderEnUke", "CaudaUnderEttDogn", "DekompAntNivaa", "DekomprAnnetNivaa",
      # "DekomrSpesAnnetNivaaDekomrSpesAnnetNivaa", "EtnKultTilhorighet", "FodtiNorge", "ForrigeInngrep",
      # "Fritekstadresse", "FusjonAntNivaa", "FusjonIleumSkrue", "FusjonKir", "FusjonKirAlif",
      # "FusjonKirPlfIkkeInstrV3", "FusjonKirPlfInstrV3", "FusjonKirPlfV3", "FusjonKirPlif", "FusjonKirTlif",
      # "FusjonKirXlif", "FusjonNedreNivaa", "FusjonOvreNivaa", "FusjonSement",
      # "FysioAnnenBeh", "FysioPsykoMotorisk", "FysioTrening", "HKirurgErfaring", "HKirurgErfaringAar",
      # "HovedSpinalKirurg", "Hoyde", "HoydeMangler", "KliniskFleksjonLindring",
      # "KliniskPosLasegue", "KnivSluttKlokkeMin", "KnivSluttKlokkeTime", "KnivStartKlokkeMin",
      # "KnivStartKlokkeTime", "KnivTidMinVarighet", "KnivTidTimerVarighet",
      # "NyAnnen12mnd", "NyAnnen3mnd", "NyHjerteKar12mnd", "NyHjerteKar3mnd", "Nykreft12mnd", "Nykreft3mnd",
      # "NyLeddSm12mnd", "NyLeddSm3mnd", "NyNerveSkyd12mnd", "NyNerveSkyd3mnd", "NyOprAnt12mnd", "NyOprAnt3mnd",
      # "NyOprNivaa12mnd", "NyOprNivaa3mnd", "NyRyggOpr12mnd", "NyRyggOpr3mnd", "NySkade12mnd", "NySkade3mnd",
      # "NySykdSkade12mnd", "NySykdSkade3mnd",
      # "OpAndreSkiveprotese", "OpAnnenOsteotomi", "OpAnnenOstetosyntSpes", "OpComputerNav",
      # "OpFusjonPerkutan", "OpFusjonUtenDekomprV3",
      # "OpKileOsteotomi", "OpKyfoseLL", "OpKyfoseLLGrader", "OpKyfosePI", "OpKyfosePIGrader",
      # "OpKyfosePT", "OpKyfosePTGrader", "OpKyfoseSVA", "OpKyfoseSVAcm", "OpKysfoseSS", "OpKysfoseSSGrader",
      # "OpL1L2", "OpL23", "OpL34", "OpL45", "OpL5S1", "OpOsteosyntFjerningV3",
      # "OpOsteosyntRevV3", "OpPonteSPOsteotomi", "OpProOsteotomi", "OpSkolioseCobb", "OpSkolioseCobbGrader",
      # "OpSkolioseKyfose", "OpTh12L1", "OpTilgangV3",
      # "PostNr", "PostopTrombProfyl", "PostSted",
      # "RfDegenListeseMM", "RfEkstrLatProl", "RfIntrforaminaltProl", "RfIstmiskLyse",
      # "RFKunDegenerasjon", "RfKyfose", "RfMeyerdingGrad", "RfModic", "RfModicTypeI",
      # "RfModicTypeII", "RfSkive", "RfSynovpre",
      # "RfTypeIAktNivaa", "RfTypeIAnnetNivaa", "RfTypeIIAktNivaa", "RfTypeIIAnnetNivaa",
      # "RvBlokadeFacett", "RvBlokadeNerverot", "RvFunksjoTranslMM", "RvFunksjoVinkelEndrGr",
      # "SpesTrombProfyl", "Utfdato12mnd", "Utfdato3mnd", "UtfyltDato")

    RegDataV3 <- RegDataV3[ ,-which(names(RegDataV3) %in% fjernesV3)]
  }

  #-----Tilrettelegging av V2-data-------------------------
    #"Arbstatus12mnd", "Arbstatus3mnd", "ArbstatusPre" - vanskelig å tilpasse til ny versjon..
if (kunV3 == 0) {
   RegDataV2$PID <- paste0(RegDataV2$PID, 'V2')

  #SykemeldVarighPre V2-numerisk, V3 - 1: <3mnd, 2:3-6mnd, 3:6-12mnd, 4:>12mnd, 9:Ikke utfylt
  RegDataV2$SykemeldVarighPreV3 <- as.numeric(cut(as.numeric(RegDataV2$SykemeldVarighPre),
                                                  breaks=c(-Inf, 90, 182, 365, Inf),
                                                  right = FALSE, labels=c(1:4)))
  RegDataV2$SykemeldVarighPreV3[is.na(RegDataV2$SykemeldVarighPreV3)] <- 9

  RegDataV2$AntibiotikaV3 <-  plyr::mapvalues(RegDataV2$Antibiotika, from = c(0, 1, NA), to = c(0,1,9))

  #V2: Kode 1:4,NA: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
  #V3: [0,1,2,3,9]	["Nei","Ja","Planlegger","Innvilget","Ikke utfylt"]
  RegDataV2$ErstatningPre <- plyr::mapvalues(RegDataV2$ErstatningPre, from = c(2,3,4,NA), to = c(0,2,3,9))
  RegDataV2$UforetrygdPre <- plyr::mapvalues(RegDataV2$UforetrygdPre, from = c(2,3,4,NA), to = c(0,2,3,9))

  RegDataV2$SmBePre[is.na(RegDataV2$SmBePre)] <- 99 #99: Ikke utfylt i V3, NA i V2
  RegDataV2$SmRyPre[is.na(RegDataV2$SmRyPre)] <- 99 #99: Ikke utfylt i V3, NA i V2
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

  RegDataV2$AvdReshID <- plyr::revalue(RegDataV2$AvdReshID,  #Gammelt navn V2 - nytt navn (V3), dvs. gmlresh	nyresh
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
  RegDataV2$SivilStatusV3 <- plyr::mapvalues(RegDataV2$SivilStatus, from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

}
  #-----Tilrettelegging av V3-data-------------------------
#Fjerner ikke-ferdigstilte pasientskjema
  RegDataV3 <- RegDataV3[RegDataV3$Ferdig1a==1 & RegDataV3$Ferdig2a==1, ]
  RegDataV3$Versjon <- 'V3'

  RegDataV3$PID <- RegDataV3$PasientID #PID vil kobles med variabel PID fra V2 og tilpasses, ønsker å beholde PasientID fra V3
  #Navneendring av V3:
  RegDataV3 <- dplyr::rename(RegDataV3,
                             OpProlap = OprProlap #Siden Alle andre heter Op..
                             ) #PIDV3 = PasientID)

  #Legge til underkategori for hovedkategori.
  ny <- rygg::kategoriserInngrep(RegData=RegDataV3)
  RegDataV3 <- ny$RegData

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

  RegDataV3$MedForstLukket <- as.character(as.Date(RegDataV3$MedForstLukket)) #Kobling med NA fungerer ikke for datotid-var

  #NB:----------Sjekk ut at alle variabler har samme format - VENTER TIL ENDELIG V2-FIL PÅ RAPPORTEKET.
  #f.eks.
  #head(RegDataV2[, V2ogV3])
  #head(RegDataV3[, V2ogV3])

if (kunV3 == 0){
  #Variabler i V2 som ikke er i V3. Noen er bevisst fjernet fra V3, se vektor fjernesV3
  #setdiff(VarV2, VarV3) #Sjekk på nytt når gått gjennom.
  RegDataV3$RokerV2 <- plyr::mapvalues(RegDataV3$RokerV3, from = 2, to = 0)
  VarV2 <- names(RegDataV2) #sort
  VarV3 <- names(RegDataV3) #sort

  V2ogV3 <- intersect(VarV2, VarV3)
  V3ikkeV2 <- setdiff(VarV3, V2ogV3)
  V2ikkeV3 <- setdiff(VarV2, V2ogV3)
  if (alleVarV2 == 0){
    RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
    #RegDataV2 <- RegDataV2[ , V2ogV3]
    RegDataV2V3 <- rbind(RegDataV2[ ,VarV3],
                         RegDataV3[ ,VarV3])
  } else {
    #RegDataV3$AvdodDato <- as.Date(RegDataV3$AvdodDato)
    RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
    RegDataV3[, V2ikkeV3] <- NA
    RegDataV2V3 <- rbind(RegDataV2,
                         RegDataV3)
  }
}
  if (kunV3 == 1) {RegDataV2V3 <- RegDataV3}
  #Avvik? PeropKompAnnet
  #ProsKode1 ProsKode2 - Kode i V2, kode + navn i V3

  # plyr::revalue(x, c('HELSEREGION MIDT-NORGE' = 'Midt', 'HELSEREGION NORD' = 'Nord',
  #                  'HELSEREGION SØR-ØST' = 'Sør-Øst', 'HELSEREGION VEST' = 'Vest'),
  #             'Helse Midt' = 'Midt', 'Helse Nord' = 'Nord', 'Helse Sør' = 'Sør-Øst',
  #             'Helse Sør-Øst' = 'Sør-Øst', 'Helse Vest' = 'Vest', 'Helse Øst' = 'Sør-Øst')

  #Mars 2021: KpInf-variabler, 3mnd er navngitt ..3Mnd i begge versjoner. Endrer navngiving
  EndreNavnInd <- grep('3Mnd', names(RegDataV2V3)) #names(RyggData)[grep('3Mnd', names(RyggData))]
  names(RegDataV2V3)[EndreNavnInd] <- gsub("3Mnd", "3mnd", names(RegDataV2V3)[EndreNavnInd])

  RegDataV2V3$AvdodDato <- as.Date(RegDataV2V3$AvdodDato, origin='1970-01-01')
  #En desimal
  RegDataV2V3$BMI <- round(RegDataV2V3$BMI,1)
  RegDataV2V3$OswTotPre <- round(RegDataV2V3$OswTotPre,1)
  RegDataV2V3$OswTot3mnd <- round(RegDataV2V3$OswTot3mnd,1)
  RegDataV2V3$OswTot12mnd <- round(RegDataV2V3$OswTot12mnd,1)

  return(RegDataV2V3)
}



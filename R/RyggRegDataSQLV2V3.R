#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg fra "staging" (?)
#'
#'
#' @inheritParams RyggUtvalgEnh
#' @param alleVarV3 0: fjerner variabler som ikke er i bruk på Rapporteket (standard),
#'                  1: har med alle variabler fra V3
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQLV2V3 <- function(datoFra = '2007-01-01', datoTil = '2099-01-01', alleVarV3=1){

#Legg inn sjekk på at ikke trenger å koble hvis: if (datoFra < '2019-01-01'){

  RegDataV2 <- rapbase::LoadRegData(registryName="rygg",
                                    query='SELECT * FROM Uttrekk_Rapport_FROM_TORE')
  RegDataV3 <- rapbase::LoadRegData(registryName="rygg",
                                    query='SELECT * FROM AlleVarNum')

  if (alleVarV3 == 0) {
    #!DENNE MÅ GÅS GJENNOM. SER UT TIL AT NOEN NØDVENDIGE VARIABLER FJERNES
  fjernesV3 <-
    c("Adresse", "Adressetype",
      "AntibiotikaAntDogn", "AntibiotikaAntDoser", "AntibiotikaDose", "AntibiotikaEvtAntDogn",
      "AntibiotikaKunOprDag", "BenAutogrType", "BenAutoHofte", "BenAutoLokalt", "BenBank", "BenSubstitutt",
      "BlodfortynnendeFast", "BlodfortynnendePreop", "BlodfortynnendeSepDato", "BlodfortynnendeSpes",
      "CaudaAntDogn", "CaudaAntTimer", "CaudaAntUker", "CaudaEnUkeTilTreMnd", "CaudaOverTreMnd",
      "CaudaUnderEnUke", "CaudaUnderEttDogn", "DekompAntNivaa", "DekomprAnnetNivaa",
      "DekomrSpesAnnetNivaaDekomrSpesAnnetNivaa", "EtnKultTilhorighet", "FodtiNorge", "ForrigeInngrep",
      "Fritekstadresse", "FusjonAntNivaa", "FusjonIleumSkrue", "FusjonKir", "FusjonKirAlif",
      "FusjonKirPlfIkkeInstrV3", "FusjonKirPlfInstrV3", "FusjonKirPlfV3", "FusjonKirPlif", "FusjonKirTlif",
      "FusjonKirXlif", "FusjonNedreNivaa", "FusjonOvreNivaa", "FusjonSement",
      "FysioAnnenBeh", "FysioPsykoMotorisk", "FysioTrening", "HKirurgErfaring", "HKirurgErfaringAar",
      "HovedSpinalKirurg", "Hoyde", "HoydeMangler", "KliniskFleksjonLindring",
      "KliniskPosLasegue", "KnivSluttKlokkeMin", "KnivSluttKlokkeTime", "KnivStartKlokkeMin",
      "KnivStartKlokkeTime", "KnivTidMinVarighet", "KnivTidTimerVarighet",
      "NyAnnen12mnd", "NyAnnen3mnd", "NyHjerteKar12mnd", "NyHjerteKar3mnd", "Nykreft12mnd", "Nykreft3mnd",
      "NyLeddSm12mnd", "NyLeddSm3mnd", "NyNerveSkyd12mnd", "NyNerveSkyd3mnd", "NyOprAnt12mnd", "NyOprAnt3mnd",
      "NyOprNivaa12mnd", "NyOprNivaa3mnd", "NyRyggOpr12mnd", "NyRyggOpr3mnd", "NySkade12mnd", "NySkade3mnd",
      "NySykdSkade12mnd", "NySykdSkade3mnd",
      "OpAndreSkiveprotese", "OpAnnenOsteotomi", "OpAnnenOstetosyntSpes", "OpComputerNav",
      "OpFusjonPerkutan", "OpFusjonUtenDekomprV3",
      "OpKileOsteotomi", "OpKyfoseLL", "OpKyfoseLLGrader", "OpKyfosePI", "OpKyfosePIGrader",
      "OpKyfosePT", "OpKyfosePTGrader", "OpKyfoseSVA", "OpKyfoseSVAcm", "OpKysfoseSS", "OpKysfoseSSGrader",
      "OpL1L2", "OpL23", "OpL34", "OpL45", "OpL5S1", "OpOsteosyntFjerningV3",
      "OpOsteosyntRevV3", "OpPonteSPOsteotomi", "OpProOsteotomi", "OpSkolioseCobb", "OpSkolioseCobbGrader",
      "OpSkolioseKyfose", "OpTh12L1", "OpTilgangV3",
      "PostNr", "PostopTrombProfyl", "PostSted",
      "RfDegenListeseMM", "RfEkstrLatProl", "RfIntrforaminaltProl", "RfIstmiskLyse",
      "RFKunDegenerasjon", "RfKyfose", "RfMeyerdingGrad", "RfModic", "RfModicTypeI",
      "RfModicTypeII", "RfSkive", "RfSynovpre",
      "RfTypeIAktNivaa", "RfTypeIAnnetNivaa", "RfTypeIIAktNivaa", "RfTypeIIAnnetNivaa",
      "RvBlokadeFacett", "RvBlokadeNerverot", "RvDpregblok", "RvFunksjoTranslMM", "RvFunksjoVinkelEndrGr",
      "SpesTrombProfyl", "Utfdato12mnd", "Utfdato3mnd", "UtfyltDato")

    RegDataV3 <- RegDataV3[ ,-which(names(RegDataV3) %in% fjernesV3)]
  }

  #RegData <- RyggPreprosess(RegData) #Sjekket at alle nødvendige var for preprosessering er med

  # table(RegDataV2$ErstatningPre, useNA = 'a') #3mnd, 12mnd
  # 1     2     3     4  <NA>
  # 1005 38830   881   767  2495
  # table(RegDataV3$ErstatningPre, useNA = 'a')
  # 0    1    2    3    9 <NA>
  # 5703  200  139   90 1736    6

  #-----Tilrettelegging av V2-data-------------------------
    #"Arbstatus12mnd", "Arbstatus3mnd", "ArbstatusPre" - vanskelig å tilpasse til ny versjon..

  #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
  RegDataV2$SivilStatusV3 <- plyr::mapvalues(RegDataV2$SivilStatus, from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

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
                             PasientID = PID, #En pasient vil skifte id fra 2019.
                             #Region = HelseRegion #Navn må evt. mappes om i ettertid. Private bare i V2.
                             SykehusNavn = AvdNavn
  )



  #-----Tilrettelegging av V3-data-------------------------

  #Navneendring av V3:
  RegDataV3 <- dplyr::rename(RegDataV3,
                             OpProlap = OprProlap) #Siden Alle andre heter Op..

  #Legge til underkategori for hovedkategori.
  ny <- kategoriserInngrep(RegData=RegDataV3)
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

  VarV2 <- sort(names(RegDataV2))
  VarV3 <- sort(names(RegDataV3))

  #Variabler i V2 som ikke er i V3. Noen er bevisst fjernet fra V3, se vektor fjernesV3
  #setdiff(VarV2, VarV3) #Sjekk på nytt når gått gjennom.

  V2ogV3 <- intersect(VarV2, VarV3)
  RegDataV2 <- RegDataV2[ , V2ogV3]
  V3ikkeV2 <- setdiff(VarV3, V2ogV3)
  #V2ikkeV3 <- setdiff(VarV2, V2ogV3)
  RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
  #BlodfortynnendeSepDato-ok, InnlagtDato-ok, UtfyltDato-ok, UtskrivelseDato-ok, MedForstLukket
  #OprDato?? ok

  RegDataV2V3 <- rbind(RegDataV2[ ,VarV3],
                       RegDataV3[ ,VarV3])
  #19.aug: 101 variabler i både V2 og V3. 128 i tillegg i V3
  #9.sept: 130 var i begge. 98 i tillegg i V3

  #Avvik? PeropKompAnnet
  #ProsKode1 ProsKode2 - Kode i V2, kode + navn i V3

  # plyr::revalue(x, c('HELSEREGION MIDT-NORGE' = 'Midt', 'HELSEREGION NORD' = 'Nord',
  #                  'HELSEREGION SØR-ØST' = 'Sør-Øst', 'HELSEREGION VEST' = 'Vest'),
  #             'Helse Midt' = 'Midt', 'Helse Nord' = 'Nord', 'Helse Sør' = 'Sør-Øst',
  #             'Helse Sør-Øst' = 'Sør-Øst', 'Helse Vest' = 'Vest', 'Helse Øst' = 'Sør-Øst')


  return(RegDataV2V3)
}



#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg og kobler samme versjon 2 og versjon 3
#'
#' @param alleVarV3 0: fjerner variabler som ikke er i bruk på Rapporteket (standard),
#'                  1: har med alle variabler fra V3
#' @inheritParams RyggUtvalgEnh
#'
#' @return RegData, dataramme med data f.o.m. 2007.
#' @export

RyggRegDataSQLV2V3 <- function(datoFra = '2007-01-01', datoTil = '2099-01-01', alleVarV3=1){

#Legg inn sjekk på at ikke trenger å koble hvis: if (datoFra < '2019-01-01'){

  RegDataV2 <- rapbase::loadRegData(registryName="rygg",
                                    query='SELECT * FROM Uttrekk_Rapport_FROM_TORE')
  RegDataV3 <- rapbase::loadRegData(registryName="rygg",
                                    query='SELECT * FROM AlleVarNum')

  # RegDataV2$Aar <- lubridate::year(RegDataV2$OpDato)
  # tapply(RegDataV2$OswTot3mnd, RegDataV2$Aar, FUN = 'median', na.rm=T)
  # RegDataV3$Aar <- lubridate::year(RegDataV3$OpDato)
  # tapply(RegDataV3$OswTot3mnd, RegDataV3$Aar, FUN = 'median', na.rm=T)

  #table(RegDataV3$Fornoyd3mnd, useNA = 'a')

  # tab <- table(RegData[,c("ShNavn", 'Aar')])
  # oppsum <- cbind('2007-18' = rowSums(tab[,c(as.character(2007:2018))]),
  #       '2019-20'= rowSums(tab[,c(as.character(2019:2020))]))
   #table(RegDataV2$AvdNavn)
   # sort(unique(RegDataV2$AvdNavn))
   # tab <- unique(RegDataV2[ ,c("AvdNavn", "AvdReshID")])
   # print(tab[order(tab$AvdNavn),], row.names = F)
  # table(RegDataV3$SykehusNavn)
  # sort(unique(RegDataV3$SykehusNavn))
  # reshV2 <- unique(RegDataV2[ ,c("AvdNavn", "AvdReshID")])[order(unique(RegDataV2$AvdNavn)),]
  # reshV3 <- unique(RegDataV3[ ,c("SykehusNavn", "AvdRESH")])[order(unique(RegDataV3$SykehusNavn)),]
  # setdiff(reshV3$SykehusNavn, reshV2$AvdNavn)
  # setdiff(reshV2$AvdNavn, reshV3$SykehusNavn)
  #Funker ikke ReshTab <- merge(reshV3, reshV2, by.x = 'sykehusNavn', by.y = 'AvdNavn', all=TRUE)
  #unique(RegData[ ,c("SykehusNavn", "AvdRESH")])

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
      "RvBlokadeFacett", "RvBlokadeNerverot", "RvFunksjoTranslMM", "RvFunksjoVinkelEndrGr",
      "SpesTrombProfyl", "Utfdato12mnd", "Utfdato3mnd", "UtfyltDato")

    RegDataV3 <- RegDataV3[ ,-which(names(RegDataV3) %in% fjernesV3)]
  }

  #-----Tilrettelegging av V2-data-------------------------
    #"Arbstatus12mnd", "Arbstatus3mnd", "ArbstatusPre" - vanskelig å tilpasse til ny versjon..


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

  # >   setdiff(reshV3$SykehusNavn, reshV2$AvdNavn)
  # [1] "Ibsensykehuset" "Skien"          "Tønsberg"
  # >   setdiff(reshV2$AvdNavn, reshV3$SykehusNavn)
  # [1] "Aleris Drammen"      "Aleris Bergen"       "Larvik"              "Rikshospitalet, ort" "Flekkefjord"         "Førde"
  # [7] "Molde"               "Bodø"                   "Molde"                  "Oslofjordklinikken Øst" "Bodø"

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
                             RokerV2 = Roker,
                             #Region = HelseRegion #Navn må evt. mappes om i ettertid. Private bare i V2.
                             SykehusNavn = AvdNavn,
                             Ferdigstilt1b3mnd = Utfylt3Mnd,
                             Ferdigstilt1b12mnd = Utfylt12Mnd
  )




  #-----Tilrettelegging av V3-data-------------------------

  #Navneendring av V3:
  RegDataV3 <- dplyr::rename(RegDataV3,
                             OpProlap = OprProlap) #Siden Alle andre heter Op..
  #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
  RegDataV2$SivilStatusV3 <- plyr::mapvalues(RegDataV2$SivilStatus, from = c(1,2,3,NA), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

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
RegDataV3$RokerV2 <- plyr::mapvalues(RegDataV3$RokerV3, from = c(2, NA), to = c(0,9))

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



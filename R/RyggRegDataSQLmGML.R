#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg fra "staging" (?)
#'
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQLV2V3 <- function(datoFra = '2007-01-01', datoTil = '2099-01-01', alle=1){

if (alle == 1) {
  RegData <- rapbase::LoadRegData(
    registryName="rygg",
    query=paste0('SELECT * FROM AlleVarNum WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\''),
    dbType="mysql")
  # RegDataV2 <- rapbase::LoadRegData(registryName="rygg",
  #                                              query='SELECT * FROM Uttrekk_Rapport', dbType="mysql")
} else {


  #RegData <- rapbase::LoadRegData(registryName="rygg", query=query)

  RegDataV2 <- rapbase::LoadRegData(registryName="rygg",
                                    query='SELECT * FROM Uttrekk_Rapport')
  RegDataV3 <- rapbase::LoadRegData(registryName="rygg",
                                    query='SELECT * FROM AlleVarNum')


  fjernesV3 <- c("Adresse", "Adressetype",
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
  "OpL1L2", "OpL23", "OpL34", "OpL45", "OpL5S1", "OpMikroV3", "OpOsteosyntFjerningV3",
  "OpOsteosyntRevV3", "OpPonteSPOsteotomi", "OpProOsteotomi", "OpSkolioseCobb", "OpSkolioseCobbGrader",
  "OpSkolioseKyfose", "OpTh12L1", "OpTilgangV3",
  "PostNr", "PostopTrombProfyl", "PostSted",
  "RfDegenListeseMM", "RfEkstrLatProl", "RfIntrforaminaltProl", "RfIstmiskLyse",
  "RFKunDegenerasjon", "RfKyfose", "RfMeyerdingGrad", "RfModic", "RfModicTypeI",
  "RfModicTypeII", "RfSkive", "RfSynovpre",
  "RfTypeIAktNivaa", "RfTypeIAnnetNivaa", "RfTypeIIAktNivaa", "RfTypeIIAnnetNivaa",
  "RvBlokadeFacett", "RvBlokadeNerverot", "RvDpregblok", "RvFunksjoTranslMM", "RvFunksjoVinkelEndrGr",
  "SpesTrombProfyl", "Utfdato12mnd", "Utfdato3mnd", "UtfyltDato")

  #RegData <- RegDataV3
  RegDataV3 <- RegDataV3[ ,-which(names(RegDataV3) %in% fjernesV3)]
  #RegData <- RyggPreprosess(RegData) #Sjekket at alle nødvendige var for preprosessering er med


#-----Tilrettelegging av V2-data-------------------------

  #"Arbstatus12mnd", "Arbstatus3mnd", "ArbstatusPre" - vanskelig å tilpasse til ny versjon..

    #V2 SivilStatus - 1:Gift, 2:Samboer, 3:Enslig, NA. SivilStatusV3 - 1:Gift/sambo, 2:Enslig, 3:Ikke utfylt
   RegDataV2$SivilStatusV3 <- plyr::mapvalues(RegDataV2$SivilStatus, from = c(1,2,3,''), to = c(1,1,2,9)) #c(2 = 1, 3 = 2, NA=9))

    #SykemeldVarighPre V2-numerisk, V3 - 1: <3mnd, 2:3-6mnd, 3:6-12mnd, 4:>12mnd, 9:Ikke utfylt
    RegDataV2$SykemeldVarighPreV3 <- as.numeric(cut(as.numeric(RegDataV2$SykemeldVarighPre),
                                         breaks=c(-Inf, 90, 182, 365, Inf),
                                         right = FALSE, labels=c(1:4)))
     RegDataV2$SykemeldVarighPreV3[is.na(RegDataV2$SykemeldVarighPreV3)] <- 9


# Variabler med samme innhold i V2 og V3, men avvikende variabelnavn. (navnV3 = navnV2)
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



  #NB:----------Sjekk ut at alle variabler har samme format - VENTER TIL ENDELIG V2-FIL PÅ RAPPORTEKET.
#f.eks. head(RegDataV2, V2ogV3), head(RegDataV3, V2ogV3)

    VarV2 <- sort(names(RegDataV2))
    VarV3 <- sort(names(RegDataV3))

    #Variabler i V2 som ikke er i V3. Noen er bevisst fjernet fra V3, se vektor fjernesV3
    #setdiff(VarV2, VarV3) #Sjekk på nytt når gått gjennom.

  V2ogV3 <- intersect(VarV2, VarV3)
  RegDataV2 <- RegDataV2[ , V2ogV3]
  V3ikkeV2 <- setdiff(VarV3, V2ogV3)
  RegDataV2[, V3ikkeV2] <- NULL

  RegDataV2V3 <- rbind(RegDataV2[ ,VarV3],
                       RegDataV3[ ,VarV3])
  #19.aug: 101 variabler i både V2 og V3. 128 i tillegg i V3


  # plyr::revalue(x, c('HELSEREGION MIDT-NORGE' = 'Midt', 'HELSEREGION NORD' = 'Nord',
  #                  'HELSEREGION SØR-ØST' = 'Sør-Øst', 'HELSEREGION VEST' = 'Vest'),
  #             'Helse Midt' = 'Midt', 'Helse Nord' = 'Nord', 'Helse Sør' = 'Sør-Øst',
  #             'Helse Sør-Øst' = 'Sør-Øst', 'Helse Vest' = 'Vest', 'Helse Øst' = 'Sør-Øst')

} #if langt oppe..
return(RegDataV2V3)
}



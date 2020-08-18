#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg fra "staging" (?)
#'
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQL <- function(datoFra = '2007-01-01', datoTil = '2099-01-01', alle=1){

if (alle == 1) {
  RegData <- rapbase::LoadRegData(
    registryName="rygg",
    query=paste0('SELECT * FROM AlleVarNum WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\''),
    dbType="mysql")
  # RegDataV2 <- rapbase::LoadRegData(registryName="rygg",
  #                                              query='SELECT * FROM Uttrekk_Rapport', dbType="mysql")
} else {
  query <- paste0('SELECT
	AlderVedOpr,
	-- Antibiotika,
	AntibiotikaV3,
	-- AntNivOpr,
Arbstatus12mndV3,
Arbstatus3mndV3,
ArbstatusPreV3,
	ASA,
	SykehusNavn,
	AvdRESH,
	BMI,
  -- Bydelkode,
  -- Bydelsted,
	Dagkirurgi,
	EQ5DV312mnd,
	EQ5DV33mnd,
	EQ5DV3Pre,
	-- Eqangst12mnd,
	-- Eqangst3mnd,
	EqangstV3Pre,
	-- Eqgange12mnd,
	-- Eqgange3mnd,
	EqgangeV3Pre,
	ErstatningPre,
	-- FirstTimeClosed,
	-- FistTimeClosed,
	-- Fornoyd12mnd,
	-- Fornoyd3mnd,
	-- HFNavn,
	-- HovedInngrep,
	-- HovedInngreptxt,
	-- Inngrep,
	-- Inngreptxt,
	Kjonn,
	-- KnivtidTot,
	KommuneNr,
	KommuneNavn,
--	Kp3Mnd,
--	KpBlod3Mnd,
--	KpDVT3Mnd,
--	KpInf3Mnd,
--	KpInfDyp12Mnd,
--	KpInfDyp3Mnd,
--	KpInfOverfla3Mnd,
--	KpLE3Mnd,
--	KpLungebet3Mnd,
--	KpMiktProb3Mnd,
--	KpUVI3Mnd,
 	Liggedogn,
 	LiggetidPostOp,
MedForstLukket AS FirstTimeClosed,
Morsmal,
	-- MorsmalV3,
	-- Nytte12mnd,
	-- Nytte3mnd,
	-- OpAar,
	OpDato,
--  OpDeUlamin,
--  OpDeFasett,
	OpIndCauda,
	OpIndParese,
	OpIndPareseGrad,
	-- OpIndSme,
	-- OpIndSmeType,
	OpKat,
	-- OpLaminektomi,
	-- OpMikro,
	-- OpProlap,
	OpTilgangV3,
	-- OswTot12mnd,
	-- OswTot3mnd,
	OswTotPre,
	PeropKomp,
	PeropKompAnafy,
	PeropKompDura,
	PeropKompFeilnivSide,
	PeropKompFeilplassImp,
	PeropKompKardio,
	PeropKompNerve,
	PeropKompResp,
	PeropKompTransfuBlodning,
	PasientID,
	-- Region,
	-- Reop90d,
	ReopUnderOpph,
	RfDegenListeseMM,
	RfSentr,
	RfSpondtypeDegen,
	RokerV3,
	RvCt,
	-- RvDiscogr,
	-- RvDpregblok,
	RvFunksjo,
	RvMr,
	-- RvRadigr,
	RvRtgLscol,
	Saardren,
	SivilStatusV3,
	-- SmBe12mnd,
	-- SmBe3mnd,
	SmBePre,
	-- SmHypp12mnd,
	-- SmHypp3mnd,
	SmHyppPre,
	-- SmRy12mnd,
	-- SmRy3mnd,
	SmRyPre,
	-- SmSti12mnd,
	-- SmSti3mnd,
	SmStiPre,
	-- surgeonform_LIGGEDOEGN_POSTOPERATIV,
	-- surgeonform_LIGGEDOEGN_TOTALT,
	Sykd,
	SykdAndreRelevanteSykdBechtrew,
	SykdAnnenendokrin,
	SykdAnnenreumatisk,
	SykdCerebrovaskular,
	SykdDepresjonAngst,
	SykdGeneralisertSmSyndr,
	SykdHjertekar,
	SykdHoftekneartose,
	SykdHypertensjon,
	SykdKreft,
	SykdKroniskLunge,
	SykdKroniskNevrologisk,
	-- SykdKroniskSmerterMuskelSkjelettsyst,
	SykdOsteoporose,
	SykDprebetesMellitus,
	SykdReumatoidartritt,
	SykdVaskularClaudicatio,
	-- Sykehustype,
	SykemeldVarighPreV3,
	SymptVarighRyggHof,
	SympVarighUtstr,
	TidlIkkeOp,
	TidlOpAnnetNiv,
	-- TdllOpAnnetNiv,
	TidlOpsammeNiv,
	-- TideOp12mnd,
	-- TideOp3mnd,
	-- TidlOpr,
	-- TidlOprAntall,
	UforetrygdPre,
	Utd,
	-- Utfylt12Mnd,
	-- Utfylt3Mnd,
	UtskrivelseDato,
	Vekt
FROM AlleVarNum
  WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\'')

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

  VarV2 <- sort(names(RegDataV2))
  VarV3 <- sort(names(RegDataV3))
  #NB: Må tilpasse variabelnavn fra V2 til V3, eks Alder (V2), AlderVedOpr (V3)
  setdiff(VarV2, VarV3)
  [4] "Antibiotika"                          "AntNivOpr"                            "Arbstatus12mnd"
  [7] "Arbstatus3mnd"                        "ArbstatusPre"
  [10] ""                              ""                            "Bydelkode"
  [13] "Bydelsted"
                 "FusjonKirAlif"                        "FusjonKirAlifBen"
  [34] "FusjonKirAlifBur"                     "FusjonKirPlf"                         "FusjonKirPlfIntr"
  [37] "FusjonKirPlif"                        "FusjonKirTlif"                        "FusjonSpes"
  [40] "HFID"                                 "HFNavn"                               "HFReshID"
  [43] "HovedInngrep"                         "HovedInngreptxt"                      "Hoyde"
  [46] "Inngrep"                              "Inngreptxt"                           "Kommunenr"
  [49] "Kp3Mnd"                               "KpInf3Mnd"                            "KpInfDyp12Mnd"
  [52] "KpMiktProb3Mnd"                       "KpSarinfUspesType3Mnd"                "OpAndreSkiveprotese"
  [55] "OpAndreSpes"                          "OpDeFasett"                           "OpDeFasettUniBi"
  [58] "OpIndSme"                             "OpIndSmeType"                         "OpL23"
  [61] "OpL34"                                "OpL45"                                "OpL5S1"
  [64] "OpMikro"                              "OpProlap"                             "OpTilgang"
                                 "Personnummer"                         "PID"
  [70] "Region"                               "Reop90d"                              "RfAnnet"
  [73] "RfAnnetspes"                          "RfDegen"                              "RfForamino"
  [76] "RfNormal"                             "RfPseudom"                            "RfSkive"
  [79] "RfSynovpre"                           "Roker"                                "RvDiscogr"
  [82] "RvDpregblok"                          "RvRadigr"                             "SivilStatus"
  [85] "SkjemaIDIa"                           "SkjemaIDIIa"                          "SykdAnnet"
  [88] "SykdKroniskSmerterMuskelSkjelettsyst" "SykdVaskulærClaudicatio"              "Sykehustype"
  [91] "SykemeldVarighPre"                    "TideOp12mnd"                          "TideOp3mnd"
  [94] "TidlOpr"                              "TypeBen"                              "TypeBenBensub"
  [97] "Utfylt12Mnd"                          "Utfylt3Mnd"
                                 library(magrittr)
  #RegDataV2 %>%
    dplyr::rename(RegDataV2,
      Alder = AlderVedOpr,
      EQ5D12mnd = EQ5DV212mnd,
      EQ5D3mnd = EQ5DV23mnd,
      AvdNavn = SykehusNavn,
      AvdReshID = AvdRESH
    )


  V2ogV3 <- intersect(VarV2, VarV3)
  RegDataV2 <- RegDataV2[ , V2ogV3]
  V3ikkeV2 <- setdiff(VarV3, V2ogV3)
  RegDataV2[, V3ikkeV2] <- NA

  RegDataV2V3 <- rbind(RegDataV2[ ,VarV3],
                       RegDataV3[ ,VarV3])

}

return(RegDataV2V3)
}



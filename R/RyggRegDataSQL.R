#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg V3, dvs. fra ca 1.jan. 2019
#'
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQLgml <- function(datoFra = '2007-01-01', datoTil = '2099-01-01', alle=1){

if (alle == 1) {
  RegData <- rapbase::loadRegData(
    registryName="rygg",
    query=paste0('SELECT * FROM allevarnum WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\''),
    dbType="mysql")
  # RegDataV2 <- rapbase::loadRegData(registryName="rygg",
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
FROM allevarnum
  WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\'')


#FROM Uttrekk_Rapport ')

RegData <- rapbase::loadRegData(registryName="rygg", query=query, dbType="mysql")
}

return(RegData)
}



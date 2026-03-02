-- This file contains views for fairly complex to complex queries or queries that return interesting stuff (more than one report)
DROP VIEW IF EXISTS user_centre_list;
DROP VIEW IF EXISTS friendlyvars;
DROP VIEW IF EXISTS forlopsoversikt;
DROP VIEW IF EXISTS skjemaoversikt;
DROP VIEW IF EXISTS tablelist;
DROP VIEW IF EXISTS allevarnum;
DROP VIEW IF EXISTS allevar;
DROP VIEW IF EXISTS fritekstvar;
DROP VIEW IF EXISTS koblingstabell;
DROP VIEW IF EXISTS pasientliste, patientlist;
DROP VIEW IF EXISTS ForlopsOversikt;
DROP VIEW IF EXISTS SkjemaOversikt;
DROP VIEW IF EXISTS AlleVarNum;
DROP VIEW IF EXISTS AlleVar;
DROP VIEW IF EXISTS FritekstVar;
DROP VIEW IF EXISTS PasientListe, patientlist;

create view user_centre_list AS
select
  u.ID AS USER_ID,
  u.FIRSTNAME,
  u.LASTNAME,
  u.STATUS,
  u.TITLE,
  u.TSCREATED,
  u.TSLASTLOGIN,
  u.PHONE1,
  u.PHONE2,
  u.EMAIL,
  ug.ID as USERGROUP_ID,
  ug.DESCRIPTION AS USERGROUP_NAME,
  c.ID AS CENTRE_ID,
  c.CENTRENAME,
  c.CENTRESHORTNAME,
  ct.NAME AS CENTRE_TYPE
from
  user u,
  usergroup ug,
  centre c,
  centretype ct
where
  u.GROUPID = ug.ID
  and u.CENTREID = c.ID
  and c.TYPEID = ct.ID;

create view friendlyvars AS
select
  vc.ID AS CONTAINER_ID,
  vc.PARENTID AS PARENT_CONTAINER_ID,
  vc.DOCUMENT,
  vv.ID AS VAR_ID,
  vv.EXPLANATION,
  vc.ORDERNUMBER AS CONT_ORDER,
  vv.ORDERNUMBER AS VAR_ORDER,
  SUBSTRING_INDEX(vv.id, '_',1) AS TABLE_NAME,
  SUBSTRING(vv.ID,LOCATE('_',vv.ID)+1) AS FIELD_NAME,
  vv.TYPE,
  IFNULL(CASE
    WHEN vv.TYPE = 'org.openqreg.element.CheckBoxVariable' THEN (select t.text from text t where t.ID = concat(vv.ID,'_D') and t.LANGUAGEID='no')
    ELSE (select t.text from text t where t.ID = concat(vv.ID,'_TEXT_D') and t.LANGUAGEID='no')
  END,
  ( select TEXT from text t where t.LANGUAGEID = 'no' AND t.ID =  CONCAT(vv.ID, '_D'))
  )
  AS USER_APP_LABEL,
  (select REPLACE(TEXT, '<br>', '\n') from text t where t.LANGUAGEID = 'no' AND t.ID =  CONCAT(vv.ID, '_H')) AS HELPTEXT,
  fv.USER_SUGGESTION,
  fv.THEME,            -- Added as part of Metavariable project
  fv.DESCRIPTION,      -- Added as part of Metavariable project
  fv.LONG_DESC,        -- Added as part of Metavariable project
  fv.DERIVED_VAR,      -- Added as part of Metavariable project
  fv.ACTUAL_FRIENDLYNAME,
  fv.REGISTRATION_TYPE,
  fv.CHILD_TABLE_NAME,
  fv.CHILD_TABLE_FIELDNAME,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'min') AS MIN_VAL,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'max') AS MAX_VAL,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'warningmin') AS MIN_WARN,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'warningmax') AS MAX_WARN,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'mandatory') AS MANDATORY,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'maxnoofchars') AS MAXCHARS,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'allowedsigns') AS ALLOWEDSIGNS,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'noofdecimals') AS DECIMALS,
  (select ATTRIBUTEVALUE from varattribute where ID = vv.id and varattribute.ATTRIBUTENAME = 'stylesheet') AS STYLESHEET,
  (select ATTRIBUTEVALUE from varattribute where ID = vc.ID and varattribute.ATTRIBUTENAME = 'stylesheet') AS PARENT_STYLESHEET,
  (select SERVICE from varservice where ID = vv.ID) AS VAR_SERVICE,
  (select SERVICE from varservice where ID = vc.ID) AS CONT_SERVICE,
  (select VALIDFROM from varinterval where ID = vc.ID) AS VALID_FROM,
  (select VALIDTO from varinterval where ID = vc.ID) AS VALID_TO,
      (select VALIDFROM from varinterval where ID = vv.ID) AS VAR_VALID_FROM,
    (select VALIDTO from varinterval where ID = vv.ID) AS VAR_VALID_TO,
  (select TEXT from text t2 where t2.LANGUAGEID = 'no' AND t2.ID = REPLACE(vc.ID, '_RIGHT', '_TEXT_D')) AS ROW_LABEL
  from
    varcontainer vc LEFT JOIN varvar vv ON vc.ID = vv.PARENTID
    LEFT JOIN friendly_vars fv ON vv.ID = fv.FIELD_NAME
ORDER BY CONT_ORDER, VAR_ORDER asc
;


DROP VIEW IF EXISTS Brukerliste;
create view Brukerliste AS
select
  u.ID AS BrukerID,
  u.FIRSTNAME AS Fornavn,
  u.LASTNAME AS Etternavn,
  u.TITLE AS Tittel,
  u.TSCREATED AS OpprettetDato,
  u.TSLASTLOGIN AS SistInnlogget,
  u.PHONE1 AS Tlf1,
  u.PHONE2 AS Tlf2,
  u.EMAIL AS Epost,
  ug.ID as Rolle,
  ug.DESCRIPTION AS RolleBeskrivelse,
  c.ID AS AvdRESH,
  c.CENTRENAME AS RESHNavnFullt,
  c.CENTRESHORTNAME SykehusKort,
  NULL AS Enhetstype,
  c.CENTRENAME AS Sykehusnavn
from
  user u,
  usergroup ug,
  centre c
where
  u.GROUPID = ug.ID
  and u.CENTREID = c.ID
  and u.GROUPID != 'SYSTEMSOWNUSER';
  -- AND c.ID =! '106006'

-- Genererated as version 1.0
CREATE VIEW forlopsoversikt AS
SELECT
-- Hospital/centre stuff
m.CENTREID AS AvdRESH,
getFriendlyName(m.CENTREID) AS SykehusNavn,
-- Patient stuff
CAST(p.ID AS CHAR(10)) AS PasientID,
-- NEXT 6 left empty for now
CAST(NULL AS CHAR(4)) AS PostNr,
CAST(NULL AS CHAR(50)) AS PostSted,
CAST(NULL AS CHAR(50)) AS Kommune,
CAST(NULL AS CHAR(4)) AS Kommunenr,
CAST(NULL AS CHAR(50)) AS Fylke,
CAST(NULL AS CHAR(2)) AS Fylkenr,
p.SSN AS KryptertFnr,
CASE
	WHEN IFNULL(p.GENDER,0) = 0 THEN 'Ikke angitt'
	WHEN p.GENDER = 1 THEN 'Mann'
	WHEN p.GENDER = 2 THEN 'Kvinne'
	WHEN p.GENDER = 9 THEN 'Ikke relevant'
	ELSE 'Ukjent'
END AS PasientKjonn,
 CASE
	WHEN p.GENDER = 1 THEN '1'
	WHEN p.GENDER = 2 THEN '0'
	ELSE NULL
END AS erMann,
datediff(surgeonform.OPERASJONSDATO, p.BIRTH_DATE) / 365.25 AS PasientAlder,
p.BIRTH_DATE AS Fodselsdato,
-- TODO some of these might be excluded and you need to send NULL instead for missing columns
-- Choose one or the other for your view
CAST(NULL AS CHAR(10)) AS Norsktalende,
getListText('PATIENTFORM_SIVILSTATUS',patientform.Sivilstatus) AS Sivilstatus,
-- CAST(NULL AS CHAR(30)) AS Sivilstatus,
 CASE patientform.UTDANNING
	WHEN 1 THEN '01 Grunnskolenivå'
	WHEN 2 THEN '02a Videregående skolenivå'
	WHEN 3 THEN '02a Videregående skolenivå'
	WHEN 4 THEN '03a Universitets- og høgskolenivå kort'
	WHEN 5 THEN '04a Universitets- og høgskolenivå lang'
	WHEN 9 THEN '09a Uoppgitt eller ingen fullført utdanning'
	ELSE '09a Uoppgitt eller ingen fullført utdanning'
END AS UtdanningSSB,
-- CAST(NULL AS CHAR(50)) AS Utdanning,
getListText('PATIENT_DECEASED',p.DECEASED) AS DodPasient,
p.DECEASED_DATE AS DodsDato,
-- Event stuff
CAST(m.MCEID AS CHAR(10)) AS ForlopsID,
CAST(LEAST( patientform.STATUS, surgeonform.STATUS) AS CHAR(2)) AS BasisRegStatus,
-- TODO next 4 should be something like m.MCETYPE or somesuch
NULL AS ForlopsType1, -- FIXME
CAST(1 AS CHAR(10)) AS ForlopsType1Num,
CAST(NULL AS CHAR(25)) AS ForlopsType2,  -- FIXME
CAST(NULL AS UNSIGNED) AS ForlopsType2Num,  -- FIXME
CAST(NULL AS CHAR(10)) AS KobletForlopsID, -- FIXME
surgeonform.OPERASJONSDATO AS HovedDato,
 -- TODO validate - you might need a CASE statement on intervention/mce type instead...
-- Followup stuff
NULL  AS OppflgRegStatus,
'0' AS ErOppflg,
NULL AS OppflgStatus,
NULL AS OppflgSekNr
FROM
mce m INNER JOIN patient p ON m.PATIENT_ID = p.ID
INNER JOIN patientform patientform ON m.MCEID = patientform.MCEID
INNER JOIN surgeonform surgeonform ON m.MCEID = surgeonform.MCEID
;

-- Genererated as version 1.0
CREATE VIEW skjemaoversikt AS
SELECT
	CAST((SELECT t.text FROM text t WHERE t.ID = (
		SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFORM'
	)
	AND t.LANGUAGEID='no') AS CHAR(100)) AS Skjemanavn,
	CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
	CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
	skjema.CREATEDBY AS OpprettetAv,
	skjema.TSCREATED AS OpprettetDato,
	skjema.UPDATEDBY AS SistLagretAv,
	skjema.TSUPDATED AS SistLagretDato,
	getFriendlyName(c.ID) AS Sykehusnavn,
	(SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
	c.ID AS AvdRESH,
		CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFORM'
	) AS CHAR(2)) AS SkjemaRekkeflg
FROM
	patientform skjema,
	centre c
	WHERE skjema.CENTREID = c.ID

UNION

SELECT
	CAST((SELECT t.text FROM text t WHERE t.ID = (
		SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'SURGEONFORM'
	)
	AND t.LANGUAGEID='no') AS CHAR(100)) AS Skjemanavn,
	CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
	CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
	skjema.CREATEDBY AS OpprettetAv,
	skjema.TSCREATED AS OpprettetDato,
	skjema.UPDATEDBY AS SistLagretAv,
	skjema.TSUPDATED AS SistLagretDato,
	getFriendlyName(c.ID) AS Sykehusnavn,
	(SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
	c.ID AS AvdRESH,
		CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'SURGEONFORM'
	) AS CHAR(2)) AS SkjemaRekkeflg
FROM
	surgeonform skjema,
	centre c
	WHERE skjema.CENTREID = c.ID

UNION

SELECT
    CAST((SELECT t.text FROM text t WHERE t.ID = (
        SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP'
    )
                                      AND t.LANGUAGEID='no') AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    getFriendlyName(c.ID) AS Sykehusnavn,
    (SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
    c.ID AS AvdRESH,
    CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP'
        ) AS CHAR(2)) AS SkjemaRekkeflg
FROM
    patientfollowup skjema,
    centre c
WHERE skjema.CENTREID = c.ID AND skjema.CONTROL_TYPE=3

UNION

SELECT
    CAST((SELECT t.text FROM text t WHERE t.ID = (
        SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP'
    )
                                      AND t.LANGUAGEID='no') AS CHAR(100)) AS Skjemanavn,
    CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
    CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
    skjema.CREATEDBY AS OpprettetAv,
    skjema.TSCREATED AS OpprettetDato,
    skjema.UPDATEDBY AS SistLagretAv,
    skjema.TSUPDATED AS SistLagretDato,
    getFriendlyName(c.ID) AS Sykehusnavn,
    (SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
    c.ID AS AvdRESH,
    CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP12'
        ) AS CHAR(2)) AS SkjemaRekkeflg
FROM
    patientfollowup skjema,
    centre c
WHERE skjema.CENTREID = c.ID AND skjema.CONTROL_TYPE=12
;

-- View for Datadump dropdown view
select 'Creating view tablelist' as 'action';
CREATE VIEW tablelist AS
    SELECT DISTINCT TABLE_NAME AS TABLE_NAME
    FROM information_schema.tables
    WHERE TABLE_TYPE IN ('BASE TABLE','VIEW')
      AND TABLE_NAME IN ('forlopsoversikt','skjemaoversikt', 'allevarnum');

-- View for allevarnum
CREATE VIEW allevarnum AS
SELECT
    mce.PATIENT_ID AS PasientID,
    mce.CENTREID AS AvdRESH,
    getFriendlyName(mce.CENTREID) AS SykehusNavn,
    mce.MCEID AS ForlopsID,
    mce.MCETYPE AS MceType,
    date_format(patientform.UTFYLLING_DATO,'%Y-%m-%d') as  UtfyltDato,
    patientform.ADDR_TYPE AS Adressetype,
    patientform.ADDRESS AS Adresse,
    patientform.ZIPCODE AS PostNr,
    patientform.TOWN AS PostSted,
    patientform.DISTRICTCODE AS Bydelskode,
    patientform.DISTRICTNAME AS Bydelsnavn,
    patientform.MUNICIPALITY_NUMBER AS KommuneNr,
    patientform.MUNICIPALITY_NAME AS KommuneNavn,
    patientform.COUNTY AS Fylke,
    patientform.REGIONAL_HEALTH_AUTHORITY  AS HelseRegion,
    patient.GENDER as Kjonn,
    patient.DECEASED AS DodPasient,
    patient.DECEASED_DATE AS DodsDato,
    patientform.FREEFORM_ADDR AS Fritekstadresse,
    patientform.ROYKING AS RokerV3,
    patientform.SNUFF AS Snuser,
    patientform.HOYDE AS Hoyde,
    patientform.HOYDE_MISS AS HoydeMangler,
    patientform.VEKT AS Vekt,
    patientform.VEKT_MISS AS VektMangler,
    patientform.BMI AS BMI,
    patientform.BMI_CATEGORY AS BMIkategori,
    patientform.TIDLIGEREBEHANDLING AS IkkeKirBehTidl,
    patientform.FYSIOTRENING AS FysioTrening,
    patientform.FYSIOANNEN AS FysioAnnenBeh,
    patientform.MANUELLTERAPI AS ManuellTerapi,
    patientform.PSYKOMOTORFYSIO AS FysioPsykoMotorisk,
    patientform.KIROPRAKTOR AS Kiropraktor,
    patientform.TVERRFAGLIGBEHANDLING AS TvFagligBeh,
    patientform.ANNENBEHANDLING AS AnnenIkkeKirBeh,
    patientform.VARIGHET_RYGGHOFTESMERTER AS SymptVarighRyggHof,
    patientform.VARIGHET_STRAALESMERTER AS SympVarighUtstr,
    patientform.VENTETID_FRA_HENVISNING AS VentetidHenvTilSpesialist,
    patientform.VENTETID_TIL_OPERASJON AS VentetidSpesialistTilOpr,
    patientform.SMERTESTILLENDE AS SmStiPre,
    patientform.SMERTESTILLENDE_BRUK AS SmHyppPre,
    patientform.RYGGHOFTESMERTE AS SmRyPre,
    patientform.BENSMERTE AS SmBePre,
    patientform.ODI_SMERTE AS OswsmertePre,
    patientform.ODI_PERSONLIG_STELL AS OswstelPre,
    patientform.ODI_LOFTING AS OswloftPre,
    patientform.ODI_GAAING AS OswgaaPre,
    patientform.ODI_SITTING AS OswsittPre,
    patientform.ODI_STAAING AS OswstaaPre,
    patientform.ODI_SOVING AS OswsovePre,
    patientform.ODI_SEKSUALLIV AS OswsexPre,
    patientform.ODI_SOSIALT AS OswsosiPre,
    patientform.ODI_REISING AS OswreisPre,
    patientform.ODI_SCORE AS OswTotPre,
    patientform.EQ5D_GANGE AS EqgangeV3Pre,
    patientform.EQ5D_PERSONLIG_STELL AS EqperstV3Pre,
    patientform.EQ5D_VANLIGE_GJOREMAL AS EqvanlgjV3Pre,
    patientform.EQ5D_SMERTE_UBEHAG AS EqsmerteV3Pre,
    patientform.EQ5D_ANGST_DEPRESJON AS EqangstV3Pre,
    patientform.EQ5D_SCORE AS EQ5DV3Pre,
    patientform.HELSETILSTAND_SCALE AS HelsetilstPre,
    patientform.HELSETILSTAND_SCALE_MISS AS HelsetilstPreMangler,
    patientform.PAIN_WORK_Q1 AS SmJobbKroniskSm,
    patientform.PAIN_WORK_Q2 AS SmJobbFravaer,
    patientform.PAIN_WORK_Q3 AS SmJobbOktSm,
    patientform.PAIN_WORK_Q4 AS SmJobbIkkeJobbe,
    patientform.SIVILSTATUS AS SivilStatusV3,
    patientform.LESEOGSKRIVEVANSKER AS LeseSkriveVansker,
    patientform.ARBEIDSSTATUS AS ArbstatusPreV3,
    patientform.SYKEMELDT_PROSENT AS SykemeldProsPre,
    patientform.UFOR_PROSENT AS UforProsPre,
    patientform.SYKEMELDT_SAMMENHENGENDE AS SykemeldVarighPreV3,
    patientform.TILBAKETILJOBB AS SykemeldArbeidsgiver,
    patientform.SOKTUFOREPENSJON AS UforetrygdPre,
    patientform.ERSTATNINGFORSIKRING AS ErstatningPre,
    patientform.TUNGTARBEID AS ArbeidTungt,
    patientform.ENSFORMIGARBEID AS ArbeidEnsformig,
    patientform.MORSMAL AS Morsmal,
    patientform.MORSMAL_SPESIFISER AS AnnetMorsm,
    patientform.FODTINORGE AS FodtiNorge,
    patientform.ETNISKTILHORIGHET AS EtnKultTilhorighet,
    patientform.UTDANNING AS Utd,
    patientform.STATUS AS StatusPasSkjema,
    patientform.FIRST_TIME_CLOSED_BY AS ForstLukketAVPreOp,
    patientform.FIRST_TIME_CLOSED AS ForstLukketPreOp,
    date_format(surgeonform.INNLEGGELSES_DATO,'%Y-%m-%d') AS InnlagtDato,
    date_format(surgeonform.OPERASJONSDATO,'%Y-%m-%d') AS OpDato,
    surgeonform.ALDER_VED_OPERASJON AS AlderVedOpr,
    surgeonform.HOVEDSAKLIGSPINALKIRURGI AS HovedSpinalKirurg,
    surgeonform.REGELMESSIG_SPINALKIRURGI AS HKirurgErfaring,
    surgeonform.AAR_REGELMESSIG_SPINALKIRURGI AS HKirurgErfaringAar,
    surgeonform.ASSISTENTOPERASJON AS KirurgAssistent,
    surgeonform.TIDLIGERRYGGOPERASJON_JASAMMENIVA AS TidlOpsammeNiv,
    surgeonform.TIDLIGERRYGGOPERASJON_JAANNETNIVA AS TidlOpAnnetNiv,
    surgeonform.TIDLIGERRYGGOPERASJON_NEI AS TidlIkkeOp,
    surgeonform.TIDLIGERRYGGOPERASJON_ANTALL AS TidlOprAntall,
    surgeonform.SISTETIDLIGERINNGREP AS ForrigeInngrep,
    surgeonform.FASTBLODFORTYNNENDEMED AS BlodfortynnendeFast,
    surgeonform.FASTBLODFORTYNNENDEMED_SPESIFISER AS BlodfortynnendeSpes,
    date_format(surgeonform.BLODFORTYNNENDESEPODATO,'%d-%m-%Y') AS BlodfortynnendeSepDato,
    surgeonform.POSTTROMBOSE AS PostopTrombProfyl,
    surgeonform.POSTTROMBOSE_SPESIFISER AS SpesTrombProfyl,
    surgeonform.FORSTEDOSETROMBOSE_JA AS BlodfortynnendePreop,
    surgeonform.STEROIDIMMUNOBEH AS ImmunSuppr,
    surgeonform.SPANDRESYKDOMSKADERPLAGE AS Sykd,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_REUMATOID AS SykdReumatoidartritt,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_BECHTEREW AS SykdAndreRelevanteSykdBechtrew,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_ANNENREUMSYK AS SykdAnnenreumatisk,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_HOFTKNEARTOSE AS SykdHoftekneartose,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_DEPRESJONANGST AS SykdDepresjonAngst,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_GENSMERTE AS SykdGeneralisertSmSyndr,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_KRONNEVSYK AS SykdKroniskNevrologisk,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_CEREBROVSYK AS SykdCerebrovaskular,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_HJERTEKARSYK AS SykdHjertekar,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_POLYNERV AS SykdPolynevropati,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_VASKCLAUD AS SykdVaskularClaudicatio,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_KRONLUNG AS SykdKroniskLunge,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_KREFT AS SykdKreft,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_OSTEO AS SykdOsteoporose,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_OSTEOBRUDDCOL AS SykdOsteoporoseBrudd,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_HYOERTENSJON AS SykdHypertensjon,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_DIABETES AS SykDprebetesMellitus,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_ENDOKTRIN AS SykdAnnenendokrin,
    surgeonform.DEFANDRESYKDOMSKADERPLAGE_PROSTATISME AS SykdProstatisme,
    surgeonform.ANDRESYKDOMSKADERPLAGE_SPESIFISER AS SykdAnnetspesifiser,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_CT AS RvCt,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_MR AS RvMr,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_RONTGEN_LS_COLUMN AS RvRtgLscol,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_RTG_FLEKSJON_EKSTENSJON AS RvFunksjo,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_RTG_FLEKSJON_EKSTENSJON_MM AS RvFunksjoTranslMM,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_RTG_FLEKSJON_EKSTENSJON_GRADER AS RvFunksjoVinkelEndrGr,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_DIAGNOST_BLOKADE AS RvDpregblok,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_DIAGNOST_FACETTLEDD AS RvBlokadeFacett,
    surgeonform.RADIOLOGI_UNDERSOEKELSE_DIAGNOST_NERVEROT AS RvBlokadeNerverot,
    surgeonform.RADIOLOGI_FUNN_PROLAPS AS RfSkive,
    surgeonform.RADIOLOGI_FUNN_INTRAFORAMINAL_PROLAPS AS RfIntrforaminaltProl,
    surgeonform.RADIOLOGI_FUNN_EKSTREMT_LATERALT_EKSTRAFOAMINALT_PROLAPS AS RfEkstrLatProl,
    surgeonform.RADIOLOGI_FUNN_SENTRAL_SPINALSTENOSE AS RfSentr,
    surgeonform.RADIOLOGI_FUNN_LATERAL_RECESS_STENOSE AS RfLateral,
    surgeonform.RADIOLOGI_FUNN_FORAMINAL_STENOSE AS RfForaminalSS,
    surgeonform.RADIOLOGI_FUNN_SKIVEDEGENERAL_UTEN_NERVE_AFFEKSJON AS RFKunDegenerasjon,
    surgeonform.RADIOLOGI_FUNN_DEGENERATIV_SPONDYLOLISTESE AS RfSpondtypeDegen,
    surgeonform.RADIOLOGI_FUNN_DEGENERATIV_SPONDYLOLISTESE_MM AS RfDegenListeseMM,
    surgeonform.RADIOLOGI_FUNN_DEGENERATIV_SKOLIOSE AS RfDegskol,
    surgeonform.RADIOLOGI_FUNN_KYFOTISK_FEILSTILLING AS RfKyfose,
    surgeonform.RADIOLOGI_FUNN_SYNOVIAL_CYSTE AS RfSynovpre,
    surgeonform.RADIOLOGI_FUNN_ISTMISK_SPONDYLYSE AS RfIstmiskLyse,
    surgeonform.RADIOLOGI_FUNN_ISTMISK_SPONDYLOLISTESE AS RfSpondtypeIsmisk,
    surgeonform.RADIOLOGI_FUNN_ISTMISK_SPONDYLOLISTESE_GRAD AS RfMeyerdingGrad,
    surgeonform.RADIOLOGI_MODIC AS RfModic,
    surgeonform.RADIOLOGI_MODIC_TYPE_I AS RfModicTypeI,
    surgeonform.RADIOLOGI_MODIC_TYPE_I_AKTUELT_NIVA AS RfTypeIAktNivaa,
    surgeonform.RADIOLOGI_MODIC_TYPE_I_ANNET_NIVA AS RfTypeIAnnetNivaa,
    surgeonform.RADIOLOGI_MODIC_TYPE_II AS RfModicTypeII,
    surgeonform.RADIOLOGI_MODIC_TYPE_II_AKTUELT_NIVA AS RfTypeIIAktNivaa,
    surgeonform.RADIOLOGI_MODIC_TYPE_II_ANNET_NIVA AS RfTypeIIAnnetNivaa,
    surgeonform.OPERASJON_SKOLIOSE AS OpSkolioseKyfose,
    surgeonform.RADIOLOGI_COBBS AS OpSkolioseCobb,
    surgeonform.RADIOLOGI_COBBS_VINKELGRAD AS OpSkolioseCobbGrader,
    surgeonform.RADIOLOGI_SVA AS OpKyfoseSVA,
    surgeonform.RADIOLOGI_SVA_CM AS OpKyfoseSVAcm,
    surgeonform.RADIOLOGI_PI AS OpKyfosePI,
    surgeonform.RADIOLOGI_PI_GRAD AS OpKyfosePIGrader,
    surgeonform.RADIOLOGI_PT AS OpKyfosePT,
    surgeonform.RADIOLOGI_PT_GRAD AS OpKyfosePTGrader,
    surgeonform.RADIOLOGI_SS AS OpKysfoseSS,
    surgeonform.RADIOLOGI_SS_GRAD AS OpKysfoseSSGrader,
    surgeonform.RADIOLOGI_LL AS OpKyfoseLL,
    surgeonform.RADIOLOGI_LL_GRAD AS OpKyfoseLLGrader,
    surgeonform.NEVROL_SYMP_FUNN_PARESE AS OpIndParese,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_GRAD AS OpIndPareseGrad,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_LESS24 AS PareseUnderEttDogn,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_LESS24_HOURS AS PareseAntTimer,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_LESSWEEK AS PaseseUnderEnUke,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_LESSWEEK_DAYS AS PareseAntDogn,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_ONEWEEKTOMONTH AS PareseEnUkeTilTreMnd,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_ONEWEEKTOMONTH_WEEKS AS PareseAntUker,
    surgeonform.NEVROL_SYMP_FUNN_PARESE_MORE3MONTH AS PareseOverTreMnd,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA AS OpIndCauda,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_LESS24 AS CaudaUnderEttDogn,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_LESS24_HOURS AS CaudaAntTimer,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_LESSWEEK AS CaudaUnderEnUke,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_LESSWEEK_DAYS AS CaudaAntDogn,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_ONEWEEKTOMONTH AS CaudaEnUkeTilTreMnd,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_ONEWEEKTOMONTH_WEEKS AS CaudaAntUker,
    surgeonform.NEVROL_SYMP_FUNN_CAUDA_MORE3MONTH AS CaudaOverTreMnd,
    surgeonform.NEVROL_SYMP_FUNN_POSITIV_LASEGUE AS KliniskPosLasegue,
    surgeonform.NEVROL_SYMP_FUNN_FLEKSJONSLINDRING AS KliniskFleksjonLindring,
    surgeonform.OPERASJONSKATEGORI_ADMITTYPE AS OpKat,
    surgeonform.OPERASJONSKATEGORI_DAGKIRURGI AS Dagkirurgi,
    surgeonform.ASA_GRAD AS ASA,
    surgeonform.OPERASJONSMETODE_SAFESURGERY AS SafeSurgery,
    surgeonform.OPERASJONSMETODE_MIKRO_MAKRO_ENDO AS OpMikroV3,
    surgeonform.OPERASJONSMETODE_COMPUTERNAV AS OpComputerNav,
    surgeonform.OPERASJONSMETODE_PROLAPSEKSTIRPASJON_ENDO AS OprProlap,
    surgeonform.OPERASJONSMETODE_DECOMPRESION AS OpDeUlamin,
    surgeonform.OPERASJONSMETODE_DECOMPRESION_SPECIFY AS OpDeUlaminTilgang,
    surgeonform.OPERASJONSMETODE_DECOMPRESION_PRO_SPIN_OSTEOTOMI AS OpProOsteotomi,
    surgeonform.OPERASJONSMETODE_DECOMPRESION_LAMINEKTOMI AS OpLaminektomi,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_PERKUTAN_FUSJONSKIRURGI AS OpFusjonPerkutan,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_SKIVEPROTESE AS OpAndreSkiveprotese,
    surgeonform.OPERASJONSMETODE_OTHER_FUSJON_NO_DEKOMPRESION AS OpFusjonUtenDekomprV3,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_INSPECT_OSTEOTOMI AS OpOsteosyntRevV3,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_REMOVE_OSTEOSYTNTESE AS OpOsteosyntFjerningV3,
    surgeonform.OPERASJONSMETODE_OTHER_PSO_KILEOSTEOTOMI AS OpKileOsteotomi,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_INTERPED_OSTEOTOMI AS OpPonteSPOsteotomi,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_ANNET AS OpAnnenOsteotomi,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_ANNET_SPESIFISER AS OpAndreSpes,
    surgeonform.OPTILGANG AS OpTilgangV3,
    surgeonform.NIVAASIDETH12L1 AS OpTh12L1,
    surgeonform.NIVAASIDEL12 AS OpL1L2,
    surgeonform.NIVAASIDEL23 AS OpL23,
    surgeonform.NIVAASIDEL3L4 AS OpL34,
    surgeonform.NIVAASIDEL4L5 AS OpL45,
    surgeonform.NIVAASIDEL5S1 AS OpL5S1,
    surgeonform.NIVAASIDE_ANNET AS DekomprAnnetNivaa,
    surgeonform.NIVAASIDE_ANNET_SPESIFISER AS DekomrSpesAnnetNivaaDekomrSpesAnnetNivaa,
    surgeonform.NIVAASIDE_ANTALL_DEKOMPRIMERT AS DekompAntNivaa,
    surgeonform.FUSJONSKIRURGI_INNGANG AS FusjonKir,
    surgeonform.FUSJONSKIRURGI_POSTEROLATERAL_FUSJON AS FusjonKirPlf,
    surgeonform.FUSJONSKIRURGI_POSTEROLATERAL_INSTRUMENTELL AS FusjonKirPlfInstr,
    surgeonform.FUSJONSKIRURGI_POSTEROLATERAL_INSTRUMENTELL_IKKE AS FusjonKirPlfIkkeInstr,
    surgeonform.FUSJONSKIRURGI_ALIF AS FusjonKirAlif,
    surgeonform.FUSJONSKIRURGI_TLIF AS FusjonKirTlif,
    surgeonform.FUSJONSKIRURGI_PLIF AS FusjonKirPlif,
    surgeonform.FUSJONSKIRURGI_XLIF AS FusjonKirXlif,
    surgeonform.TYPEBENGRAFT_AUTOGRAFT AS BenAutogrType,
    surgeonform.TYPEBENGRAFT_AUTGRAFT_LOKALT AS BenAutoLokalt,
    surgeonform.TYPEBENGRAFT_AUTGRAFT_HOFTEKAM AS BenAutoHofte,
    surgeonform.TYPEBENGRAFT_BENSUBSTITUTT AS BenSubstitutt,
    surgeonform.TYPEBENGRAFT_BANKE_BEN AS BenBank,
    surgeonform.NIVAA_FUSJONERT_OVERST AS FusjonOvreNivaa,
    surgeonform.NIVAA_FUSJONERT_NEDERST AS FusjonNedreNivaa,
    surgeonform.NIVAA_FUSJONERT_ANTALL AS FusjonAntNivaa,
    surgeonform.NIVAA_FUSJONERT_ILEUMSKRUE AS FusjonIleumSkrue,
    surgeonform.NIVAA_FUSJONERT_ILEUMSKRUE_LATERAL AS IleumSkrueUniBiLat,
    surgeonform.SEMENTERTE_SKRUVE AS FusjonSement,
    surgeonform.ANTIBIOTIKA AS AntibiotikaV3,
    surgeonform.ANTIBIOTIKA_MEDIKAMENT AS AntibiotikaMedikament,
    surgeonform.ANTIBIOTIKA_DOSE AS AntibiotikaDose,
    surgeonform.ANTIBIOTIKA_ANTALL_DOSER AS AntibiotikaAntDoser,
    surgeonform.ANTIBIOTIKA_INTERVALL_KUN_OPERASJONSDAGEN AS AntibiotikaKunOprDag,
    surgeonform.ANTIBIOTIKA_INTERVALL_EVT_ANTALL_DOEGN AS AntibiotikaEvtAntDogn,
    surgeonform.ANTIBIOTIKA_ANTALL_DOEGN AS AntibiotikaAntDogn,
    surgeonform.SAARDREN AS Saardren,
    surgeonform.KNIVTID_KLOKKESLETT_START_TIMER AS KnivStartKlokkeTime,
    surgeonform.KNIVTID_KLOKKESLETT_START_MIN AS KnivStartKlokkeMin,
    surgeonform.KNIVTID_KLOKKESLETT_SLUTT_TIMER AS KnivSluttKlokkeTime,
    surgeonform.KNIVTID_KLOKKESLETT_SLUTT_MIN AS KnivSluttKlokkeMin,
    surgeonform.KNIVTID_EXACT_TIMER AS KnivTidTimerVarighet,
    surgeonform.KNIVTID_EXACT_MIN AS KnivTidMinVarighet,
    surgeonform.KNIVTID_MIN AS KnivtidTot,
    surgeonform.KOMPLIKASJONER_INNGANG AS PeropKomp,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_DURARIFT_LIQUORLEKASJE AS PeropKompDura,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_NERVEROTSKADE AS PeropKompNerve,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_OP_FEIL_NIVAA AS PeropKompFeilnivSide,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_FEILPLASSERING_IMPLANTAT AS PeropKompFeilplassImp,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_BLOEDNING AS PeropKompTransfuBlodning,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_RESPIRATORISKE AS PeropKompResp,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_KARDIOVASKULAERE AS PeropKompKardio,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANAFYLAKSI AS PeropKompAnafy,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANNET AS PeropKompAnnet,
    surgeonform.OP_KODE1 AS ProsKode1,
    surgeonform.OP_INDICATION2 AS ProsKode2,
    date_format(surgeonform.UTDATO,'%Y-%m-%d') AS UtskrivelseDato,
    surgeonform.LIGGEDOEGN_POSTOPERATIV AS LiggetidPostop,
    surgeonform.LIGGEDOEGN_TOTALT AS Liggedogn,
    surgeonform.DOEDSFALL_UNDER_OPPHOLDET AS DodsfallOpphold,
    surgeonform.REOPERERT AS ReopUnderOpph,
    surgeonform.FIRST_TIME_CLOSED_BY AS ForstLukketLegeAV,
    surgeonform.FIRST_TIME_CLOSED AS ForstLukketLege,
    surgeonform.STATUS AS StatusLegeSkjema,
    surgeonform.FIRST_TIME_CLOSED_BY AS ForstLukketAVMed,
    surgeonform.FIRST_TIME_CLOSED AS ForstLukketLege,
    date_format(patientfollowup.UTFYLLING_DATO,'%Y-%m-%d')  AS Utfdato3mnd,
    patientfollowup.NYTTE_AV_OPERASJON AS Nytte3mnd,
    patientfollowup.FORNOYDHET_MED_OPERASJON AS Fornoyd3mnd,
    patientfollowup.SMERTE_VAS_RYGGHOFTE AS SmRy3mnd,
    patientfollowup.SMERTE_VAS_BEIN AS SmBe3mnd,
    patientfollowup.KOMPLIKASJONER_BLODNING AS KpBlod3mnd,
    patientfollowup.KOMPLIKASJONER_URINVEI AS KpUVI3mnd,
    patientfollowup.KOMPLIKASJONER_LUNGE AS KpLungebet3mnd,
    patientfollowup.KOMPLIKASJONER_TROMBOSE_BEN AS KpDVT3mnd,
    patientfollowup.KOMPLIKASJONER_TROMBOSE_LUNGE AS KpLE3mnd,
    patientfollowup.KOMPLIKASJONER_INFEKSJON_OVERFLATE AS KpInfOverfla3mnd,
    patientfollowup.KOMPLIKASJONER_INFEKSJON_DYP AS KpInfDyp3mnd,
    patientfollowup.KOMPLIKASJONER_ETTER_OPERASJON AS NySykdSkade3mnd,
    patientfollowup.KOMPLIKASJONER_LEDDSMERTE AS NyLeddSm3mnd,
    patientfollowup.KOMPLIKASJONER_KREFTSYKDOM AS Nykreft3mnd,
    patientfollowup.KOMPLIKASJONER_HJERTEKAR AS NyHjerteKar3mnd,
    patientfollowup.KOMPLIKASJONER_ANNEN_NERVESYKDOM AS NyNerveSkyd3mnd,
    patientfollowup.KOMPLIKASJONER_SKADE_FOLGETILSTAND AS NySkade3mnd,
    patientfollowup.KOMPLIKASJONER_ANNEN_VESENTLIG_SYKDOM AS NyAnnen3mnd,
    patientfollowup.ODI_SMERTE AS Oswsmerte3mnd,
    patientfollowup.ODI_PERSONLIG_STELL AS Oswstel3mnd,
    patientfollowup.ODI_LOFTING AS Oswloft3mnd,
    patientfollowup.ODI_GAAING AS Oswgaa3mnd,
    patientfollowup.ODI_SITTING AS Oswsitt3mnd,
    patientfollowup.ODI_STAAING AS Oswstaa3mnd,
    patientfollowup.ODI_SOVING AS Oswsove3mnd,
    patientfollowup.ODI_SEKSUALLIV AS Oswsex3mnd,
    patientfollowup.ODI_SOSIALT AS Oswsosi3mnd,
    patientfollowup.ODI_REISING AS Oswreis3mnd,
    patientfollowup.ODI_SCORE AS OswTot3mnd,
    patientfollowup.EQ5D_GANGE AS EqgangeV33mnd,
    patientfollowup.EQ5D_PERSONLIG_STELL AS EqperstV33mnd,
    patientfollowup.EQ5D_VANLIGE_GJOREMAL AS EqvanlgjV33mnd,
    patientfollowup.EQ5D_SMERTE_UBEHAG AS EqsmerteV33mnd,
    patientfollowup.EQ5D_ANGST_DEPRESJON AS EqangstV33mnd,
    patientfollowup.EQ5D_SCORE AS EQ5D3mndV3,
    patientfollowup.HELSETILSTAND_IDAG AS Helsetilst3mnd,
    patientfollowup.SMERTESTILLENDE AS SmSti3mnd,
    patientfollowup.SMERTESTILLENDE_BRUK AS SmHypp3mnd,
    patientfollowup.ARBEIDSSTATUS AS Arbstatus3mndV3,
    patientfollowup.SYKEMELDT_PROSENT AS SykemeldPros3mnd,
    patientfollowup.UFORETRYGDET_PROSENT AS UforPst3mnd,
    patientfollowup.TILBAKETILJOBB AS JobbArbGiver3mnd,
    patientfollowup.SOKT_UFOR AS Uforetrygd3mnd,
    patientfollowup.SOKT_ERSTATNING AS Erstatning3mnd,
    patientfollowup.REOPERERT_ETTER_OPERASJON AS NyRyggOpr3mnd,
    patientfollowup.REOPERERT_ANTALL AS NyOprAnt3mnd,
    patientfollowup.REOPERERT_OMRAADE AS NyOprNivaa3mnd,
    patientfollowup.STATUS AS Status3mnd,
    patientfollowup.FIRST_TIME_CLOSED_BY AS ForstLukketAV3mnd,
    patientfollowup.FIRST_TIME_CLOSED AS ForstLukket3mnd,
    date_format(patientfollowup12.UTFYLLING_DATO,'%Y-%m-%d')  AS Utfdato12mnd,
    patientfollowup12.NYTTE_AV_OPERASJON AS Nytte12mnd,
    patientfollowup12.FORNOYDHET_MED_OPERASJON AS Fornoyd12mnd,
    patientfollowup12.SMERTE_VAS_RYGGHOFTE AS SmRy12mnd,
    patientfollowup12.SMERTE_VAS_BEIN AS SmBe12mnd,
    patientfollowup12.KOMPLIKASJONER_BLODNING AS KpBlod12mnd,
    patientfollowup12.KOMPLIKASJONER_URINVEI AS KpUVI12mnd,
    patientfollowup12.KOMPLIKASJONER_LUNGE AS KpLungebet12mnd,
    patientfollowup12.KOMPLIKASJONER_TROMBOSE_BEN AS KpDVT12mnd,
    patientfollowup12.KOMPLIKASJONER_TROMBOSE_LUNGE AS KpLE12mnd,
    patientfollowup12.KOMPLIKASJONER_INFEKSJON_OVERFLATE AS KpInfOverfla12mnd,
    patientfollowup12.KOMPLIKASJONER_INFEKSJON_DYP AS KpInfDyp12mnd,
    patientfollowup12.KOMPLIKASJONER_ETTER_OPERASJON AS NySykdSkade12mnd,
    patientfollowup12.KOMPLIKASJONER_LEDDSMERTE AS NyLeddSm12mnd,
    patientfollowup12.KOMPLIKASJONER_KREFTSYKDOM AS Nykreft12mnd,
    patientfollowup12.KOMPLIKASJONER_HJERTEKAR AS NyHjerteKar12mnd,
    patientfollowup12.KOMPLIKASJONER_ANNEN_NERVESYKDOM AS NyNerveSkyd12mnd,
    patientfollowup12.KOMPLIKASJONER_SKADE_FOLGETILSTAND AS NySkade12mnd,
    patientfollowup12.KOMPLIKASJONER_ANNEN_VESENTLIG_SYKDOM AS NyAnnen12mnd,
    patientfollowup12.ODI_SMERTE AS Oswsmerte12mnd,
    patientfollowup12.ODI_PERSONLIG_STELL AS Oswstel12mnd,
    patientfollowup12.ODI_LOFTING AS Oswloft12mnd,
    patientfollowup12.ODI_GAAING AS Oswgaa12mnd,
    patientfollowup12.ODI_SITTING AS Oswsitt12mnd,
    patientfollowup12.ODI_STAAING AS Oswstaa12mnd,
    patientfollowup12.ODI_SOVING AS Oswsove12mnd,
    patientfollowup12.ODI_SEKSUALLIV AS Oswsex12mnd,
    patientfollowup12.ODI_SOSIALT AS Oswsosi12mnd,
    patientfollowup12.ODI_REISING AS Oswreis12mnd,
    patientfollowup12.ODI_SCORE AS OswTot12mnd,
    patientfollowup12.EQ5D_GANGE AS EqgangeV312mnd,
    patientfollowup12.EQ5D_PERSONLIG_STELL AS EqperstV312mnd,
    patientfollowup12.EQ5D_VANLIGE_GJOREMAL AS EqvanlgjV312mnd,
    patientfollowup12.EQ5D_SMERTE_UBEHAG AS EqsmerteV312mnd,
    patientfollowup12.EQ5D_ANGST_DEPRESJON AS EqangstV312mnd,
    patientfollowup12.EQ5D_SCORE AS EQ5DV312mnd,
    patientfollowup12.HELSETILSTAND_IDAG AS Helsetilst12mnd,
    patientfollowup12.SMERTESTILLENDE AS SmSti12mnd,
    patientfollowup12.SMERTESTILLENDE_BRUK AS SmHypp12mnd,
    patientfollowup12.ARBEIDSSTATUS AS Arbstatus12mndV3,
    patientfollowup12.SYKEMELDT_PROSENT AS SykemeldPros12mnd,
    patientfollowup12.UFORETRYGDET_PROSENT AS UforPst12mnd,
    patientfollowup12.TILBAKETILJOBB AS JobbArbGiver12mnd,
    patientfollowup12.SOKT_UFOR AS Uforetrygd12mnd,
    patientfollowup12.SOKT_ERSTATNING AS Erstatning12mnd,
    patientfollowup12.REOPERERT_ETTER_OPERASJON AS NyRyggOpr12mnd,
    patientfollowup12.REOPERERT_ANTALL AS NyOprAnt12mnd,
    patientfollowup12.REOPERERT_OMRAADE AS NyOprNivaa12mnd,
    patientfollowup12.STATUS AS Status12mnd,
    patientfollowup12.FIRST_TIME_CLOSED_BY AS ForstLukketAV12mnd,
    patientfollowup12.FIRST_TIME_CLOSED AS ForstLukket12mnd

FROM mce mce
         INNER JOIN patient patient ON mce.PATIENT_ID = patient.ID
         INNER JOIN patientform patientform ON mce.MCEID = patientform.MCEID
         INNER JOIN surgeonform surgeonform ON mce.MCEID = surgeonform.MCEID
         INNER JOIN patientfollowup patientfollowup ON mce.MCEID = patientfollowup.MCEID  AND patientfollowup.CONTROL_TYPE=3
         INNER JOIN patientfollowup patientfollowup12 ON mce.MCEID = patientfollowup12.MCEID  AND patientfollowup12.CONTROL_TYPE=12
WHERE (patientform.STATUS = 1 OR
       (patientform.STATUS <> 1 AND (patientfollowup.STATUS = 1 OR patientfollowup12.STATUS = 1)))
  AND (surgeonform.STATUS = 1 OR
       (surgeonform.STATUS <> 1 AND (patientfollowup.STATUS = 1 OR patientfollowup12.STATUS = 1)))
;

-- fritekstvar
CREATE VIEW fritekstvar AS
SELECT
    mce.PATIENT_ID AS PasientID,
    mce.CENTREID AS AvdRESH,
    getFriendlyName(mce.CENTREID) AS SykehusNavn,
    mce.MCEID AS ForlopsID,
    patientform.USERCOMMENT AS patientform_USERCOMMENT,
    surgeonform.POSTTROMBOSE_SPESIFISER as surgeonform_POSTTROMBOSE_SPESIFISER,
    surgeonform.ANDRESYKDOMSKADERPLAGE_SPESIFISER as surgeonform_ANDRESYKDOMSKADERPLAGE_SPESIFISER,
    surgeonform.OPERASJONSMETODE_OTHER_METHOD_ANNET_SPESIFISER as surgeonform_OPERASJONSMETODE_OTHER_METHOD_ANNET_SPESIFISER,
    surgeonform.NIVAASIDE_ANNET_SPESIFISER as surgeonform_NIVAASIDE_ANNET_SPESIFISER,
    surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANNET_SPESIFISER as surgeonform_PEROPERATIVE_KOMPLIKASJONER_ANNET_SPESIFISER,
    surgeonform.USERCOMMENT AS surgeonform_USERCOMMENT
FROM
    mce mce INNER JOIN patient patient ON mce.PATIENT_ID = patient.ID
            INNER  JOIN patientform patientform ON mce.MCEID = patientform.MCEID and patientform.STATUS = 1
            INNER  JOIN surgeonform surgeonform ON mce.MCEID = surgeonform.MCEID and surgeonform.STATUS = 1
;

-- View for sensitiveopplysninger/koblingstabell
select 'Creating view koblingstabell' as 'action';
CREATE VIEW koblingstabell AS
SELECT ID, SSN

FROM patient;

CREATE VIEW pasientliste AS
SELECT
    ID AS PasientID,
    REGISTERED_DATE AS RegistreringsDato,
    BIRTH_DATE AS Fodselsdato,
    getListText('PATIENT_GENDER', GENDER) AS Kjonn,
    getListText('PATIENT_DECEASED', DECEASED) AS DodPasient,
    DECEASED_DATE AS Dodsdato,
    MUNICIPALITY_NUMBER AS KommuneNr,
    MUNICIPALITY_NAME AS Kommune,
    COUNTY AS Fylke
FROM patient;

CREATE VIEW patientlist AS
SELECT
    ID,
    REGISTERED_DATE,
    BIRTH_DATE ,
    GENDER,
    DECEASED,
    DECEASED_DATE,
    MUNICIPALITY_NUMBER,
    MUNICIPALITY_NAME,
    COUNTY
FROM patient;

-- View for adgangsbegrenset Datadump dropdown in view
DROP VIEW IF EXISTS tablelist_restricted;

CREATE VIEW tablelist_restricted AS
SELECT DISTINCT TABLE_NAME AS TABLE_NAME
FROM information_schema.tables
WHERE TABLE_TYPE IN ('BASE TABLE','VIEW')
  AND TABLE_NAME IN ('')
ORDER BY TABLE_NAME;

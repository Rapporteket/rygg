#' Hente data fra V2
#'
#' @return Dataramme med alle data fra V2
#' @export

hentDataV2 <- function(){
  V2oper <- rapbase::loadRegData(registryName = 'data',
                                 query='SELECT * FROM ryggv2_operation')
  V2pas <- rapbase::loadRegData(registryName = 'data',
                                query='SELECT * FROM ryggv2_patient_preop')
  V2oppf <- rapbase::loadRegData(registryName = 'data',
                                 query='SELECT * FROM ryggv2_followup')

  V2_operpas <- merge(V2oper, V2pas[-which(names(V2pas)=='OLD_PID')], by = 'MCEID')
  V2 <- merge(V2_operpas, V2oppf[-which(names(V2oppf)=='OLD_PID')], by = 'MCEID')
  MCEtab <- rapbase::loadRegData(registryName = 'data',
                                 query='SELECT * FROM mce
                                 WHERE MCETYPE = 9 ')
  dodsdato <- rapbase::loadRegData(registryName = 'data',
                                   query='SELECT DECEASED_DATE as DodsDato,
                                   DECEASED as DodPasient,
                                   ID as PATIENT_ID FROM patient')
  RegDataV2 <- merge(V2, MCEtab[,c("MCEID", "PATIENT_ID", "MCETYPE")], by = 'MCEID' )
  RegDataV2 <- merge(RegDataV2, dodsdato, by = 'PATIENT_ID')
}



#' Endre variabelnavn/kolonnenavn til selvvalgte navn
#' @param tabell datatabellnavn i databasen
#' @param tabType REGISTRATION_TYPE
#' @return tabell med selvvalgte variabelnavn spesifisert i friendlyvar. Intern funksjon
#'
#' @export

mappingEgneNavn <- function(tabell, tabType) {

  friendlyVarTab  <-
    rapbase::loadRegData( "data",
                          query = "SELECT FIELD_NAME, REGISTRATION_TYPE, USER_SUGGESTION
                           FROM friendly_vars") #

  indTabType <- which(friendlyVarTab$REGISTRATION_TYPE %in% tabType)
  if (!length(indTabType)==0) {
    friendlyVarTabType <- friendlyVarTab[indTabType,]
    kuttTabPrefiks <- if (tabType == 'PATIENTFOLLOWUP12') {'PATIENTFOLLOWUP_'} else {paste0(tabType, '_')}

    rydd <- which(friendlyVarTabType$USER_SUGGESTION %in% c('VERBOTEN', 'NEINNICHTS'))

    #Fjerner variabler merket 'VERBOTEN' eller NEINNICHTS
    if (length(rydd)>0) {
      fjernvar <- gsub(kuttTabPrefiks, "", friendlyVarTabType$FIELD_NAME[rydd])
      indFjern <- which(names(tabell) %in% fjernvar)
      if (length(indFjern) > 0) {
        tabell <- tabell[ , -indFjern]}
      friendlyVarTabType <- friendlyVarTabType[-rydd, ]
    }

    navn <- gsub(kuttTabPrefiks, "", friendlyVarTabType$FIELD_NAME)
    names(navn) <- friendlyVarTabType$USER_SUGGESTION
    tabell <- dplyr::rename(tabell, dplyr::any_of(navn)) #all_of(navn
  }
  return(tabell)
}


# LEGG INN FJERNING AV VARIABLER SOM GJENTAS I FLERE TABELLER. f.EKS. ReshId (CENTREID)
# Alle variabler, Bare utvalgte var, Bare selvvalgte navn ?

#' Hent datatabell fra ngers database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
#' @param egneVarNavn 0 - Qreg-navn benyttes.
#'                    1 - selvvalgte navn fra Friendlyvar benyttes
#' Bare ferdigstilte (status=1) legeskjema og pasientskjema overføres
#'
#' @export

hentDataTabellV3 <- function(tabellnavn = "surgeonform",
                           qVar = '*',
                           datoFra = '2019-01-01',
                           datoTil = Sys.Date(),
                           egneVarNavn = 1) { #  status = 1

  tabType <- toupper(tabellnavn)
  query <- paste0("SELECT ", qVar, " FROM ", tabellnavn)

  if (tabellnavn == 'surgeonform'){
    query <- paste0(query,
                    ' WHERE OPERASJONSDATO >= \'', datoFra,
                    '\' AND OPERASJONSDATO <= \'', datoTil, '\' ')
  }


  if (tabellnavn == 'patientfollowup3') {
    query <- paste0("SELECT ", qVar, ' FROM patientfollowup
                    WHERE CONTROL_TYPE = 3')
    tabType <- 'PATIENTFOLLOWUP'
  }

  if (tabellnavn == 'patientfollowup12') {
    query <- paste0("SELECT ", qVar, ' FROM patientfollowup
                              WHERE CONTROL_TYPE = 12')}

  tabell <- rapbase::loadRegData(registryName = "data",
                                 query = query)

  if (egneVarNavn == 1) {
    tabell <- mappingEgneNavn(tabell, tabType)}

  return(tabell)
}

#' Henter Rygg-tabeller og kobler sammen
#'
#' @param medPROM: koble på RAND og TSS2-variabler
#' @param alleData 1- alle variabler med, 0 - utvalgte variabler med
#'
#' @return RegData data frame
#'
#' @export


hentRegDataV3 <- function(datoFra = '2019-01-01', datoTil = Sys.Date(),
                             medOppf = 1,  ...) {
  # Få til å fungere med ny sammenkobling av alle data
  # legg på valg av variabler?
  # legg på datofiltrering

  #mce Trenger nok ganske få av disse variablene
  # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
  qmce <- 'CENTREID AS ReshId, CREATEDBY, MCEID, PATIENT_ID AS PasientID'

  mceSkjema <- hentDataTabellV3(tabellnavn = "mce",
                              qVar = qmce,
                              egneVarNavn = 0) #Ingen selvvalgte navn

  #Pasientskjema:
  qPas <- 'BIRTH_DATE as DatoFodt,
             DECEASED,
             DECEASED_DATE,
             GENDER,
             ID,
             REGISTERED_DATE'

  PasInfoSkjema <- hentDataTabellV3(tabellnavn = "patient",
                                  qVar = qPas,
                                  egneVarNavn = 1)

  varFjernes <- c('TSCREATED', 'TSUPDATED', 'FIRST_TIME_CLOSED_BY', 'FIRST_TIME_CLOSED',
                  'CENTREID', 'TYPE_UNDERSOEKELSE_UTFYLT', 'CREATED_BY', 'CREATEDBY',
                  'UPDATEDBY')

  #Legeskjema
  LegeSkjema <- hentDataTabellV3(tabellnavn = "surgeonform",
                               qVar = '*',
                               datoFra = datoFra, datoTil = datoTil,
                               egneVarNavn = 1)
  LegeSkjema <- dplyr::rename(LegeSkjema,
                              'ForstLukketLege' = 'FIRST_TIME_CLOSED',
                              'UtfyltDatoLege' = 'TSCREATED')
  LegeSkjema <- LegeSkjema[ ,-which(names(LegeSkjema) %in% varFjernes)]

  #Pasientens spørreskjema
  PasSkjema <- hentDataTabellV3(tabellnavn = "patientform",
                              qVar = '*',
                              egneVarNavn = 1)
  PasSkjema <- PasSkjema[ ,-which(names(PasSkjema) %in% varFjernes)]

  #Sykehusnavn
  EnhetsNavn <- hentDataTabellV3(tabellnavn = "centreattribute",
                               qVar = 'ID, ATTRIBUTEVALUE as SykehusNavn')

  # SAMMENSTILL SKJEMA:
  RegData <-
    merge(mceSkjema,
          PasInfoSkjema, by = "PasientID",
          suffixes = c("", "_pas"), all = F) |>
    merge(LegeSkjema, by = "MCEID", all = F, suffixes = c("", "_lege")) |>
    merge(PasSkjema,
          by = "MCEID", all.x = TRUE, suffixes = c("", "_oppf0")) |>
    merge(EnhetsNavn,
          by.x = "ReshId", by.y = 'ID', all.x = TRUE)





  if (medOppf == 1) {
    #Oppfølging, 3 mnd
    Oppf3Skjema <- hentDataTabellV3(tabellnavn = "patientfollowup3",
                                  qVar = '*',
                                  egneVarNavn = 1)
    #Oppfølging, 12 mnd
    Oppf12Skjema <- hentDataTabellV3(tabellnavn = "patientfollowup12",
                                   qVar = '*',
                                   egneVarNavn = 1)

    # SAMMENSTILL SKJEMA:
    RegData <- RegData |>
      merge(Oppf3Skjema,
            suffixes = c("", "_oppf3"), by = "MCEID", all.x = TRUE) |>
      merge(Oppf12Skjema,
            suffixes = c("", "_oppf12"), by = "MCEID", all.x = TRUE)

 # --------------Justere statusvariabler
    ePROMadmTab <- rapbase::loadRegData(registryName = 'data',
                                        query='SELECT * FROM proms')
    ePROMvar <- c("MCEID", "TSSENDT", "TSRECEIVED", "NOTIFICATION_CHANNEL", "DISTRIBUTION_RULE",
                  'REGISTRATION_TYPE')
    # «EpromStatus» er definert av HNIKT, og den som er viktigst med tanke på svarprosent.
    # Verdien 3 betyr at pasienten har besvart.
    # OBS at den skiller seg litt fra tilsvarende variabel i MRS som er definert slik:
    # 0 = Created, 1 = Ordered, 2 = Expired, 3 = Completed, 4 = Failed
    ind3mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                       c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'))

    ind12mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                        c('PATIENTFOLLOWUP12', 'PATIENTFOLLOWUP_12_PiPP', 'PATIENTFOLLOWUP_12_PiPP_REMINDER'))

    ePROM3mnd <- ePROMadmTab[intersect(ind3mnd, which(ePROMadmTab$STATUS==3)), ePROMvar] #STATUS==3 completed
    names(ePROM3mnd) <- paste0(ePROMvar, '3mnd')
    ePROM12mnd <- ePROMadmTab[intersect(ind12mnd, which(ePROMadmTab$STATUS==3)), ePROMvar]
    names(ePROM12mnd) <- paste0(ePROMvar, '12mnd')

    indIkkeEprom3mnd <-  which(!(RegData$MCEID %in% ePROMadmTab$MCEID[ind3mnd]))
    indIkkeEprom12mnd <-  which(!(RegData$MCEID %in% ePROMadmTab$MCEID[ind12mnd]))
    RegData$Ferdig1b3mndGML <- RegData$Status3mnd
    RegData$Status3mnd <- 0
    RegData$Status3mnd[RegData$MCEID %in% ePROM3mnd$MCEID] <- 1
    RegData$Status3mnd[intersect(which(RegData$Ferdig1b3mndGML ==1), indIkkeEprom3mnd)] <- 1

    RegData$Status12mndGML <- RegData$Status12mnd
    RegData$Status12mnd <- 0
    RegData$Status12mnd[RegData$MCEID %in% ePROM12mnd$MCEID] <- 1
    RegData$Status12mnd[intersect(which(RegData$Status12mndGML ==1), indIkkeEprom12mnd)] <- 1
     }

  #Evt flytt dette til skjemaet det hører hjemme...
  fjernes <- c(varFjernes, "Bydelskode",	"Bydelsnavn","Fylke", "HelseRegion",
               'MceType', 'DodsDato', 'KommuneNr',	'KommuneNavn',
               'REGIONAL_HEALTH_AUTHORITY')

  RegData <- RegData[ ,-c(grep('_MISS', names(RegData)), which(names(RegData) %in% fjernes))]

  return(invisible(RegData))
}

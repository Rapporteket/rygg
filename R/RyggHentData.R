#' Hente data fra V2
#'
#' @return Dataramme med alle data fra V2
#' @export

hentDataV2 <- function(){

  dbList <- rapbase::rapOpenDbConnection("rygg", "mysql")
  dbconn <- dbList$con
  on.exit(
    if (DBI::dbIsValid(dbconn)) {
      rapbase::rapCloseDbConnection(dbconn)
    },
    add = TRUE
  )

  V2oper <- DBI::dbGetQuery(conn = dbconn, statement='SELECT * FROM ryggv2_operation')
  #rapbase::loadRegData(registryName = 'data', query=)
  V2pas <- DBI::dbGetQuery(conn = dbconn, statement='SELECT * FROM ryggv2_patient_preop')
  #rapbase::loadRegData(registryName = 'data',query=)
  V2oppf <- DBI::dbGetQuery(conn = dbconn, statement='SELECT * FROM ryggv2_followup')

  V2_operpas <- merge(V2oper, V2pas[-which(names(V2pas)=='OLD_PID')], by = 'MCEID')
  V2 <- merge(V2_operpas, V2oppf[-which(names(V2oppf)=='OLD_PID')], by = 'MCEID')
  V2 <- V2[,-which(names(V2) %in% c('OLD_PID'))]

  MCEtab <- DBI::dbGetQuery(conn = dbconn, statement='SELECT * FROM mce
                                 WHERE MCETYPE = 9 ')
  dodsdato <-DBI::dbGetQuery(conn = dbconn,
                             statement='SELECT DECEASED_DATE as DodsDato,
                                   DECEASED as DodPasient,
                                   ID as PATIENT_ID FROM patient')
  rapbase::rapCloseDbConnection(dbconn)
  dbList <- NULL

  RegDataV2 <- merge(V2, MCEtab[,c("MCEID", "PATIENT_ID", "MCETYPE")], by = 'MCEID' )
  RegDataV2 <- merge(RegDataV2, dodsdato, by = 'PATIENT_ID')

}



#' Endre variabelnavn/kolonnenavn til selvvalgte navn
#' @param tabell datatabellnavn i databasen
#' @param tabType REGISTRATION_TYPE
#' @param dbconn databasekobling
#' @return tabell med selvvalgte variabelnavn spesifisert i friendlyvar. Intern funksjon
#'
#' @export

mappingEgneNavn <- function(tabell, tabType, dbconn = NULL) {
  if (is.null(dbconn)) {
    dbconn <- rapbase::rapOpenDbConnection("rygg", "mysql")$con
    on.exit(rapbase::rapCloseDbConnection(dbconn), add = TRUE)
  }
  friendlyVarTab  <-
    DBI::dbGetQuery(conn = dbconn,
                     statement = "SELECT FIELD_NAME, REGISTRATION_TYPE, USER_SUGGESTION
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
                              egneVarNavn = 1,
                              dbconn = NULL) { # status = 1
  if (is.null(dbconn)) {
    dbconn <- rapbase::rapOpenDbConnection("rygg", "mysql")$con
    on.exit(rapbase::rapCloseDbConnection(dbconn), add = TRUE)
  }

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

  tabell <- # rapbase::loadRegData(registryName = "data", query = query)
    DBI::dbGetQuery(conn = dbconn, statement = query)


  if (egneVarNavn == 1) {
    tabell <- mappingEgneNavn(tabell, tabType, dbconn = dbconn)}

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


  dbList <- rapbase::rapOpenDbConnection("rygg", "mysql")
  dbconn <- dbList$con
  on.exit(rapbase::rapCloseDbConnection(dbconn), add = TRUE)

  # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
  qmce <- 'CENTREID AS ReshId, MCEID, PATIENT_ID AS PasientID'

  mceSkjema <- hentDataTabellV3(tabellnavn = "mce",
                                dbconn = dbconn, #dbList$con,
                                qVar = qmce,
                                egneVarNavn = 0) #Ingen selvvalgte navn

  #Pasientskjema:
  qPas <- 'BIRTH_DATE as DatoFodt,
             DECEASED,
             DECEASED_DATE,
             GENDER,
             ID'

  PasInfoSkjema <- hentDataTabellV3(tabellnavn = "patient",
                                    dbconn = dbconn,
                                    qVar = qPas,
                                    egneVarNavn = 1)

  varFjernes <- c('TSCREATED', 'TSUPDATED', 'FIRST_TIME_CLOSED_BY', 'FIRST_TIME_CLOSED',
                  'CENTREID', 'TYPE_UNDERSOEKELSE_UTFYLT', 'CREATED_BY', 'CREATEDBY',
                  'UPDATEDBY')

  #Legeskjema
  LegeSkjema <- hentDataTabellV3(tabellnavn = "surgeonform",
                                 dbconn = dbconn,
                                 qVar = '*',
                                 datoFra = datoFra, datoTil = datoTil,
                                 egneVarNavn = 1)
  LegeSkjema <- dplyr::rename(LegeSkjema,
                              'ForstLukketLege' = 'FIRST_TIME_CLOSED',
                              'UtfyltDatoLege' = 'TSCREATED')
  LegeSkjema <- LegeSkjema[ ,-which(names(LegeSkjema) %in% varFjernes)]

  #Pasientens spørreskjema
  PasSkjema <- hentDataTabellV3(tabellnavn = "patientform",
                                dbconn = dbconn,
                                qVar = '*',
                                egneVarNavn = 1)
  PasSkjema <- PasSkjema[ ,-which(names(PasSkjema) %in% varFjernes)]

  #Sykehusnavn
  EnhetsNavn <- hentDataTabellV3(tabellnavn = "centreattribute",
                                 dbconn = dbconn,
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
    varFjernes <- c(varFjernes, 'CONTROL_TYPE', 'CREATEDBY', 'FOLLOWUP_TYPE',
                    'FORM_COMPLETED_VIA_PROMS', 'HELSETILSTAND_SCALE', 'ID',
                    'PROM_ANSWERED', 'STATUS_CONTROL', 'UPDATEDBY',
                    'KOMPLIKASJONER_ANNEN_VESENTLIG_SYKDOM_SPESIFISER')
    #Oppfølging, 3 mnd
    Oppf3Skjema <- hentDataTabellV3(tabellnavn = "patientfollowup3",
                                    dbconn = dbconn,
                                    qVar = '*',
                                    egneVarNavn = 1)
    Oppf3Skjema <- Oppf3Skjema[ ,-which(names(Oppf3Skjema) %in% varFjernes)]

    #Oppfølging, 12 mnd
    Oppf12Skjema <- hentDataTabellV3(tabellnavn = "patientfollowup12",
                                     dbconn = dbconn,
                                     qVar = '*',
                                     egneVarNavn = 1)
    Oppf12Skjema <- Oppf12Skjema[ ,-which(names(Oppf12Skjema) %in% varFjernes)]

    # SAMMENSTILL SKJEMA:
    RegData <- RegData |>
      merge(Oppf3Skjema,
            suffixes = c("", "_oppf3"), by = "MCEID", all.x = TRUE) |>
      merge(Oppf12Skjema,
            suffixes = c("", "_oppf12"), by = "MCEID", all.x = TRUE)

    # --------------Justere statusvariabler
    ePROMadmTab <- DBI::dbGetQuery(conn = dbconn, statement='SELECT * FROM proms')
    #rapbase::loadRegData(registryName = 'data', query='SELECT * FROM proms')
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
               'MceType', 'KommuneNr',	'KommuneNavn', 'REGIONAL_HEALTH_AUTHORITY')

  RegData <- RegData[ ,-c(grep('_MISS', names(RegData)), which(names(RegData) %in% fjernes))]

  return(invisible(RegData))
}



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
#' @param datoFra Benyttes kun til å avgjøre om kobling til V2 skal utføres.
#' @param datoTil P.t ikke i bruk
#'
#' @return RegData, dataramme med data f.o.m. 2007.
#' @export

RyggRegDataV2V3 <- function(datoFra = '2007-01-01') {
  #, datoTil = '2099-01-01', alleVarV3=1 ){ #alleVarV2=0
  #NB: datovalg benyttes foreløpig kun til å avgjøre om kobling til V2 skal utføres.

  message('Henter data, RyggRegDataV2V3')
  kunV3 <- ifelse(datoFra >= '2019-11-01' & !is.na(datoFra), 1, 0)

  if (kunV3 == 0) {
    RegDataV2 <- hentDataV2()

    RegDataV2 <- tilpassV2data(RegDataV2=RegDataV2)
  }

  RegDataV3 <- hentRegDataV3(datoFra = datoFra, datoTil = Sys.Date(),
                             medOppf = 1)
  RegDataV3 <- tilpassV3data(RegDataV3 = RegDataV3)

  if (kunV3 == 0){
    RegDataV3$RokerV2 <- dplyr::replace_values(RegDataV3$RokerV3, from = 2, to = 0)

    VarV2 <- names(RegDataV2) #sort
    VarV3 <- names(RegDataV3) #sort

    V2ogV3 <- intersect(VarV2, VarV3)
    V3ikkeV2 <- setdiff(VarV3, V2ogV3)
    V2ikkeV3 <- setdiff(VarV2, V2ogV3)
    # if (alleVarV2 == 0){
    #   RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
    #   RegDataV2V3 <- rbind(RegDataV2[ ,VarV3],
    #                        RegDataV3[ ,VarV3])
    # } else {
    RegDataV2[, V3ikkeV2] <- NA #Fungerer ikke for datoTid-variabler
    RegDataV3[, V2ikkeV3] <- NA
    RegDataV2V3 <- rbind(RegDataV2,
                         RegDataV3)
    # }
  }

  if (kunV3 == 1) {RegDataV2V3 <- RegDataV3}
  #Avvik? PeropKompAnnet
  #ProsKode1 ProsKode2 - Kode i V2, kode + navn i V3


  #En desimal
  RegDataV2V3$BMI <- round(RegDataV2V3$BMI,1)
  RegDataV2V3$OswTotPre <- round(RegDataV2V3$OswTotPre,1)
  RegDataV2V3$OswTot3mnd <- round(RegDataV2V3$OswTot3mnd,1)
  RegDataV2V3$OswTot12mnd <- round(RegDataV2V3$OswTot12mnd,1)

  message('Ferdig med RegDataV2V3')
  return(RegDataV2V3)
}




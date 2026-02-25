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
    navnFr <- friendlyVarTab$FIELD_NAME[indTabType]
    kuttTabPrefiks <- if (tabType == 'PATIENTFOLLOWUP12') {'PATIENTFOLLOWUP_'} else {paste0(tabType, '_')}
    navn <- gsub(kuttTabPrefiks, "", navnFr)

    rydd <- which(friendlyVarTab$USER_SUGGESTION[indTabType] == 'VERBOTEN')

    #Fjerner variabler merket 'VERBOTEN'
    if (length(rydd)>0) {
      fjernvar <- gsub(kuttTabPrefiks, "", friendlyVarTab$FIELD_NAME[rydd])
      indFjern <- which(names(tabell) %in% fjernvar)
      if (indFjern > 0) {
        tabell <- tabell[ , -indFjern]}
      friendlyVarTab <- friendlyVarTab[-rydd, ]
    }

    names(navn) <- friendlyVarTab$USER_SUGGESTION[indTabType]
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

hentDataTabell <- function(tabellnavn = "surgeonform",
                           qVar = '*',
                           egneVarNavn = 1) { #  status = 1

  tabType <- toupper(tabellnavn)
  query <- paste0("SELECT ", qVar, " FROM ", tabellnavn)

  if (tabellnavn == 'patientfollowup3') {
    query <- paste0("SELECT ", qVar, ' FROM patientfollowup
                    WHERE CONTROL_TYPE = 3')
    tabType <- 'PATIENTFOLLOWUP'
  }

  if (tabellnavn == 'patientfollowup12') {
    query <- query <- paste0("SELECT ", qVar, ' FROM patientfollowup
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


RyggHentRegDataV3 <- function(datoFra = '2019-01-01', datoTil = Sys.Date(),
                             medOppf = 1,  ...) {
  # Få til å fungere med ny sammenkobling av alle data
  # legg på valg av variabler?
  # legg på datofiltrering

  #mce Trenger nok ganske få av disse variablene
  # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
  qmce <- 'CENTREID AS ReshId, CREATEDBY, MCEID, PATIENT_ID,
             sendtSMS12mnd, sendtSMS3mnd, TSCREATED, TSUPDATED'

  mceSkjema <- hentDataTabell(tabellnavn = "mce",
                              qVar = qmce,
                              egneVarNavn = 0) #Ingen selvvalgte navn

  #Pasientskjema:
  qPas <- 'BIRTH_DATE,
             DECEASED,
             DECEASED_DATE,
             GENDER,
             ID,
             OWNING_CENTRE,
             REAPER_DATE,
             REGISTERED_DATE,
             TSCREATED,
             TSUPDATED'

  PasInfoSkjema <- hentDataTabell(tabellnavn = "patient",
                                  qVar = qPas,
                                  egneVarNavn = 0)
  #Legeskjema
  LegeSkjema <- hentDataTabell(tabellnavn = "surgeonform",
                               qVar = '*',
                               egneVarNavn = 1)

  #Pasientens spørreskjema
  PasSkjema <- hentDataTabell(tabellnavn = "patientform",
                              qVar = '*',
                              egneVarNavn = 1)

  #Sykehusnavn
  EnhetsNavn <- hentDataTabell(tabellnavn = "centreattribute",
                               qVar = 'ID, ATTRIBUTEVALUE as SykehusNavn')

  # SAMMENSTILL SKJEMA:
  RegData <-
    merge(mceSkjema,
          PasInfoSkjema, by.x = "PATIENT_ID", by.y = "ID",
          suffixes = c("", "_pas"), all = F) |>
    merge(LegeSkjema, by = "MCEID", all = F, suffixes = c("", "_lege")) |>
    merge(PasSkjema,
          by = "MCEID", all.x = TRUE, suffixes = c("", "_oppf0")) |>
    merge(EnhetsNavn,
          by.x = "ReshId", by.y = 'ID', all.x = TRUE)


  if (medOppf == 1) {
    #Oppfølging, 3 mnd
    Oppf3Skjema <- hentDataTabell(tabellnavn = "patientfollowup3",
                                  qVar = '*',
                                  egneVarNavn = 1)
    #Oppfølging, 12 mnd
    Oppf12Skjema <- hentDataTabell(tabellnavn = "patientfollowup12",
                                   qVar = '*',
                                   egneVarNavn = 1)

    # SAMMENSTILL SKJEMA:
    RegData <- RegData |>
      merge(Oppf3Skjema,
            suffixes = c("", "_oppf3"), by = "MCEID", all.x = TRUE) |>
      merge(Oppf12Skjema,
            suffixes = c("", "_oppf12"), by = "MCEID", all.x = TRUE)

     }

  return(invisible(RegData))
}

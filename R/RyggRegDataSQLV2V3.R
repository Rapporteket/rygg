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

RyggRegDataSQLV2V3 <- function(datoFra = '2007-01-01', #datoTil = '2099-01-01',
                               alleVarV3=1, alleVarV2=0){
#NB: datovalg benyttes foreløpig kun til å avgjøre om kobling til V2 skal utføres.

  message('Henter data, RyggRegDataSQLV2V3')
  kunV3 <- ifelse(datoFra >= '2019-01-01' & !is.na(datoFra), 1, 0)

  if (kunV3 == 0) {
    RegDataV2 <- hentDataV2()
    tilpassV2data(RegDataV2=RegDataV2)
    }

  RegDataV3 <- hentRegDataV3(datoFra = '2019-01-01', datoTil = Sys.Date(),
                             medOppf = 1)
  # RegDataV3AVN <- rapbase::loadRegData(registryName = 'data',
  #                                    query='SELECT * FROM allevarnum')
  # RegDataV3Forl <- rapbase::loadRegData(
  #   registryName = 'data',
  #   query='SELECT ForlopsID, Kommune, Kommunenr, Fylkenr,     #Fylke er med i AVN
  #                DodsDato, BasisRegStatus, KryptertFnr FROM forlopsoversikt')
  #
  # RegDataV3 <- merge(RegDataV3AVN,
  #                    RegDataV3Forl, by='ForlopsID',
  #                    all.x = TRUE, all.y = FALSE)



    #I perioden 2019-21 ble ikke dyp og overfladisk sårinfeksjon registrert.
    indIkkeSaarInf <- which(RegDataV3$OpDato >= '2019-01-01' & RegDataV3$OpDato <= '2021-12-31')
    RegDataV3$KpInfOverfla3mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfOverfla12mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp3mnd[indIkkeSaarInf] <- NA
    RegDataV3$KpInfDyp12mnd[indIkkeSaarInf] <- NA



    #-----Tilrettelegg V3-data-------------------------
#Fjerner ikke-ferdigstilte pasientskjema

  #  RegDataV3 <- RegDataV3[RegDataV3$StatusPasSkjema==1 & RegDataV3$StatusLegeSkjema==1, ]
  RegDataV3$Versjon <- 'V3'

  #Ønsker tom for manglende
  RegDataV3$SmBePre[RegDataV3$SmBePre == 99] <- NA #99: Ikke utfylt i V3, NA i V2
  RegDataV3$SmRyPre[RegDataV3$SmRyPre == 99] <- NA #99: Ikke utfylt i V3, NA i V2

  #Ny ARBEIDSSTATUS-variabel, basert på V2 og V3:
  #Arbstatus3mndV3 - de med verdi 11 eller 99 eller tom som har oppgitt sykemeldingsgrad.,
  # Skal ha verdien 7.
  RegDataV3$Arbstatus3mndV3[which(RegDataV3$Arbstatus3mndV3 %in% c(11,99))] <- NA
  ind7_3mnd <- which(is.na(RegDataV3$Arbstatus3mndV3) & RegDataV3$SykemeldPros3mnd>0) #17 per 1.mars 2024
  RegDataV3$Arbstatus3mndV3[ind7_3mnd] <- 7

  RegDataV3$Arbstatus12mndV3[which(RegDataV3$Arbstatus12mndV3 %in% c(10,99))] <- NA
  ind7_12mnd <- which( is.na(RegDataV3$Arbstatus12mndV3) & RegDataV3$SykemeldPros12mnd>0) #6 per 1.mars 2024
  RegDataV3$Arbstatus12mndV3[ind7_12mnd] <- 7

  # 1: I arbeid - V3- 1+2
  RegDataV3$ArbstatusPreV2V3 <- plyr::mapvalues(RegDataV3$ArbstatusPreV3, from = c(2, 99), to = c(1, NA))
  RegDataV3$Arbstatus3mndV2V3 <- plyr::mapvalues(RegDataV3$Arbstatus3mndV3, from = 2, to = 1)
  RegDataV3$Arbstatus12mndV2V3 <- plyr::mapvalues(RegDataV3$Arbstatus12mndV3, from = 2, to = 1)

  #Legge til underkategori for hovedkategori.
  #ny <- rygg::kategoriserInngrep(RegData=RegDataV3)
  RegDataV3 <- kategoriserInngrep(RegData=RegDataV3)$RegData

  #Definasjon av diagnosegrupper prolaps og spinal stenose
  RegDataV3 <- defProSS(RegDataV3)

  RegDataV3$Kp3Mnd <- NULL
  RegDataV3$Kp3Mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3mnd','KpInfDyp3mnd', 'KpUVI3mnd',
                                         'KpLungebet3mnd', 'KpBlod3mnd','KpDVT3mnd','KpLE3mnd')],
                           na.rm = T) > 0] <- 1
  RegDataV3$KpInf3Mnd <- NULL
  RegDataV3$KpInf3Mnd[rowSums(RegDataV3[ ,c('KpInfOverfla3mnd','KpInfDyp3mnd')], na.rm = T) > 0] <- 1


  #TidlOp. V2: 1:4,9 c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
  #TidlIkkeOp, TidlOpAnnetNiv, TidlOpsammeNiv
  RegDataV3$TidlOpr <- 9
  RegDataV3$TidlOpr[RegDataV3$TidlIkkeOp==1] <- 4
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1] <- 1
  RegDataV3$TidlOpr[RegDataV3$TidlOpAnnetNiv==1] <- 2
  RegDataV3$TidlOpr[RegDataV3$TidlOpsammeNiv==1 & RegDataV3$TidlOpAnnetNiv==1] <- 3

  RegDataV3$OpMikro <- plyr::mapvalues(RegDataV3$OpMikroV3, from = c(0,1,2,3,9), to = c(0,1,1,1,0))
  RegDataV3$OpAndreEndosk <- plyr::mapvalues(RegDataV3$OpMikroV3, from = c(0,1,2,3,9), to = c(0,0,0,1,0))

  RegDataV3$ForstLukketLege <- as.character(as.Date(RegDataV3$ForstLukketLege))
  #Kobling med NA fungerer ikke for datotid-var


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
    #RegDataV3$DodsDato <- as.Date(RegDataV3$DodsDato)
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

  #RegDataV2V3$DodsDato <- as.Date(RegDataV2V3$DodsDato) #, origin='1970-01-01')
  #En desimal
  RegDataV2V3$BMI <- round(RegDataV2V3$BMI,1)
  RegDataV2V3$OswTotPre <- round(RegDataV2V3$OswTotPre,1)
  RegDataV2V3$OswTot3mnd <- round(RegDataV2V3$OswTot3mnd,1)
  RegDataV2V3$OswTot12mnd <- round(RegDataV2V3$OswTot12mnd,1)

  message('Ferdig med RegDataV2V3')
  return(RegDataV2V3)
}



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
  RegDataV3$RokerV2 <- plyr::mapvalues(RegDataV3$RokerV3, from = 2, to = 0)
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



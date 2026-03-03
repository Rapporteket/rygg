
#' Kategorisere inngrep og hovedinngrep av operasjoner
#'
#' @param RegData
#'
#' @return Variabler for inngrepskategorisering lagt til i RegData
#' @export
#'
kategoriserInngrep <- function(RegData){
  # Lag ny variabel: InngrepV3 der alle data verdier settes til 0 .
  RegData$InngrepV3 <- 0

  #Skivepotese.
  RegData$InngrepV3[RegData$OpAndreSkiveprotese == 1] <- 17

  #revisjon, fjerning.
  RegData$InngrepV3[RegData$InngrepV3 == 0 &
                      (RegData$OpOsteosyntRevV3 == 1 | RegData$OpOsteosyntFjerningV3 == 1)] <- 16

  #deformitet.
  RegData$InngrepV3[RegData$InngrepV3 == 0 &
                      (RegData$OpKileOsteotomi == 1 | RegData$OpPonteSPOsteotomi == 1 |
                         RegData$OpSkolioseKyfose==1)] <- 10

  #XLIF.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirXlif == 1 & RegData$FusjonKir == 1] <- 15

  #*ALIF.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirAlif == 1 & RegData$FusjonKir == 1] <- 14

  # *TLIF.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirTlif == 1 & RegData$FusjonKir == 1] <- 13

  # *PLIF.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirPlif == 1 & RegData$FusjonKir == 1] <- 12

  #*PLF.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirPlf == 1 & RegData$FusjonKir == 1] <- 11

  # *Laminectomi.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OpLaminektomi == 1 & RegData$FusjonKir == 0] <- 9

  # *Midlinje bevarende dekompresjon.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & (RegData$OpDeUlamin == 1 | RegData$OpProOsteotomi == 1) &
                      RegData$FusjonKir == 0 & RegData$OpProlap < 1] <- 8


  # *Prolapskirurgi åpen.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OpProlap > 0 &
                      RegData$OpMikroV3 == 0 & RegData$FusjonKir == 0] <- 7



  # *Prolapskirurgi mikro.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & (RegData$OpProlap > 0) &
                      RegData$OpMikroV3 > 0 & RegData$FusjonKir == 0] <- 6

  # Prolaps udefinert, defineres inn i prolaps mikro.
  RegData$InngrepV3[RegData$InngrepV3 == 0 &
                      ((RegData$OpProlap > 0) & (RegData$RfSkive == 1) &
                         (RegData$FusjonKir  !=  1) & (RegData$OpMikroV3  !=  0))] <- 6

  # *Udefinert fusjon.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKir == 1] <- 5

  # *Udefinert fusjon_2.
  RegData$InngrepV3[RegData$InngrepV3 == 0
                    & ((RegData$OpAndrePerkutanFusjon == 1) | (RegData$FusjonUtenDekompr == 1))] <- 5

  # *Udefinert, defineres inn i Midlinje bevarende dekompresjon.
  RegData$InngrepV3[RegData$InngrepV3 == 0
                    & ((RegData$RfSkive == 0 & RegData$OpLaminektomi != 1) & (RegData$OpProlap == 0))] <- 8

  # *Udefinert, defineres inn i Laminektomi.
  RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OpLaminektomi == 1] <- 9

  # RECODE InngrepV3, InngrepV2V3 'fusjonV2 og V3 Inngrep'.
  RegData$InngrepV2V3 <- as.numeric(dplyr::recode(
    RegData$InngrepV3, '6'='1', '7'='2', '8'='3', '9'='4', '4'='5', '11'='6', '12'='7',
    '13'='8', '14'='9', '15'='10', '0'='0', '5'='11','10'='12', '16'='13', '17'='14'))

  InngrepV2V3_txt <- c('Udef.', 'Prolaps micro', 'Prolaps åpen', 'Dekompresjon',
                       'Laminektomi', 'Eksp. intersp impl.', 'PLF', 'PLIF', 'TLIF', 'ALIF', 'XLIF',
                       'Udef. fusjon', 'Osteotomi/deform.', 'Revisjon', 'Skiveprotese')
  InngrepV2V3_txtlang <- c('Andre inngrep', 'Prolaps micro', 'Prolaps åpen', 'Midtlinjebev. dekompresjon',
                           'Laminektomi', 'Eksp. intersp implantat', 'PLF', 'PLIF', 'TLIF', 'ALIF', 'XLIF',
                           'Udefinert fusjon', 'Osteotomi, deformitet', 'Revisjon,fjerning av implantat',
                           'Skiveprotese')

  # VALUE LABELS  InngrepV2V3
  # 0 'Andre inngrep udefinert'
  # 1 'Prolaps micro'
  # 2 'Prolaps åpen'
  # 3 'Midtlinjebevarende dekompresjon'
  # 4 'Laminektomi'
  # 5 'Ekspanderende intersp implantat'
  # 6 'PLF'
  # 7 'PLIF'
  # 8 'TLIF'
  # 9 'ALIF'
  # 10 'XLIF'
  # 11 'Udefinert fusjon'
  # 12 'Osteotomi, deformitet'
  # 13 'Revisjon,fjerning av implantat'
  # 14 'Skiveprotese'.



  #*Ny variabelV2 og V3: HovedInngrepV2V3.
  #Fusjon av V2 og V3 HovedInngrep
  #RECODE InngrepV2V3 (0=0) (3=2) (4=3) (5=4) (12=6) (13=7) (14=8) (1 thru 2=1) (6 thru 11=5) INTO
  RegData$HovedInngrepV2V3 <- dplyr::recode(RegData$InngrepV2V3,
                                            '0'='0', '3'='2', '4'='3', '5'='4', '12'='6', '13'='7', '14'='8', '1'='1', '2'='1',
                                            '6'='5', '7'='5', '8'='5', '9'='5', '10'='5', '11'='5')

  HovedInngrepV2V3_txt <- c('Udef.', 'Prolaps', 'Dekomp.', 'Laminektomi', 'Eksp. intersp impl.',
                            'Fusjon', 'Deformitet', 'Revisjon', 'Skiveprotese')
  HovedInngrepV2V3_txtlang <- c('Andre inngrep', 'Prolapskirurgi', 'Midtlinjebevarende dekompr.',
                                'Laminektomi', 'Eksp. intersp implantat', 'Fusjonskirurgi', 'Osteotomi, deformitet',
                                'Revisjon,fjerne implantat', 'Skiveprotese')

  # VALUE LABELS HovedInngrepV2V3
  # 0 'Andre inngrep udefinert'
  # 1 'Prolaps kirurgi'
  # 2 'Midtlinjebevarende dekompresjon'
  # 3 'Laminektomi'
  # 4 'Ekspanderende intersp implantat'
  # 5 'Fusjonskirurgi'
  # 6 'Osteotomi, deformitet'
  # 7 'Revisjon,fjerning av implantat'
  # 8 'Skiveprotese'.

  utdata <- list(RegData=RegData,
                 HovedInngrTxtLang = HovedInngrepV2V3_txtlang,
                 HovedInngrTxt = HovedInngrepV2V3_txt,
                 InngrepTxtLang = InngrepV2V3_txtlang,
                 InngrepTxt = InngrepV2V3_txt)
  return(utdata)
}




#' Definisjon av diagnosegrupper prolaps og spinal stenose, V3
#'
#' @param RegDataV3
#'
#' @return Variablene LSSopr, ProlapsDekr, ProlapsFusjonert, ProlapsoprAlle
#' legges til opprinnelig dataramme
#' @export
#'

defProSS <- function(RegDataV3){
  #--------Definasjon av diagnosegrupper prolaps og spinal stenose V3----
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

  return(RegDataV3)
  }

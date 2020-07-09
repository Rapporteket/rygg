
#' Kategorisere inngrep og hovedinngrep av operasjoner
#'
#' @param RegData
#'
#' @return
#' @export
#'
kategoriserInngrep <- function(RegData){
# Lag ny variabel: InngrepV3 der alle data verdier settes til 0 .
RegData$InngrepV3 <- 0

#Skivepotese.
#DO IF (OpAndreSkiveprotese = 1).
#RECODE InngrepV3  (0 = 17).
RegData$InngrepV3[RegData$OpAndreSkiveprotese == 1] <- 17

#revisjon, fjerning.
#DO IF InngrepV3 = 0 & (OpOsteosyntRevV3 = 1 or OpOsteosyntFjerningV3 = 1) .
#RECODE InngrepV3  (0 = 16).
RegData$InngrepV3[RegData$InngrepV3 == 0 &
                    (RegData$OpOsteosyntRevV3 == 1 | RegData$OpOsteosyntFjerningV3 == 1)] <- 16
table(RegData$InngrepV3)

#deformitet.
#DO IF InngrepV3 = 0 & (OpKileOsteotomi = 1 or OpPonteSPOsteotomi= 1 or OpSkolioseKyfose =1).
#RECODE InngrepV3  (0 = 10).
RegData$InngrepV3[RegData$InngrepV3 == 0 & (RegData$OpKileOsteotomi == 1 | RegData$OpPonteSPOsteotomi == 1
                  |RegData$OpSkolioseKyfose)] <- 10

#XLIF.
# DO IF InngrepV3 = 0 & (FusjonKirXlif = 1 & FusjonKir =1).
# RECODE InngrepV3  (0 = 15).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirXlif == 1 & RegData$FusjonKir == 1] <- 15

#*ALIF.
# DO IF InngrepV3 = 0 & (FusjonKirAlif = 1 & FusjonKir =1).
# RECODE InngrepV3  (0 = 14).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirAlif == 1 & RegData$FusjonKir == 1] <- 14

# *TLIF.
# DO IF InngrepV3 = 0 & (FusjonKirTlif = 1 & FusjonKir =1).
# RECODE InngrepV3  (0 = 13).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirTlif == 1 & RegData$FusjonKir == 1] <- 13

# *PLIF.
# DO IF InngrepV3 = 0 & (FusjonKirPlif = 1 & FusjonKir =1).
# RECODE InngrepV3  (0 = 12).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirPlif == 1 & RegData$FusjonKir == 1] <- 12

#*PLF.
#DO IF InngrepV3 = 0 & (FusjonKirPlfV3 = 1 & FusjonKir =1).
#RECODE InngrepV3  (0 = 11).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKirPlfV3 == 1 & RegData$FusjonKir == 1] <- 11

# *Laminectomi.
# DO IF InngrepV3 = 0 & (OpLaminektomi = 1 & FusjonKir =0).
# RECODE InngrepV3  (0 = 9).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OpLaminektomi == 1 & RegData$FusjonKir == 0] <- 9

# *Midlinje bevarende dekompresjon.
# DO IF InngrepV3 = 0 & (OpDeUlamin = 1 or OpProOsteotomi = 1) & OprProlap < 1 & FusjonKir =0.
# RECODE InngrepV3 (0 = 8).
RegData$InngrepV3[RegData$InngrepV3 == 0 & (RegData$OpDeUlamin == 1 | RegData$OpProOsteotomi == 1)
                  & RegData$FusjonKir == 0 & RegData$OprProlap < 1] <- 8


# *Prolapskirurgi åpen.
# DO IF InngrepV3 = 0 & OprProlap > 0 & OpMikroV3 = 0 & FusjonKir =0.
# RECODE InngrepV3 (0 = 7).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OprProlap > 0
                  & RegData$OpMikroV3 == 0 & RegData$FusjonKir == 0] <- 7



# *Prolapskirurgi mikro.
# DO IF InngrepV3 = 0 & OprProlap > 0 & OpMikroV3 > 0 & FusjonKir =0.
# RECODE InngrepV3 (0 = 6).
RegData$InngrepV3[RegData$InngrepV3 == 0 & (RegData$OprProlap > 0)
                  & RegData$OpMikroV3 > 0 & RegData$FusjonKir == 0] <- 6

# Prolaps udefinert, defineres inn i prolaps mikro.
# DO IF InngrepV3 = 0 & (RfSkive = 1 & OprProlap > 0 & FusjonKir  ~=  1 & OpMikroV3  ~=  0).
# RECODE InngrepV3 (0 = 6).
RegData$InngrepV3[RegData$InngrepV3 == 0 &
                    ((RegData$OprProlap > 0) & (RegData$RfSkive == 1)
                     & (RegData$FusjonKir  !=  1) & (RegData$OpMikroV3  !=  0))] <- 6

# *Udefinert fusjon.
# DO IF InngrepV3 = 0 & FusjonKir = 1.
# RECODE InngrepV3 (0 = 5).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$FusjonKir == 1] <- 5

# *Udefinert fusjon_2.
# DO IF InngrepV3 = 0 & (OpFusjonPerkutan = 1 or OpFusjonUtenDekomprV3 = 1).
# RECODE InngrepV3 (0 = 5).
RegData$InngrepV3[RegData$InngrepV3 == 0
                  & ((RegData$OpFusjonPerkutan == 1) & (RegData$OpFusjonUtenDekomprV3 == 1))] <- 5

# *Udefinert, defineres inn i Midlinje bevarende dekompresjon.
# DO IF InngrepV3 = 0 & (RfSkive = 0 & OpLaminektomi ~= 1) & OprProlap = 0.
# RECODE InngrepV3 (0 = 8).
RegData$InngrepV3[RegData$InngrepV3 == 0
                  & ((RegData$RfSkive == 0) & (RegData$OpLaminektomi != 1) & (RegData$OprProlap == 0))] <- 8

# *Udefinert, defineres inn i Laminektomi.
# DO IF InngrepV3 = 0 & OpLaminektomi = 1.
# RECODE InngrepV3 (0 = 9).
RegData$InngrepV3[RegData$InngrepV3 == 0 & RegData$OpLaminektomi == 1] <- 9
table(RegData$InngrepV3)
# 0    5    6    7    8    9   10   11   12   13   14   15   16   17
# 7    4 1327    2 1358   98 3859  164    7  159   39    8   79  114
# RECODE InngrepV3 (6=1) (7=2) (8=3) (9=4) (4=5) (11=6) (12=7) (13=8) (14=9) (15=10) (0=0) (5=11)
# (10=12) (16=13) (17=14) INTO InngrepV2V3.
# VARIABLE LABELS  InngrepV2V3 'fusjonV2 og V3 Inngrep'.
RegData$InngrepV2V3 <- as.numeric(dplyr::recode(
  RegData$InngrepV3, '6'='1', '7'='2', '8'='3', '9'='4', '4'='5', '11'='6', '12'='7',
  '13'='8', '14'='9', '15'='10', '0'='0', '5'='11','10'='12', '16'='13', '17'='14'))

table(RegData$InngrepV2V3)
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
table(RegData$HovedInngrepV2V3)
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




# Fra notater:
#   Variabler: SPC for hver avdeling for å se når endring ble innført
#   1.jan. 2007 - ut 2024. Få med 12mnd k
#   Indikatorer fra forrige artikkel
#   Mikroskop vs durarift, to akser, antibiotika og infeksjon
#   Alle separat for ss og prolaps
#   Andre som har publisert effekt av kvalitetsregister?? Dvs. forbedring av kvalitetsindikatorer
#   Scatterplot for å se på data?


# ------- Figur	Ventetid (valgtVar == 'ventetidHenvTimePol')----------
#  grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
# RegData$VariabelGr <- factor(RegData$VentetidHenvTilSpesialist, levels = c(1:4,9))
# Y1-akse:	Andel med ventetid under 3 måneder fra ryggkirurgi er besluttet til operasjon er utført
# Utvalg til Y1-akse:	Alle elektivt opererte pasienter
# Y2-akse:	Andel pasienter med < 12 måneder varighet av beinsmerte
# Utvalg til Y2-akse	Alle elektivt opererete prolapspasienter

library(rygg)
library(ggplot2)
library(tidyverse)
source("dev/sysSetenv.R")
RyggDataAlle <- RyggPreprosess(RegData = RyggRegDataSQLV2V3(datoFra = '2019-01-01'))


#Data
RegDataElektiv <-
  RyggDataAlle %>% dplyr::filter(OpKat %in% c(1,3))

RegDataElektivProlaps <-
  RegDataElektiv %>% dplyr::filter(HovedInngrepV2V3 ==1)

#VentetidSpesialistTilOpr
grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
levels <- c(1:4,9)

#SympVarighUtstr
grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ikke utfylt')
gr <- c(1:5,9)


data <- data.frame(VentU3m = tapply(X=RegDataElektiv$VentetidSpesialistTilOpr,
                                    INDEX = RegDataElektiv$Aar,
                                    FUN =  function(x) {sum(x==1)/sum(x %in% 1:4)} ),
                   U12mndVarigh = tapply(RegDataElektivProlaps$SympVarighUtstr,
                                    INDEX = RegDataElektivProlaps$Aar,
                                    FUN = function(x){sum(x %in% 1:3)/sum(x %in% 1:5)} )
                   )
dataRygg <- data.frame(Aar = as.numeric(row.names(data)),
                   data)


#Utvikling av figur
# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>), stat = <Stat>, position = <Position>) +
#   <Coordinate_Function> + <Facet_Function> +  <Scale_Function> +   <Theme_Function>

# scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

# df %>%
#andelTidflereVar <- function(RegData, x, y1, y2, yAkseTxt,...){
  ggplot2::ggplot(dataRygg) +
  geom_line(aes(x = Aar, y = VentU3m),
            color = "steelblue", linewidth = 1.3) +
  geom_point(aes(x = Aar, y = VentU3m),
             color = "steelblue", size = 2)  +
  labs( title = "Ventetid < 3 mnd. fra kirurgi besluttes til utført operasjon",
        x = "År",
        y = "Andel ventetid under 3 mnd." ) +
  coord_cartesian(ylim = c(0 ,1))  +
    geom_line(aes(x = Aar, y= U12mndVarigh), color = 'darkgreen', linewidth = 1.3)
  ggsave("Ventetid.png", width = 5, height = 5)
#}


  scale_y_continuous(
    "mpg (US)",
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  )




#--- Ventetid
#Andel med ventetid under 3 måneder fra ryggkirurgi er besluttet til operasjon er utført
# Elektivt opererte pasienter

#Andel pasienter med < 12 måneder varighet av beinsmerte
# Utvalg: Elektive, prolaps

#--- Indikasjonsstilling
# Andel pasienter med lite beinsmerte (NRS≤3) og ingen parese
# Utvalg: prolaps
# Kan vurderes inbakt i suksess prolapsfirgur

#--- Degenerativ spondylolistese
#  1. Andel som blir operert med fusjonskirurgi ved første operasjon
#  (om mulig: 2. andel som blir operert med mikrokirurgi ved første operasjon)
# Utvalg:  Alle pasienter operert for første gang for degenerativ spondylolistese

# Y-akse2: Andel som oppnår suksesskriterium ved 12 mnd (ODI forbedring ≥ 30% fra baseline)
# Utvalg: Pasienter operert for første gang for degenerativ spondylolistese
# SPM: Skal vi evt. ta bort de som har vært ryggoperert for noe annet før første spondylolisteseoperasjon?


#--- Suksess prolaps
  #Andel som oppnår suksesskriterium ved 12 måneder (≥ 20 poeng forbedring i ODI)
  #Utvalg: Prolaps

  #1. Andel > 70 år (om mulig:
  #2. Andel med ASA grad > 2)
  #Utvalg: Prolapspasienter

#--- Suksess spinal stenose
  #Andel som oppnår suksesskriterium ved 12 måneder (≥ 30 % forbedring i ODI)
  #Utvalg: Lumbal spinal stenose

  #1. Andel > 70 år
  # (om mulig: 2. Andel med ASA grad > 2)
  #Utvalg: Lumbal spinal stenose

#--- Unødvendig tromboseprofylakse
#  Andel menn uten risikofaktorer som får tromboseprofylekse i forbindelse med lett ryggkirurgi
  #Utvalg: Menn med ASA < 3
  #Statistisk prosesskontroll senere?


#--- Trygg kirurgi
  #Alle pasienter
  #Andel pasienter der "trygg kirurgi" brukes

  #Om mulig: 1. Andel der "trygg kirurgi" brukes ved operasjonsstart.
  #2. Andel der "trygg kirurgi" brukes ved operasjon avsluttning

#--- Sårinfeksjon etter prolapsoperasjon
  #Utvalg: prolaps
  #y1:Andel pasienter som fikk sårinfeksjon innen 3 måneder

  #y2:Andel som fikk peroperativ antibiotika


#--- Sårinfeksjon etter lumbal spinal stenoseoperasjon
  #Utvalg: lumbal spinal stenose
  #y1:Andel pasienter som fikk sårinfeksjon innen 3 måneder
  #y2:Andel som fikk peroperativ antibiotika	Alle lumbal spinal stenosepasienter


#--- Durarift ved prolapsoperasjon
  #Utvalg: prolaps
  #Andel pasienter som fikk durarift
  #y2: Andel pasienter der det ble brukt mikroskop eller lupebriller under operasjonen

#--- Durarift ved lumbal spinal stenoseoperasjon
  #Utvalg: lumbal spinal stenosepasienter
  #y1: Andel pasienter som fikk durarift
  #y2:Andel pasienter der det ble brukt mikroskop eller lupebriller under operasjonen


#--- BMI og alder, alle pasienter ikke med i artikkelen
  #--Vektreduksjon	Gjennomsnittlig BMI
  #  Røykere	Andel røykere


#--- Andel forverring
  #Andel som ble verre etter prolapsoperasjon (ODI raw score > 48 poeng ved 12 måneder)
  #Utvalg: prolaps
  #y1: Andel som ble verre etter LSSoperasjon (ODI raw score ≥ 39 poeng ved 12 måneder)
  #Utvalg: lumbal spinal stenose
  #Kan vurderes inbakt i suksess figurer






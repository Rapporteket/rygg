

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



#Utvikling av figur
# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>), stat = <Stat>, position = <Position>) +
#   <Coordinate_Function> + <Facet_Function> +  <Scale_Function> +   <Theme_Function>
# scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

plotArt <- function(dataRygg, tittel='Tittel'){
   ggplot2::ggplot(dataRygg, aes(x = Aar, y = VarVerdi, colour = VarTxt)) +
  geom_line(linewidth = 1.1) +
   scale_color_manual(
     values = c("steelblue", "darkblue"),
     #labels = c("A" = "First line", "B" = "Second line")
     ) +  # legend text
    geom_point(size = 1.5) + #aes(x = Aar, y = VentU3m), )  +
    ylim(0, NA) +
    labs(title = tittel,
        x = "År",
        y = "Andel (%)",
        color = " ") +
    theme_minimal() +
    theme(legend.position = "top")
 }

dataRygg <- function(var1, var2,
                     txt1='Var 1', txt2='Var 2'){
  var <- rbind(cbind(Var12 =var1 ,
                     Variabel = txt1),
               cbind(Var12 = var2,
                     Variabel = txt2))

  dataRygg <- data.frame(
    Aar = as.numeric(row.names(var)),
    VarVerdi = as.numeric(var[,'Var12']),
    VarTxt = var[ ,'Variabel']
  )
}
#--- 1 Ventetid ----------------------
#Andel med ventetid under 3 måneder fra ryggkirurgi er besluttet til operasjon er utført
# Elektivt opererte pasienter
#VentetidSpesialistTilOpr
grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
levels <- c(1:4,9)

#SympVarighUtstr
grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ikke utfylt')
gr <- c(1:5,9)

VentU3m <- tapply(X=RegDataElektiv$VentetidSpesialistTilOpr,
                 INDEX = RegDataElektiv$Aar,
                 FUN =  function(x) {sum(x==1)/sum(x %in% 1:4)} )

U12mndVarigh <- tapply(RegDataElektivProlaps$SympVarighUtstr,
                      INDEX = RegDataElektivProlaps$Aar,
                      FUN = function(x){sum(x %in% 1:3)/sum(x %in% 1:5)} )

p <- plotArt(dataRygg = dataRygg(var1=VentU3m, var2=U12mndVarigh,
                                 txt1='<3mnd fra beslutning til operasjon',
                                 txt2='Utstrålende smerter <12mnd (prolapsop)'),
        tittel = "Ventettid <3mnd fra beslutning til utført operasjon")
p
ggsave("Ventetider.png", p) #, width = 5, height = 5)

#--- Degenerativ spondylolistese ---------------------
#  1. Andel som blir operert med fusjonskirurgi ved første operasjon
#  (om mulig: 2. andel som blir operert med mikrokirurgi ved første operasjon)

RyggdataDSprimEl <- RyggUtvalgEnh(RyggDataAlle,
                               hovedkat=10, #Degen spond
                               tidlOp = 4, #Primærop
                               hastegrad = 1)$RegData #Elektive

RyggDataDSprimElOsw <- RyggVarTilrettelegg(RegData = RegDataDSprimEl,
                                           valgtVar = 'OswEndr30pst12mnd')$RegData

degSponFusj <- tapply(X=RyggDataDSprimElOsw$HovedInngrep,
                  INDEX = RyggDataDSprimElOsw$Aar,
                  FUN =  function(x) {sum(x==5)/length(x)} ) #Fusjonskirurgi

# Y-akse2: Andel som oppnår suksesskriterium ved 12 mnd (ODI forbedring ≥ 30% fra baseline)
# Utvalg: Pasienter operert for første gang for degenerativ spondylolistese

OswEndr30pst12mnd <- tapply(RyggDataDSprimElOsw$Variabel,
                       INDEX = RyggDataDSprimElOsw$Aar,
                       FUN = 'mean' )

p <- plotArt(dataRygg = dataRygg(var1=degSponFusj, var2=OswEndr30pst12mnd,
                                 txt1='Fusjonskirurgi, primærop ',
                                 txt2='Minst 30% bedring av ODI etter 1 år'),
             tittel = 'Degen. spondylolistese operert med fusjonskirurgi')
p
ggsave("DegenSpondFusjODI.png", p) #, width = 5, height = 5)

#--- Suksess prolaps ---------------------
  #Andel som oppnår suksesskriterium ved 12 måneder (≥ 20 poeng forbedring i ODI)
  #Utvalg: Prolaps


RyggDataProlaps <-
  RyggDataAlle %>% dplyr::filter(HovedInngrepV2V3 ==1)

RyggDataProlapsODI <-
  RyggVarTilrettelegg(RegData = RyggDataProlaps, valgtVar = 'OswEndr20', ktr = 2)$RegData


var1 <- tapply(X=RyggDataProlapsODI$Variabel,
                      INDEX = RyggDataProlapsODI$Aar,
                      FUN =  mean )

#1. Andel > 70 år (om mulig:
#2. Andel med ASA grad > 2)
#Utvalg: Prolapspasienter
RyggData70 <-  RyggVarTilrettelegg(RegData = RyggDataProlapsODI,
                                   valgtVar = 'alder70')$RegData

var2 <- tapply(RyggData70$Variabel,
             INDEX = RyggData70$Aar,
             FUN = 'mean' )

p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='≥ 20 poeng forbedring i ODI',
                                 txt2='over 70 år'),
             tittel = 'Prolapspasienter, besvart 12mnd-skjema')
p
ggsave("Pro_ODI.png", p) #, width = 5, height = 5)


#--- Suksess spinal stenose ---------------------
  #Andel som oppnår suksesskriterium ved 12 måneder (≥ 30 % forbedring i ODI)
  #Utvalg: Lumbal spinal stenose

RyggDataSS <- RyggUtvalgEnh(RegData = RyggDataAlle,
                            hovedkat = 9)$RegData

RyggDataSSODI <-
  RyggVarTilrettelegg(RegData = RyggDataSS, valgtVar = 'OswEndr30pst12mnd', ktr = 2)$RegData

var1 <- tapply(X=RyggDataSSODI$Variabel,
               INDEX = RyggDataSSODI$Aar,
               FUN =  mean )

#1. Andel > 70 år
# (om mulig: 2. Andel med ASA grad > 2)
#Utvalg: Lumbal spinal stenose

RyggData70SS <-  RyggVarTilrettelegg(RegData = RyggDataSSODI,
                                   valgtVar = 'alder70')$RegData

var2 <- tapply(RyggData70SS$Variabel,
               INDEX = RyggData70SS$Aar,
               FUN = 'mean' )

p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='≥ 30% forbedring i ODI',
                                 txt2='over 70 år'),
             tittel = 'Lumbal spinal stenose, besvart 12mnd-skjema')
p
ggsave("SS_ODI.png", p) #, width = 5, height = 5)




#--- Trygg kirurgi ---------------------
  #Alle pasienter
  #Andel pasienter der "trygg kirurgi" brukes

# RyggDataTryggK <-
#   RyggVarTilrettelegg(RegData = RyggDataAlle, valgtVar = 'tryggKir', figurtype = 'andelGrVar')$RegData

RyggFigAndelTid(RegData = RyggDataAlle, preprosess = 0,
                valgtVar ='tryggKir',
                outfile = 'TryggKir.png')

#Tror ikke vi har opplysninger om dette:
#Om mulig: 1. Andel der "trygg kirurgi" brukes ved operasjonsstart.
#2. Andel der "trygg kirurgi" brukes ved operasjon avsluttning




#--- Sårinfeksjon etter prolapsoperasjon ---------------------
  #Utvalg: prolaps
  #y1:Andel pasienter som fikk sårinfeksjon innen 3 måneder

RyggDataInfPro <- RyggVarTilrettelegg(RegData = RyggDataProlaps,
                                   valgtVar = 'kpInf3mnd')$RegData

var1 <- tapply(X=RyggDataInfPro$Variabel,
               INDEX = RyggDataInfPro$Aar,
               FUN =  mean )

#y2:Andel som fikk peroperativ antibiotika

RyggDataProInfAnti <- RyggVarTilrettelegg(RegData = RyggDataInfPro,
                                   valgtVar = 'antibiotika',
                                   figurtype = 'andelGrVar')$RegData

var2 <- tapply(1-RyggDataProInfAnti$Variabel,
               INDEX = RyggDataProInfAnti$Aar,
               FUN = 'mean' )

p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='rapportert sårinfeksjon, 3 mnd.',
                                 txt2='IKKE fått antibiotika perop.'),
             tittel = 'Sårinfeksjon etter prolapsoperasjon')
p
ggsave("Inf_prolaps.png", p) #, width = 5, height = 5)



#--- Sårinfeksjon etter lumbal spinal stenoseoperasjon ---------------------
  #Utvalg: lumbal spinal stenose
  #y1:Andel pasienter som fikk sårinfeksjon innen 3 måneder
  #y2:Andel som fikk peroperativ antibiotika	Alle lumbal spinal stenosepasienter

RyggDataInfSS <- RyggVarTilrettelegg(RegData = RyggDataSS,
                                   valgtVar = 'kpInf3mnd')$RegData

var1 <- tapply(X=1-RyggDataInfSS$Variabel,
               INDEX = RyggDataInfSS$Aar,
               FUN =  mean )

#y2:Andel som fikk peroperativ antibiotika

RyggDataInfSSAnti <- RyggVarTilrettelegg(RegData = RyggDataInfSS,
                                       valgtVar = 'antibiotika',
                                       figurtype = 'andelGrVar')$RegData

var2 <- tapply(RyggDataInfSSAnti$Variabel,
               INDEX = RyggDataInfSSAnti$Aar,
               FUN = 'mean' )

p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='rapportert sårinfeksjon, 3 mnd.',
                                 txt2='IKKE fått antibiotika perop.'),
             tittel = 'Sårinfeksjon etter prolapsoperasjon')
p
ggsave("Inf_SS.png", p) #, width = 5, height = 5)


#--- Durarift ved prolapsoperasjon ---------------------
  #Utvalg: prolaps
  #Andel pasienter som fikk durarift

RyggDataProlaps$Variabel <- 1
RyggDataProlaps$Variabel[which(RyggDataProlaps$PeropKompDura == 1)] <- 0

var1 <- tapply(X=RyggDataProlaps$Variabel,
               INDEX = RyggDataProlaps$Aar,
               FUN =  mean )

#y2: Andel pasienter der det ble brukt mikroskop eller lupebriller under operasjonen

var2 <- tapply(X=RyggDataProlaps$OpMikroV3,
                  INDEX = RyggDataProlaps$Aar,
                  FUN =  function(x) {sum(x%in% 1:2)/sum(x %in% 0:3)} )


p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='IKKE durarift',
                                 txt2='brukt mikroskop/lupebriller'),
             tittel = 'Durarift etter prolapsoperasjon')
p
ggsave("DuraPro.png", p) #, width = 5, height = 5)

#--- Durarift ved lumbal spinal stenoseoperasjon ---------------------
  #Utvalg: lumbal spinal stenosepasienter
  #y1: Andel pasienter som fikk durarift
  #y2:Andel pasienter der det ble brukt mikroskop eller lupebriller under operasjonen

RyggDataSS$Variabel <- 1
RyggDataSS$Variabel[which(RyggDataSS$PeropKompDura == 1)] <- 0

var1 <- tapply(X=RyggDataSS$Variabel,
               INDEX = RyggDataSS$Aar,
               FUN =  mean )

#y2: Andel pasienter der det ble brukt mikroskop eller lupebriller under operasjonen

var2 <- tapply(X=RyggDataSS$OpMikroV3,
               INDEX = RyggDataSS$Aar,
               FUN =  function(x) {sum(x%in% 1:2)/sum(x %in% 0:3)} )


p <- plotArt(dataRygg = dataRygg(var1=var1, var2=var2,
                                 txt1='IKKE durarift',
                                 txt2='brukt mikroskop/lupebriller'),
             tittel = 'Durarift etter lumbal Spinal stenose')
p
ggsave("DuraSS.png", p) #, width = 5, height = 5)


#--------Andel pasienter med < 12 måneder varighet av beinsmerte ---------------------
# Utvalg: Elektive, prolaps


#--- Indikasjonsstilling ---------------------
# Andel pasienter med lite beinsmerte (NRS≤3) og ingen parese
# Utvalg: prolaps
# Kan vurderes inbakt i suksess prolapsfirgur

#--- Unødvendig tromboseprofylakse ---------------------
#  Andel menn uten risikofaktorer som får tromboseprofylekse i forbindelse med lett ryggkirurgi
#Utvalg: Menn med ASA < 3
#Statistisk prosesskontroll senere?


#--- BMI og alder, alle pasienter ikke med i artikkelen ---------------------
  #--Vektreduksjon	Gjennomsnittlig BMI
  #  Røykere	Andel røykere


#--- Andel forverring
  #Andel som ble verre etter prolapsoperasjon (ODI raw score > 48 poeng ved 12 måneder)
  #Utvalg: prolaps
  #y1: Andel som ble verre etter LSSoperasjon (ODI raw score ≥ 39 poeng ved 12 måneder)
  #Utvalg: lumbal spinal stenose
  #Kan vurderes inbakt i suksess figurer



#--------EGENKURSING--------
#  data, aesthetics (estetikk) and geoms
library('ggplot2')

#  mpg - miles per gallon
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point()
#  data and aesthetic mappings are supplied in ggplot(), then layers are added on with +.
#To add additional variables to a plot, we can use other aesthetics
  #like colour, shape, and size

  ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point()
  ggplot(mpg, aes(displ, hwy, shape = drv)) +
    geom_point()
  ggplot(mpg, aes(displ, hwy, size = cyl)) +
    geom_point()

  #See vignette("ggplot2-specs") for the values needed for colour and other aesthetics.

  #Faceting:
  facet_wrap(~class)

  #Fordelinger
  ggplot(drugs, aes(drug, effect)) + geom_bar(stat = "identity")
  ggplot(drugs, aes(drug, effect)) + geom_point()

  #Tidsserier
  ggplot(economics, aes(date, 100*unemploy / pop)) +
    geom_line()
  ggplot(economics, aes(date, uempmed)) +
    geom_line()

  year <- function(x) as.POSIXlt(x)$year + 1900
  ggplot(economics, aes(unemploy / pop, uempmed)) +
    geom_path(colour = "grey50") +
    geom_point(aes(colour = year(date)))

  ggplot(mpg, aes(cty, hwy)) + geom_point() +
    xlab('hwy - highway driving') +
    ylab('cty - citydriving') +
    ylim(0,NA)

  #Kan ta vare på figuren i en variabel:
  p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
    geom_point()

  #Lagre:
  ggsave("plot.png", p, width = 5, height = 5)


  #Flere lag, ulike data
  mod <- loess(hwy ~ displ, data = mpg)
  std_resid <- resid(mod) / mod$s
  outlier <- filter(mpg, abs(std_resid) > 2)
  grid <- tibble(displ = seq(min(mpg$displ), max(mpg$displ), length = 50))
  grid$hwy <- predict(mod, newdata = grid)

  ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_line(data = grid, colour = "blue", linewidth = 1.5) +
    geom_text(data = outlier, aes(label = model), colour='red')+
    geom_point(data=outlier, aes(x=displ, y=hwy), colour='red', size=2)


  #Prøv: 13.3.1 Exercises

  #Fra copilot
  # Plot with two lines and a legend
  df <- data.frame( x = rep(1:10, 2),
                    y = c(1:10, (1:10)*1.5),
                    group = rep(c("Line A", "Line B"), each = 10) )
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(linewidth = 1.2) +
    labs( title = "Two-Line Plot with Legend",
          x = "X-axis",
          y = "Y-axis",
          color = "Legend" ) +
    theme_minimal()

  df <- data.frame( x = rep(1:10, 2),
                    y = c(1:10, (1:10)*1.5),
                    group = rep(c("A", "B"), each = 10) )
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(
      values = c("A" = "steelblue", "B" = "firebrick"),
      labels = c("A" = "First line", "B" = "Second line") # legend text
      ) +
    labs( color = "My Legend Heading", # legend heading
          x = "X-axis", y = "Y-axis" ) +
    theme_minimal()











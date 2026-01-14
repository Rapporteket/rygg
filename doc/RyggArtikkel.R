

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
RyggDataAlle <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())


Utv <- RyggVarTilrettelegg(RegData=RyggDataAlle, figurtype = 'andelTid',
                           valgtVar = 'ventetidSpesOp')
RyggData <- Utv$RegData

TestData <- prop.table(table(RyggData[ ,c('Variabel', 'Aar')]), margin = 2)[2, ]
df <- dplyr::tibble( Aar = as.integer(names(TestData)), Andel = as.numeric(TestData) )

#Data
RegData <- RegData[which(RegData$VentetidSpesialistTilOpr %in% 1:4),]
RegData$Variabel[which(RegData$VentetidSpesialistTilOpr == 1)] <- 1

#VentetidSpesialistTilOpr
grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
levels <- c(1:4,9)

#SympVarighUtstr
grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ikke utfylt')
gr <- c(1:5,9)

Andel <-  function(x) {sum(x==1)/length(x)}
data <- data.frame(VentU3m = tapply(RyggData$VentetidSpesialistTilOpr,
                                    INDEX = RyggData$Aar,
                                    FUN =  function(x) {sum(x==1)/sum(x %in% 1:4)} ),
                   U12mndVarigh = tapply(RyggData$SympVarighUtstr,
                                    INDEX = RyggData$Aar,
                                    FUN = function(x){sum(x %in% 1:3)/sum(x %in% 1:5)} )
)
data <- data.frame(Aar = as.numeric(row.names(data)),
                   data)
data

#Utvikling av figur
# scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

# df %>%
  ggplot2::ggplot(data,
                  aes(x = Aar,
                      y = c(VentU3m, U12mndVarigh)) +
  geom_line(color = "steelblue", size = 1.3) +
  geom_point(color = "steelblue", size = 2)  +
  labs( title = "Ventetid < 3 måneder fra kirurgi er besluttet til utført operasjon",
        x = "År", y = "Andel ventetid <3mnd" ) +
  coord_cartesian(ylim = c(0 ,1))
  ) # +
# aes(data, x = Aar, y= U12mndVarigh) +
#    geom_line(color = 'darkgreen')



  scale_y_continuous(
    "mpg (US)",
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  )

# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>), stat = <Stat>, position = <Position>) +
#   <Coordinate_Function> + <Facet_Function> +  <Scale_Function> +   <Theme_Function>



# Fra notater:
#   Variabler: SPC for hver avdeling for å se når endring ble innført
#   1.jan. 2007 - ut 2024. Få med 12mnd k
#   Indikatorer fra forrige artikkel
#   Mikroskop vs durarift, to akser, antibiotika og infeksjon
#   Alle separat for ss og prolaps
#   Andre som har publisert effekt av kvalitetsregister?? Dvs. forbedring av kvalitetsindikatorer
#   Scatterplot for å se på data?


# Figur	Ventetid (valgtVar == 'ventetidHenvTimePol')
#  grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
# RegData$VariabelGr <- factor(RegData$VentetidHenvTilSpesialist, levels = c(1:4,9))
# Y1-akse:	Andel med ventetid under 3 måneder fra ryggkirurgi er besluttet til operasjon er utført
# Utvalg til Y1-akse:	Alle elektivt opererte pasienter
# Y2-akse:	Andel pasienter med < 12 måneder varighet av beinsmerte
# Utvalg til Y2-akse	Alle elektivt opererete prolapspasienter

library(rygg)
source("dev/sysSetenv.R")
RyggDataAlle <- RyggPreprosess(RegData = RyggRegDataSQLV2V3())
Utv <- RyggVarTilrettelegg(RegData=RyggDataAlle, figurtype = 'andelTid',
                           valgtVar = 'ventetidSpesOp')
RyggData <- Utv$RegData
table(RyggData[ ,c('Variabel', 'Aar')])

ggplot2::ggplot(data = RyggData[ ,c('Variabel', 'Aar')]) +


# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>), stat = <Stat>, position = <Position>) +
#   <Coordinate_Function> + <Facet_Function> +  <Scale_Function> +   <Theme_Function>

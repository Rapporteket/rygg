# Litt om SPC og diagrammer

#Dersom dataene er forholdstall, for eksempel et antall hendelser (teller) i forhold til totalt antall
#mulige hendelser (nevner), og nevnerne er større enn 12, bør p-diagram brukes
#Ved sjeldne hendelser brukes enten g-diagram eller datapunkter omgjort til rater (1/x) framstilt i Idiagram.

# Operasjonstid (knivtid)
# Liggetid
# Komplikasjoner: Nerverotskade, Blødning, infeksjon, durarift
# 3mndOppfølging
# EndoSkopTilg (ja/nei) for prolapsopererte (ProlapsoprAlle), EnodoSkopTekn
# Reoperasjon innen 30d
# PROM - 3mnd Oswestry endring og terskel 30% SS 20% prolaps (sjekk), evt. Smerte bein
# ODI

library(qicharts2)

library(rygg)
source("dev/sysSetenv.R")
RegDataRaa <- RyggRegDataV2V3(datoFra = '2020-01-01')
RegData <- RyggPreprosess(RegData =RegDataRaa)
# Prolapskirurgi hos de tre sykehusene slått sammen
# Ullevål Nkir, ReshId = 109820 og Bærum, ReshId = 103094  og Martina Hansenes, ReshId = 110633.
RegData <- RyggUtvalgEnh(RegData=RegData, hovedkat = 1, datoTil='2025-12-31')$RegData
RegData <- RegData[RegData$ReshId %in% c(109820, 103094, 103094), ]
RegData <- SorterOgNavngiTidsEnhet(RegData = RegData, tidsenhet = 'Mnd')$RegData
RegData23 <- RyggUtvalgEnh(RegData=RegData, datoFra = '2023-01-01')$RegData

#Tidsperiode: 2020 tom 2025.
#Fom. 2020 tom 2022 (stabilt nesten ingen endoskopi) fom 2023 tom 2025 implementering av endoskopi.


#Endoskpisk prolapskirurgi: HovedInngrepV2V3 = 1 og  OpAndreEndosk = 1
#Mikrokirurgi for prolaps def:  HovedInngrepV2V3 = 1 og  OpAndreEndosk = 0

# Respons:
# Endoskopi (OpAndreEndosk=1)	Mikrokirurgi for prolaps
# Total operasjonstid (min)	Median eller mean
# Postoperativ liggetid	Median eller mean
# ODI differanse 3 mnd	Andel,  ja

#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
dagind <- which( (is.na(RegData$LiggetidPostop) | is.nan(RegData$LiggetidPostop))  & RegData$Dagkirurgi==1)
RegData$LiggetidPostop[dagind]<-0
RegData <- RegData[which(RegData$LiggetidPostop>=0),]

RegData$OswEndr <- RegData$OswTotPre - RegData$OswTot3mnd

tapply(RegData23$KnivtidTot, INDEX = RegData23$OpAndreEndosk, 'median', na.rm=T )
tapply(RegData23$OswEndr, INDEX = RegData23$OpAndreEndosk, 'mean', na.rm=T )
100*tapply(RegData23$OswEndr>20, INDEX = RegData23$OpAndreEndosk, 'mean', na.rm=T )
tapply(RegData23$LiggetidPostop, INDEX = RegData23$OpAndreEndosk, 'mean', na.rm=T )

PerMnd <- RegData |>
  dplyr::filter(OpAndreEndosk==1) |>
  dplyr::group_by(TidsEnhet)|>
  dplyr::summarise(
    N = dplyr::n(),
    Endo = sum(OpAndreEndosk), # /N, #Ingen NA
    nODI = sum(!is.na(OswEndr)),
    ODIendr = sum(OswEndr >=20, na.rm = T), #/nODI,
    LiggetidPost = sum(LiggetidPostop), #sum(LiggetidPostop),
    KnivtidTot = sum(KnivtidTot)
    ) |>
  dplyr::ungroup()



# Komplikasjoner: Nerverotskade, Blødning, infeksjon, durarift
qicharts2::qic(x = TidsEnhet, #as.Date(OpDato), #
               y = KnivtidTot,
               n = N,
              # agg.fun = "sum",
               data     = PerMnd,
               chart    = 'p',
               title    = 'KnivtidTot, OpAndreEndosk=1',
               # ylab     = 'minutter',
               xlab     = 'Måned',
               x.angle = 90,
               y.percent = FALSE,
               point.size = 2,
               show.95 = TRUE,
               show.labels = TRUE,
               print.summary = TRUE
)

ggplot2::ggsave('KnivtidTot_endo.pdf',
                width = 20,
                height = 20)

qicharts2::qic(x = TidsEnhet, #as.Date(OpDato), #
               y = Reop30d,
               n = n,
               agg.fun = "mean",
               #n        = days,
               data     = PerMndInf, #[RegDataInf$SykehusNavn == 'Gjøvik',],
               chart    = 'p',
               title    = 'Infeksjon rapportert 3 mnd etter',
               #x.period = 'month',
               point.size = 2,
               show.95 = TRUE,
               show.labels = TRUE,
               print.summary = TRUE
)

ggplot2::ggsave('testKniv.pdf',
                width = 20,
                height = 20)


#-----------Div testing---------------------------------------
DataSh <- RegData[RegData$SykehusNavn == 'Elverum',]

chart_data <- qicharts2::qic(x = as.Date(OpDato), #TidsEnhet, #
    y = KnivtidTot,
    agg.fun = "mean",
    #n        = days,
    data     = DataSh,
    facets   =  ~ SykehusNavn,
    chart    = 'i',
    title    = 'Knivtid',
    ylab     = 'minutter',
    xlab     = 'OpDato',
    x.angle = 90,
   # x.period = 'day',
   point.size = 2,
   show.95 = TRUE,
   print.summary = TRUE
)

ggplot2::ggsave('KnivtidDato.pdf',
                width = 20,
                height = 20)



#Andelsdata

RegDataInf <- RyggVarTilrettelegg(RegData = RegData, valgtVar = 'kpInf3mnd')$RegData

PerMndInf <- RegDataInf |>
  dplyr::group_by(TidsEnhet)|> #, SykehusNavn
  dplyr::summarise(
    Infeksjon = sum(Variabel),
    Reop30d = sum(NyRyggOpr3mnd),
    ReopUopph = sum(ReopUnderOpph),
    n         = dplyr::n()) |>
  dplyr::ungroup()

# Komplikasjoner: Nerverotskade, Blødning, infeksjon, durarift
qicharts2::qic(x = TidsEnhet, #as.Date(OpDato), #
               y = Reop30d,
               n = n,
               agg.fun = "mean",
               #n        = days,
               data     = PerMnd, #[RegDataInf$SykehusNavn == 'Gjøvik',],
               #facets   =  ~ SykehusNavn,
               chart    = 'p',
               title    = 'Infeksjon rapportert 3 mnd etter',
               #ylab     = 'minutter',
               xlab     = 'OpDato',
               x.angle = 90,
               y.percent = TRUE,
               #x.period = 'month',
               point.size = 2,
               show.95 = TRUE,
               show.labels = TRUE,
               print.summary = TRUE
)

ggplot2::ggsave('InfeksjonOverordnet.pdf',
                width = 20,
                height = 20)




#Telledata





test <- chart_data$data
violations <- chart_data$data %>% filter(signal != 0)

# Build the plot from scratch in ggplot2
ggplot(chart_data, aes(x = x, y = y)) +
  geom_line(aes(y = y), color = "black") +
  geom_point() +
  # Add centerline and control limits (using values calculated by qic)
  geom_hline(aes(yintercept = mean(y)), linetype = "solid", color = "blue") +
  geom_hline(aes(yintercept = ucl), linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = lcl), linetype = "dashed", color = "red") +
  # Add visual mark for violations
  geom_point(data = violations, aes(x = x, y = y),
             color = "red", size = 4, shape = 8) +
  theme_minimal()

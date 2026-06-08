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
RegDataRaa <- RyggRegDataV2V3(datoFra = '2025-01-01')
RegData <- RyggPreprosess(RegData =RegDataRaa)
RegData <- RyggUtvalgEnh(RegData=RegData, datoTil='2025-12-31')$RegData
RegData <- SorterOgNavngiTidsEnhet(RegData = RegData, tidsenhet = 'Mnd')$RegData

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

PerMnd <- RegDataInf |>
  dplyr::group_by(TidsEnhet, SykehusNavn)|>
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

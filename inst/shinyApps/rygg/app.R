#library(magrittr)
library(dplyr)
library(kableExtra)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(rapbase)
library(rygg)
library(rapFigurer)
library(shiny)
library(shinyjs)
#library(tools)
library(zoo)

idag <- Sys.Date()
startDato <- paste0(as.numeric(format(idag-120, "%Y")), '-01-01') #'2019-01-01' #Sys.Date()-364
sluttDato <- idag
datoTil <- as.POSIXlt(idag)
datofra12 <- lubridate::floor_date(as.Date(datoTil)- months(12, abbreviate = T), unit='month')
idag <- Sys.Date()

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))



context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) #rapbase::isRapContext()
regTitle = ifelse(paaServer, 'NKR: Nasjonalt Kvalitetsregister for Ryggkirurgi',
                  'Nasjonalt Kvalitetsregister for Ryggkirurgi, testversjon med FIKTIVE data')


if (paaServer) {
  RegData <- RyggRegDataSQLV2V3()  #RyggRegDataSQL(alle = 1)
  qSkjemaOversikt <- 'SELECT * from SkjemaOversikt'
  SkjemaOversikt <- rapbase::loadRegData(registryName="rygg", query=qSkjemaOversikt, dbType="mysql")
  qForlop <- 'SELECT AvdRESH, SykehusNavn, Fodselsdato, HovedDato, BasisRegStatus from ForlopsOversikt'
  RegOversikt <- rapbase::loadRegData(registryName="rygg", query=qForlop, dbType="mysql")
  RegOversikt <- dplyr::rename(RegOversikt, 'ReshId'='AvdRESH', 'InnDato'='HovedDato')
} else {
  print('Data ikke tilgjengelig')
}

RegData <- RyggPreprosess(RegData = RegData)
SkjemaOversikt <- dplyr::rename(.data=SkjemaOversikt, !!c(InnDato='HovedDato', ShNavn='Sykehusnavn'))




#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
# c("Egen mot resten av landet"=1,
#   "Hele landet"=0,
#   "Egen enhet"=2,
#   "Egen enhet mot egen sykehustype" = 3,
#   "Egen sykehustype" = 4,
#   "Egen sykehustype mot resten av landet" = 5,
#   "Egen enhet mot egen region" = 6,
#   "Egen region" = 7,
#   "Egen region mot resten" = 8)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))
tidlOprValg <-	c('Alle'=99, 'Tidl. operert samme nivå'=1, 'Tidl. operert annet nivå'=2,
                   'Tidl. operert annet og sm. nivå'=3, 'Primæroperasjon'=4)
hastegradValg <- c('Alle' = 99, 'Elektiv' = 1, 'Akutt' = 2)
ktrValg <- c('3 mnd oppfølging' = 1, '12 mnd oppfølging' = 2)

sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Alle',sykehusNavn$x)

hovedkatValg <- c('Alle'=99, 'Andre inngrep'=0, 'Prolapskirurgi'=1, 'Midtlinjebevarende dekompr.'=2,
  'Laminektomi'=3, 'Eksp. intersp implantat'=4, 'Fusjonskirurgi'=5, 'Osteotomi, deformitet'=6,
  'Revisjon,fjerne implantat'=7, 'Skiveprotese'=8, 'Spinal stenose'=9, 'Degen. spondylolistese og LSS'=10)


# Define UI for application
ui <- navbarPage(id = "tab1nivaa",

  #title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle), # lag logo og tittel som en del av navbar. - Funker det med fluidPage?
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),# sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",



  #------------ Startside -----------------
  tabPanel(p("Startside", title='Oversikt over registreringer og resultater'),
           shinyjs::useShinyjs(),
           tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color#fluidRow(
           #column(width=5,
           h2('Velkommen til Rapporteket for NKR, Rygg!', align='center'),




           sidebarPanel(
             h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
             h4('I feltet til venstre på hver side kan man velge hvilken variabel man ønsker å se
                            resultater for. Der kan man også gjøre ulike filtreringer/utvalg av data.'),
             h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret.'),
             #h4(tags$b('Kvalitetsindikatorer '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
             h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.'),
             # h4(tags$b('Andeler: per sykehus og over tid'), ' viser andeler(prosent) per sykehus og utvikling over tid.
             #                Man kan velge hvilken tidsskala man vi se på.'),
             # h4(tags$b('Gjennomsnitt: per sykehus og over tid'), ' viser gjennomsnittsverdier per sykehus og utvikling over tid.
             #                Man kan velge om man vil se gjennomsnitt eller median.'),
             br(),
             br(),
             h3("Rapport med kvartalsresultater"),
             h5('Rapporten kan man også få regelmessig på e-post.
                        Gå til fanen "Abonnement" for å bestille dette.'),
             br(),
             downloadButton(outputId = 'mndRapp.pdf', label='Last ned kvartalsrapport', class = "butt"),
             tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
             br(),
             br(),
             #br(),
             #br(),
             #h4('Her Kan det komme nedlastbare dokumenter med samling av resultater'),
          br()
           ),
           mainPanel(
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
             rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                          organization = uiOutput("appOrgName")
                                          , addUserInfo = TRUE
             ),
             br(),
             h5('Her kan du se på figurer og tabeller som viser resultater fra registeret.
                            Du kan se på resultater for eget sykehus, nasjonale tall og eget sykehus sett opp
                              mot landet for øvrig. Resultatene som vises er
                              basert på operasjonsdato.
                              Alle resultater er basert på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                            Dette medfører at nyere data ikke er kvalitetssikret ennå.'),

             h5('Gi gjerne innspill og tilbakemeldinger til registerledelsen vedrørende
                            innhold på Rapporteket'),
             br(),
             #h2(paste("Drift og resultater, egen avdeling")), #,
             h2((uiOutput("egetShTxt"))),
             # fluidRow(
             #   h5('Registreringer siste år:'),
             #   tableOutput("tabAntOpphEget")
             # ),
             br(),
             fluidRow(
               column(4,
               h4('Antall skjema i kladd'),
               uiOutput("iKladdPas"),
               uiOutput("iKladdLege")
               #h5(paste('Pasientskjema:', uiOutput("iKladdPas"))),
               #h5(paste('Lengeskjema:', uiOutput("iKladdLege")))
             ),
             column(6,
                    h4('Registreringsforsinkelse'),
                    uiOutput('forSen3mnd'),
                    br(),
                    uiOutput('forSen12mnd')
             )),
             br(),
             br(),

             fluidRow(
               column(10,
                      h4(strong("Nøkkeltall")),
                      # selectInput(inputId = 'enhetsNivaaStart', label='Enhetsnivå',
                      #             choices = c("Egen enhet"=2, "Hele landet"=0,
                                              # "Egen sykehustype"=4, "Egen region"=7)),
                      tableOutput('tabNokkeltallStart')
           ))
           )#main
  ), #tab

  # #------------- Registreringsoversikter (vise antall)--------------------

  tabPanel(p('Registreringsoversikter', title="Tabeller med registreringsoversikter"),
           value = 'Registreringsoversikter',
           sidebarPanel(width=3,
                        h3('Valgmuligheter'),
                        conditionalPanel(condition = "input.ark == 'Antall operasjoner'",
                                         dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                   value = Sys.Date(), max = Sys.Date() )
                        ),
                        conditionalPanel(
                          condition = "input.ark == 'Antall operasjoner'",
                          selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                      choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                        conditionalPanel(
                          condition = "input.ark == 'Antall skjema'",
                          dateRangeInput(inputId = 'datovalgReg', start = startDato, end = Sys.Date(),
                                         label = "Tidsperiode", separator="t.o.m.", language="nb"),
                          selectInput(inputId = 'skjemastatus', label='Velg skjemastatus',
                                      choices = c("Ferdigstilt"=1,
                                                  "Kladd"=0,
                                                  "Åpen"=-1)
                          )
                        ),

                        br(),
                        br(),
                        h4('Last ned egne data'),
                        dateRangeInput(inputId = 'datovalgRegKtr', start = startDato, end = idag,
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
                        selectInput(inputId = 'velgReshReg', label='Velg sykehus',
                                    selected = 0,
                                    choices = sykehusValg),
                        downloadButton(outputId = 'lastNed_dataTilRegKtr', label='Last ned fødselsdato og operasjonsdato'),
                        br(),
                        br(),
                        downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump'),
                        h4('Datadumpen inneholder alle variabler fra begge versjoner')

           ),

           mainPanel(
             tabsetPanel(id='ark',
                         tabPanel('Antall operasjoner',
                                  uiOutput('OppsumAntReg'),
                                  br(),
                                  uiOutput("undertittelReg"),
                                  p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntOpphSh"),
                                    downloadButton(outputId = 'lastNed_tabAntOpphSh', label='Last ned tabell')
                                  )
                         ),
                         tabPanel('Antall skjema',
                                  h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                  p("Velg tidsperiode i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntSkjema"),
                                    downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned tabell')
                                  )
                                           # h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                                           # p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                           # tableOutput("tabAvdSkjema12"))
             )))
           ), #tab


#-------Registeradministrasjon----------
    tabPanel(p("Registeradministrasjon", title='Registrators side for registreringer og resultater'),
             value = "Registeradministrasjon",
             h3('Egen side for registeradministratorer. Siden er bare synlig for SC-bruker'),
             tabsetPanel(
               tabPanel('Utsending',
                 sidebarPanel(
                   h4('Lage abonnementslister for utsendinger'),
                   uiOutput("reportUts"),
                   uiOutput("freqUts"),
                   uiOutput("ReshUts"),
                   h5('E-postmottagere legges inn en og en. Trykk legg til e-postmottager for hver gang.
                           Når du har lagt til alle, trykker du på lag utsending. '),
                   textInput("email", "Epostmottakere:"),
                   uiOutput("editEmail"),
                   htmlOutput("recipients"),
                   tags$hr(),
                   uiOutput("makeDispatchment"), #utsending
                   br(),
                   br(),
                   h3('Last ned data fra versjon 2.0:'),
                   downloadButton(outputId = 'lastNed_dataV2', label='Last ned data V2'),
                   br(),
                   br(),
                   h4('Nedlasting av data til Resultatportalen:'),
                   h5('Fjernes eller erstattes av data til sykehusviser'),

                   selectInput(inputId = "valgtVarRes", label="Velg variabel",
                               choices = c('Lite beinsmerter før operasjon' = 'beinsmLavPre',
                                           'Durarift' = 'peropKompDura',
                                           'Utstrålende smerter i mer enn ett år' = 'sympVarighUtstr')
                   ),
                   selectInput(inputId = 'hovedInngrepRes', label='Hovedinngrepstype',
                               choices = hovedkatValg
                   ),selectInput(inputId = 'hastegradRes', label='Operasjonskategori (hastegrad)',
                                 choices = hastegradValg
                   ),
                   selectInput(inputId = 'tidlOpRes', label='Tidligere operert?',
                               choices = tidlOprValg
                   ),
                   # dateRangeInput(inputId = 'aarRes', start = startDato, end = Sys.Date(),
                   #                label = "Operasjonaår", separator="t.o.m.", language="nb", format = 'yyyy'
                   #                ),
                   sliderInput(inputId="aarRes", label = "Operasjonsår",
                               min = as.numeric(2007), max = as.numeric(year(idag)),
                               value = c(2018, year(idag)),
                               step=1, sep=""
                   ),
                   br(),
                   downloadButton(outputId = 'lastNed_dataTilResPort', label='Last ned data'),
                 ),

                 mainPanel(
                   h3('Oversikt over aktive utsendinger'),
                   uiOutput("dispatchmentContent"))
                 ), #Utsendingstab


                 shiny::tabPanel(
                   "Eksport",
                   #shiny::sidebarLayout(
                     shiny::sidebarPanel(
                       rapbase::exportUCInput("ryggExport")
                     ),
                     shiny::mainPanel(
                       rapbase::exportGuideUI("ryggExportGuide")
                     )
                   #)
                 ) #Eksport-tab
               ) #tabsetPanel
), #Registeradm-tab


#-------------Fordelinger---------------------
tabPanel(p('Fordelinger',
                    title='Alder, Innkomstmåte,... '),
                  sidebarPanel(
                    id = "brukervalg_fordeling",
                    width = 3,
                    h4('Her kan man velge hvilken variabel man ønsker å se og gjøre ulike filtreringer.'),
                    br(),
                    selectInput(
                      inputId = "valgtVar", label="Velg variabel",
                      choices = c('Alder' = 'alder',
                                  'Angst/depresjon (EQ5D) før operasjon' = 'EQangstPre',
                                  'Antibiotikaprofylakse?' = 'antibiotika',
                                  'Antibiotikatyper' = 'antibiotikaMedikament',
                                  'Arbeidsstatus' = 'arbstatus', #Velger skjema separat
                                  'Arbeidsstatus, 3 mnd. etter' = 'arbstatus3mnd',
                                  'Arbeidsstatus 12 mnd. etter' = 'arbstatus12mnd',
                                  'ASA-grad' = 'ASA',
                                  'BMI (Body Mass Index)' = 'BMI',
                                  'EQ5D, preoperativt' = 'EQ5DPre',
                                  'Gangfunksjon (EQ5D) før operasjon' = 'EQgangePre',
                                  'Fornoyd3mnd: Fornøydhet 3 mnd etter operasjon' = 'fornoydhet3mnd',
                                  'Fornoyd12mnd: Fornøydhet 12 mnd etter operasjon' = 'fornoydhet12mnd',
                                  'Hovedinngrep' = 'hovedInngrep',
                                  'Inngrepstyper' = 'inngrep',
                                  'Komorbiditet' = 'komorbiditet',
                                  'Komplikasjoner, perop. ' = 'komplPer' ,
                                  'Komplikasjoner, pasientrapp. ' = 'komplPost',
                                  'Liggetid ved operasjon, totalt' = 'liggedogn',
                                  'Liggetid, postoperativt' = 'liggetidPostOp',
                                  'Morsmål' = 'morsmal',
                                  'Nytte av operasjonen, 3 mnd. etter' = 'nytte3mnd',
                                  'Nytte av operasjonen, 12 mnd. etter' = 'nytte12mnd',
                                  'Operasjonsindikasjon' = 'opInd',
                                  'Operasjonsindikasjon, paresegrad' = 'opIndPareseGrad',
                                  #'Operasjonsindikasjon, smertetype' = 'opIndSmeType',
                                  'Operasjonskategori' = 'opKat',
                                  'Radiologisk undersøkelse' = 'radUnders',
                                  'Registreringsforsinkelse' = 'regForsinkelse',
                                  'Røyker du?' = 'roker',
                                  'Sårdren' = 'saardren',
                                  'Sivilstatus' = 'sivilStatus',
                                  'Smertestillende, bruk preop.' = 'smStiPre',
                                  'Smertestillende, hyppighet preop.' = 'smStiPreHypp',
                                  'Varighet av rygg-/hoftesmerter' = 'symptVarighRyggHof',
                                  'Varighet av utstrålende smerter' = 'sympVarighUtstr',
                                  'Tidligere ryggoperert?' = 'tidlOpr',
                                  'Tidligere operasjoner, antall' = 'tidlOprAntall',
                                  'Søkt erstatning?' = 'erstatningPre',
                                  'Søkt uføretrygd før operasjon' = 'uforetrygdPre',
                                  #Underkat: Fordeling av inngrepstyper. NB: hovedkategori MÅ velges
                                  'Trygg kirurgi-prosedyre utført' = 'tryggKir',
                                  'Utdanning (høyeste fullførte)' = 'utd',
                                  'Ventetid fra henvisning til time på poliklinikk' = 'ventetidHenvTimePol',
                                  'Ventetid fra operasjon bestemt til utført' = 'ventetidSpesOp'
                      ),
                      selected = c('Registreringsforsinkelse' = 'regForsinkelse')
                    ),
                    dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                                   label = "Tidsperiode (operasjonsdato)", separator="t.o.m.", language="nb"),
                    selectInput(inputId = "erMann", label="Kjønn",
                                choices = kjonn
                    ),
                    sliderInput(inputId="alder", label = "Alder", min = 0,
                                max = 110, value = c(0, 110)
                    ),
                    selectInput(inputId = 'hastegrad', label='Operasjonskategori (hastegrad)',
                                choices = hastegradValg
                    ),
                    selectInput(inputId = 'tidlOp', label='Tidligere operert?',
                                choices = tidlOprValg
                    ),
                    selectInput(inputId = 'hovedInngrep', label='Hovedinngrepstype',
                                choices = hovedkatValg
                    ),
                    selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                choices = enhetsUtvalg,
                    ),
                    selectInput(inputId = "bildeformatFord",
                                label = "Velg format for nedlasting av figur",
                                choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                    br(),
                    #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                    #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
                    actionButton("reset_fordValg", label="Tilbakestill valg"),
                    selectInput(inputId = 'velgReshFord', label='Velg eget Sykehus',
                                #selected = reshID,
                                choices = sykehusValg)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel(
                        'Figur',
                        h3('Fordelingsfigurer'),
                        h5('Høyreklikk på figuren for å laste den ned'),
                        plotOutput('fordelinger', height = 'auto'),
                      downloadButton('LastNedFigFord', label='Velg format (til venstre) og last ned figur')
                      ),
                      tabPanel(
                        'Tabell',
                        uiOutput("tittelFord"),
                        tableOutput('fordelingTab'),
                        downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
                      )
                    )
                  )
         ), #tab Fordelinger

#----------Andeler-----------------------------
tabPanel(p("Andeler: per sykehus og tid", title='Alder, antibiotika, ASA, fedme, gjennomføringsgrad, komplikasjoner,
           konvertering, oppfølging, registreringsforsinkelse, komplikasjoner, TSS2, utdanning'),
         h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
         h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
            til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
         br(),
         sidebarPanel(
           width=3,
           h3('Utvalg'),

           selectInput(
             inputId = "valgtVarAndel", label="Velg variabel",
             choices = c('Kval.ind: For sen registrering' = 'regForsinkelse',
                         'Kval.ind: Lite beinsmerter, ingen parese' = 'smBePreLav',
                         'Kval.ind: Trygg kirurgi-prosedyre utført' = 'tryggKir',
                         'Kval.ind: Varighet av utstrålende smerter >1 år' = 'sympVarighUtstr',
                         'Kval.ind: Ventetid < 3 mnd. fra op. bestemt til utført' = 'ventetidSpesOp',
                         'Alder over 70 år' = 'alder70',
                         'Antibiotika' = 'antibiotika',
                         'Arbeidsstatus' = 'arbstatus',
                         'ASA-grad > II' = 'ASA',
                         'Degen. spondy. op. m/fusjon' = 'degSponFusj',
                         'Fedme (BMI>30)' = 'BMI',
                         'Flere enn to tidligere operasjoner' = 'tidlOp3',
                         'Forbedring av Oswestry-skår >= 20p' = 'OswEndr20',
                         'Fornøyde pasienter' = 'fornoydhet',
                         'Fremmedspråklig' = 'morsmal',
                         # 'Komplikasjoner, pasientrapportert' = 'kp3Mnd',
                         'Har degen. spondy. og spin.stenose' = 'degSponSSSten',
                         'Helt bra eller mye bedre' = 'nytte',
                         'Høyere utdanning' = 'utd',
                         'Komplikasjoner ved operasjon' = 'peropKomp',
                         'Komplikasjon ved op.: Durarift' = 'peropKompDura',
                         'Misfornøyde pasienter' = 'misfornoyd',
                         'Minst 30% forbedring av Oswestry-skår' = 'OswEndr30pst',
                         'Mye verre/verre enn noen gang' = 'verre',
                         'Oswestry-skår < 23 poeng' = 'Osw22',
                         'Oswestry-skår > 48 poeng' = 'Osw48',
                         'Røykere' = 'roker',
                         'Smertestillende før operasjon' = 'smStiPre',
                         'Varighet av rygg-/hoftesmerter >1 år' = 'symptVarighRyggHof',
                        'Søkt erstatning før operasjon' = 'erstatningPre',
                        'Søkt uføretrygd før operasjon' = 'uforetrygdPre',
                        'Tromboseprofylakse gitt ifm. operasjon' = 'trombProfyl',
                        'Ventetid fra henvisning til time på poliklinikk' = 'ventetidHenvTimePol'
             )
           ),
           #uiOutput("datovalgAndel"),
           dateRangeInput(inputId = 'datovalgAndel', start = startDato, end = idag,
                          label = "Tidsperiode", separator="t.o.m.", language="nb"),
           selectInput(inputId = 'ktrAndel', label='Oppfølgingsskjema',
                       choices = ktrValg
           ),

           selectInput(inputId = "erMannAndel", label="Kjønn",
                       choices = kjonn
           ),
           sliderInput(inputId="alderAndel", label = "Alder", min = 0,
                       max = 110, value = c(0, 110)),
           selectInput(inputId = 'hastegradAndel', label='Operasjonskategori (hastegrad)',
                       choices = hastegradValg
           ),
           selectInput(inputId = 'tidlOpAndel', label='Tidligere operert?',
                       choices = tidlOprValg
           ),
           selectInput(inputId = 'hovedInngrepAndel', label='Hovedinngrepstype',
                       choices = hovedkatValg
           ),
           selectInput(inputId = "bildeformatAndel",
                       label = "Velg format for nedlasting av figur",
                       choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
           br(),
           br(),
           p(em('Følgende utvalg gjelder bare figuren/tabellen som viser utvikling over tid')),
           selectInput(inputId = 'enhetsUtvalgAndel', label='Egen enhet og/eller landet',
                       choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
           selectInput(inputId = "tidsenhetAndel", label="Velg tidsenhet",
                       choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                       'Kvartal'='Kvartal', 'Måned'='Mnd')))
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("Figurer",
                      #column(10,
                      h3(em("Utvikling over tid")),
                      br(),
                      plotOutput("andelTid", height = 'auto'),
                      downloadButton('LastNedFigAndelTid', label='Velg format (til venstre) og last ned figur'),
                      br(),
                      h3(em("Sykehusvise resultater")),
                      plotOutput("andelerGrVar", height='auto'),
             downloadButton('LastNedFigAndelGrVar', label='Velg format (til venstre) og last ned figur')
             ),
             tabPanel("Tabeller",
                      uiOutput("tittelAndel"),
                      br(),
                      #fluidRow(
                      column(width = 4,
                             h3("Sykehusvise resultater"),
                             tableOutput("andelerGrVarTab"),
                             downloadButton(outputId = 'lastNed_tabAndelGrVar', label='Last ned tabell')),
                      column(width = 1),
                      column(width = 6,
                             h3("Utvikling over tid"),
                             tableOutput("andelTidTab"),
                             downloadButton(outputId = 'lastNed_tabAndelTid', label='Last ned tabell'))
                      #DT::DTOutput("andelerGrVarTab")
             ))
         ) #mainPanel

), #tab

#------------------Abonnement-------------------------

tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av rapporter på e-post'),
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("subscriptionRep", "Rapport:",
                                    c("Kvartalsrapport")), #, "Samlerapport", "Influensaresultater")),
                        selectInput("subscriptionFreq", "Frekvens:",
                                    list(Årlig="Årlig-year",
                                          Kvartalsvis="Kvartalsvis-quarter",
                                          Månedlig="Månedlig-month",
                                          Ukentlig="Ukentlig-week",
                                          Daglig="Daglig-DSTday"),
                                    selected = "Månedlig-month"),
                        #selectInput("subscriptionFileFormat", "Format:",
                        #            c("html", "pdf")),
                        actionButton("subscribe", "Bestill!")
           ),
           mainPanel(
             uiOutput("subscriptionContent")
           )
         )
)



) #fluidpage, dvs. alt som vises på skjermen


#----------------- Define server logic required  -----------------------
server <- function(input, output,session) {

  rapbase::appLogger(session, msg = 'Starter Rapporteket-Rygg')
  #reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)),
  #                           601161)})
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 601161)
  output$reshID <- renderText(reshID)
  #rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'LU')
  output$rolle <- renderText(rolle)
  brukernavn <- ifelse(paaServer, rapbase::getUserName(session), 'inkognito')
  output$egetShnavn <- renderText(as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))
  output$egetShTxt <- renderText(paste('Drift og resultater, ',
                                       as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])))

  observe({if (rolle != 'SC') { #
    shinyjs::hide(id = 'velgReshReg')
    shinyjs::hide(id = 'velgReshFord')
    shinyjs::hide(id = 'lastNed_dataDump')
    #hideTab(inputId = "tabs", target = "Foo")
    shiny::hideTab(inputId = "tab1nivaa",
                   target = 'Registeradministrasjon') #
  }
  })

  observeEvent(input$reset_fordValg, shinyjs::reset("brukervalg_fordeling"))

  #observeEvent(input$reset_andelValg, shinyjs::reset("brukervalg_andeler"))
  #observeEvent(input$reset_gjsnValg, shinyjs::reset("brukervalg_gjsn"))

  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle, '<br> ReshID: ', reshID) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  #------ Dæsjbord ---------------------

  # output$... <- renderTable()
  vec <- factor(SkjemaOversikt$SkjemaRekkeflg, levels= c(5,10))
  iKladd <- table(vec[Reduce(intersect, list(#which(as.Date(SkjemaOversikt$InnDato) >= datofra12),
                                               which(SkjemaOversikt$SkjemaStatus==0)
                                             ,which(SkjemaOversikt$AvdRESH == reshID)
                                             ))]) #, indSkjema
  names(iKladd) <- c('Pasientskjema','Legeskjema')
  output$iKladdPas <- renderText(paste('Pasientskjema: ', iKladd[1]))
  output$iKladdLege <- renderPrint(iKladd[2])



  output$forSen3mnd <- renderText(paste0('<b>',forsinketReg(RegData=RegData, fraDato=Sys.Date()-400,
                                                      tilDato=Sys.Date()-100, forsinkelse=100, reshID=reshID),'</b>',
                                  ' skjema ferdigstilt for sent for 3 mnd.ktr i perioden ',
                                         format.Date(Sys.Date()-400, '%d.%b%Y'), '-', format.Date(Sys.Date()-100, '%d.%b%Y'))
                                         )
  output$forSen12mnd <- renderText(paste0('<b>', forsinketReg(RegData=RegData, fraDato=max(as.Date('2019-01-01'),Sys.Date()-745),
                                                       tilDato=Sys.Date()-380, forsinkelse=380, reshID=reshID), '</b>',
                                          ' skjema ferdigstilt for sent for 12 mnd.ktr i perioden ',
                                          format.Date(max(as.Date('2019-01-01'),Sys.Date()-745), '%d.%b%Y'), '-', format.Date(Sys.Date()-380, '%d.%b%Y')))
  output$tabNokkeltallStart <- function() {
    tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd', reshID=reshID)) #enhetsUtvalg=as.numeric(input$enhetsNivaaStart),
    kableExtra::kable(tab,
                      full_width=F,
                      digits = c(0,0,1,1,1,1,0)
    ) %>%
      column_spec(column = 1, width_min = '5em', width_max = '10em') %>%
      column_spec(column = 2:(ncol(tab)), width = '7em')  %>%
      row_spec(0, bold = T, align = 'c') %>%
      kable_styling(full_width = FALSE, position = 'left') #"hover",
  }

    output$tabAntOpphEget <- renderTable(
      tabAntOpphShMnd(RegData=RegData, datoTil=datoTil, reshID = reshID, antMnd=12)
      ,rownames = T, digits=0, spacing="xs" )

    #-------Samlerapporter--------------------

    output$mndRapp.pdf <- downloadHandler(
      filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporter(file, rnwFil="RyggMndRapp.Rnw",
                            reshID = reshID, datoFra = startDato)
      }
    )



#------Registreringsoversikter---------------------
  observe({
    output$OppsumAntReg <- renderUI({
      Registreringer <- RyggUtvalgEnh(RegData=RegData, datoFra = input$datovalgRegKtr[1], datoTil=input$datovalgRegKtr[2])$RegData[,'PasientID']
      antallReg <- length(Registreringer)
      antallPers <- length(unique(Registreringer))
      HTML(paste0('<b> I perioden ',format.Date(input$datovalgRegKtr[1], '%d. %B %Y'), ' - ', format.Date(input$datovalgRegKtr[2], '%d. %B %Y'),
             ' er det totalt registrert ', antallReg, ' operasjoner. Disse er utført på tilsammen ',
             antallPers, ' personer.', '</b>' ))})


    tabAntOpphSh <- switch(input$tidsenhetReg,
           Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
           Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))

    output$tabAntOpphSh <- renderTable(tabAntOpphSh, rownames = T, digits=0, spacing="xs")
    output$lastNed_tabAntOpphSh <- downloadHandler(
      filename = function(){'tabAntOpphSh.csv'},
      content = function(file, filename){write.csv2(tabAntOpphSh, file, row.names = T, fileEncoding = 'latin1', na = '')})

  output$undertittelReg <- renderUI({
    br()
    valgtAar <- as.numeric(format.Date(input$sluttDatoReg, "%Y"))
    t1 <- 'Tabellen viser operasjoner '
    h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                   Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                   Aar = paste0(t1, 'per år til og med ', valgtAar, '<br />'))
    ))})

  #RegData som har tilknyttede skjema av ulik type. Fra NGER!
  AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaOversikt, datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                  skjemastatus=as.numeric(input$skjemastatus))
  tabAntSkjema(SkjemaOversikt)

  output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                     ,rownames = T, digits=0, spacing="xs" )
  output$lastNed_tabAntSkjema <- downloadHandler(
    filename = function(){'tabAntSkjema.csv'},
    content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, fileEncoding = 'latin1', na = '')})
    })


  # #Velge ferdigstillelse og tidsintervall.
  # output$tabAntSkjema <- renderTable({})

# Hente oversikt over hvilke registrereinger som er gjort (opdato og fødselsdato)
  tilretteleggDataDumper <- function(data, datovalg, reshID, rolle){
    data <- dplyr::filter(data,
                          as.Date(InnDato) >= datovalg[1],
                          as.Date(InnDato) <= datovalg[2])
    if (rolle == 'SC') {
      PIDtab <- rapbase::loadRegData(registryName="rygg", query='SELECT * FROM koblingstabell')
      data <- merge(data, PIDtab, by.x = 'PasientID', by.y = 'ID', all.x = T)
      valgtResh <- as.numeric(reshID)
      ind <- if (valgtResh == 0) {1:dim(data)[1]
      } else {which(as.numeric(data$ReshId) %in% as.numeric(valgtResh))}
      data <- data[ind,]
    } else {
      data <- data[which(data$ReshId == reshID), ]}
    return(data)
  }

  observe({
    reshKtr <- ifelse(rolle=='SC', input$velgReshReg, reshID )
    indKtr <- if (reshKtr == 0) {1:dim(RegOversikt)[1]} else {which(RegOversikt$ReshId == reshKtr)}
    uiOutput({

    })
    dataRegKtr <- dplyr::filter(RegOversikt[indKtr, ],
                                as.Date(InnDato) >= input$datovalgRegKtr[1],
                                as.Date(InnDato) <= input$datovalgRegKtr[2])

  output$lastNed_dataTilRegKtr <- downloadHandler(
    filename = function(){'dataTilKtr.csv'},
    content = function(file, filename){write.csv2(dataRegKtr, file, row.names = F, fileEncoding = 'latin1', na = '')})


  variablePRM <- 'variable som skal fjernes hvis lastes ned av avdeling'
  #Foreløpig ikke def siden oppf.skjema ikke med i dump. Dump bare for SC.
  # observe({
  #   DataDump <- dplyr::filter(RegData,
  #                             as.Date(HovedDato) >= input$datovalgRegKtr[1],
  #                             as.Date(HovedDato) <= input$datovalgRegKtr[2])
  #   tabDataDump <- if (rolle == 'SC') {
  #     valgtResh <- as.numeric(input$velgReshReg)
  #     ind <- if (valgtResh == 0) {1:dim(DataDump)[1]
  #     } else {which(as.numeric(DataDump$ReshId) %in% as.numeric(valgtResh))}
  #     DataDump[ind,]
  #   } #else {
  #     #DataDump[which(DataDump$ReshId == reshID), -variablePRM]} #Tar bort PROM/PREM til egen avdeling

  RegDataV2V3 <- RyggRegDataSQLV2V3(alleVarV2=1)
    #rapbase::loadRegData(registryName="rygg", query='SELECT * FROM AlleVarNum')
  RegDataV2V3 <- RyggPreprosess(RegDataV2V3)
  dataDump <- tilretteleggDataDumper(data=RegDataV2V3, datovalg = input$datovalgRegKtr,
                                     reshID=input$velgReshReg, rolle = rolle)
  output$lastNed_dataDump <- downloadHandler(
      filename = function(){'dataDump.csv'},
      content = function(file, filename){write.csv2(dataDump, file, row.names = F, fileEncoding = 'latin1', na = '')})

  dataDumpV2 <- rapbase::loadRegData(registryName="rygg",
                                     query='select * FROM Uttrekk_Rapport_FROM_TORE', dbType="mysql")
  output$lastNed_dataV2 <- downloadHandler(
    filename = function(){'dataDumpV2.csv'},
    content = function(file, filename){write.csv2(dataDump, file, row.names = F, fileEncoding = 'latin1', na = '')})
  })
#-----------Registeradministrasjon-----------

  if (rolle=='SC') {
  observe({
    tabdataTilResPort <- dataTilOffVisning(RegData=RegData, valgtVar = input$valgtVarRes,
                                        hovedkat = as.numeric(input$hovedInngrepRes),
                                        aar=as.numeric(input$aarRes[1]):as.numeric(input$aarRes[2]),
                                        hastegrad = input$hastegradRes, tidlOp = input$tidlOpRes)

    output$lastNed_dataTilResPort <- downloadHandler(
      filename = function(){'dataTilResPort.csv'},
      content = function(file, filename){write.csv2(tabdataTilResPort, file, row.names = T, fileEncoding = 'latin1', na = '')})


  })
  }

  #----------- Eksport ----------------
  registryName <- "rygg"
  ## brukerkontroller
  rapbase::exportUCServer("ryggExport", registryName)
  ## veileding
  rapbase::exportGuideServer("ryggExportGuide", registryName)





#------------Fordelinger---------------------

  observeEvent(input$reset, {
    shinyjs::reset("enhetsUtvalg")
    shinyjs::reset("datovalg")
    shinyjs::reset("hastegrad")
    shinyjs::reset("erMann")
    shinyjs::reset("alder")
  })
  output$fordelinger <- renderPlot({
    reshIDford <- ifelse(rolle=='SC', input$velgReshFord, reshID)
    RyggFigAndeler(RegData=RegData, preprosess = 0,
                   valgtVar=input$valgtVar,
                  reshID=reshIDford,
                   enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                   datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                   erMann=as.numeric(input$erMann),
                   hastegrad = as.numeric(input$hastegrad),
                   tidlOp = as.numeric(input$tidlOp),
                  hovedkat = as.numeric(input$hovedInngrep),
                  session = session)
  }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  )

  observe({
    reshIDford <- ifelse(rolle=='SC', input$velgReshFord, reshID)
    UtDataFord <- RyggFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                reshID=reshIDford, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                erMann=as.numeric(input$erMann),
                                hastegrad = as.numeric(input$hastegrad),
                                tidlOp = as.numeric(input$tidlOp),
                                hovedkat = as.numeric(input$hovedInngrep),
                                lagFig = 0, session = session)

    tabFord <- lagTabavFig(UtDataFraFig = UtDataFord)

    output$tittelFord <- renderUI({
      tagList(
        h3(HTML(paste0(UtDataFord$tittel, '<br />'))),
        #h3(UtDataFord$tittel),
        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
      )}) #, align='center'

    output$LastNedFigFord <- downloadHandler(
      filename = function(){
        paste0('FordelingsFig_', valgtVar=input$valgtVar, '_', Sys.Date(), '.', input$bildeformatFord)
      },
      content = function(file){
        RyggFigAndeler(RegData=RegData, preprosess = 0,
                         valgtVar=input$valgtVar,
                         reshID=reshIDford,
                         enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                         datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                         minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                         erMann=as.numeric(input$erMann),
                         hastegrad = as.numeric(input$hastegrad),
                         tidlOp = as.numeric(input$tidlOp),
                         hovedkat = as.numeric(input$hovedInngrep),
                         session = session,
                         outfile = file)
      })

    kolGruppering <- c(1,3,3)
    names(kolGruppering) <- c(' ', UtDataFord$hovedgrTxt, UtDataFord$smltxt)
    output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
      antKol <- ncol(tabFord)
      kableExtra::kable(tabFord, format = 'html'
                        , full_width=F
                        , digits = c(0,0,1,0,0,1)[1:antKol]
      ) %>%
        add_header_above(kolGruppering[1:(2+UtDataFord$medSml)]) %>%
        #add_header_above(c(" "=1, tittelKolGr[1] = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        column_spec(column = 1, width='5em') %>% #width_min = '3em', width_max = '10em') %>%
        column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
        row_spec(0, bold = T)
    }

    output$lastNed_tabFord <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_fordeling.csv')
      },
      content = function(file, filename){
        write.csv2(tabFord, file, row.names = F, fileEncoding = 'latin1', na = '')
      })
  }) #observe


#--------------Andeler-----------------------------------
  observe({
  #   minDato <- ifelse(input$ktrAndel == '2', min(as.Date(startDato), Sys.Date()-365*2), startDato)
  #   output$datovalgAndel <- renderUI({
  #
  #   #selectInput("User", "Date:", choices = as.character(dat5[dat5$email==input$Select,"date"]))
  #   dateRangeInput(inputId = 'datovalgAndel', start = as.Date(minDato), end = idag,
  #                  label = "Tidsperiode", separator="t.o.m.", language="nb")
  # })

  output$andelerGrVar <- renderPlot({
    RyggFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                        datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                        minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                        ktr = as.numeric(input$ktrAndel),
                        erMann=as.numeric(input$erMannAndel),
                        hastegrad = as.numeric(input$hastegradAndel),
                        tidlOp = as.numeric(input$tidlOpAndel),
                        hovedkat = as.numeric(input$hovedInngrepAndel),
                        session=session)
  }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
  )

  output$LastNedFigAndelGrVar <- downloadHandler(
    filename = function(){
      paste0('AndelTid_', valgtVar=input$valgtVarAndel, '_', Sys.Date(), '.', input$bildeformatAndel)
    },
    content = function(file){
      RyggFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                          datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                          minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                          ktr = as.numeric(input$ktrAndel),
                          erMann=as.numeric(input$erMannAndel),
                          hastegrad = as.numeric(input$hastegradAndel),
                          tidlOp = as.numeric(input$tidlOpAndel),
                          hovedkat = as.numeric(input$hovedInngrepAndel),
                          session=session,
                      outfile = file)
    })

  output$andelTid <- renderPlot({

    RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                    reshID= reshID,
                    datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                    minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                    ktr = as.numeric(input$ktrAndel),
                    erMann=as.numeric(input$erMannAndel),
                    hastegrad = as.numeric(input$hastegradAndel),
                    tidlOp = as.numeric(input$tidlOpAndel),
                    hovedkat = as.numeric(input$hovedInngrepAndel),
                    tidsenhet = input$tidsenhetAndel,
                    enhetsUtvalg = input$enhetsUtvalgAndel,
                    session=session)
  }, height = 300, width = 1000
  )

    #AndelTid
    AndelerTid <- RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                  reshID= reshID,
                                  datoFra=as.Date(input$datovalgAndel[1]), datoTil=input$datovalgAndel[2],
                                  minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                  ktr = as.numeric(input$ktrAndel),
                                  erMann=as.numeric(input$erMannAndel),
                                  hastegrad = as.numeric(input$hastegradAndel),
                                  tidlOp = as.numeric(input$tidlOpAndel),
                                  hovedkat = as.numeric(input$hovedInngrepAndel),
                                  enhetsUtvalg = input$enhetsUtvalgAndel,
                                  tidsenhet = input$tidsenhetAndel,
                                  session=session) #,lagFig=0)
    tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid, figurtype = 'andelTid')


    output$andelTidTab <- function() {
      antKol <- ncol(tabAndelTid)
      kableExtra::kable(tabAndelTid, format = 'html'
                        , full_width=F
                        , digits = c(0,0,1,0,0,1)[1:antKol]
      ) %>%
        add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        column_spec(column = 1, width_min = '7em') %>%
        column_spec(column = 2:(antKol+1), width = '7em') %>%
        row_spec(0, bold = T)
    }
    output$lastNed_tabAndelTid <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_andelTid.csv')
      },
      content = function(file, filename){
        write.csv2(tabAndelTid, file, row.names = T, fileEncoding = 'latin1', na = '')
      })

    output$LastNedFigAndelTid <- downloadHandler(
      filename = function(){
        paste0('AndelTid_', valgtVar=input$valgtVarAndel, '_', Sys.Date(), '.', input$bildeformatAndel)
      },
      content = function(file){
        RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                        reshID= reshID,
                        datoFra=as.Date(input$datovalgAndel[1]), datoTil=input$datovalgAndel[2],
                        minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                        ktr = as.numeric(input$ktrAndel),
                        erMann=as.numeric(input$erMannAndel),
                        hastegrad = as.numeric(input$hastegradAndel),
                        tidlOp = as.numeric(input$tidlOpAndel),
                        hovedkat = as.numeric(input$hovedInngrepAndel),
                        enhetsUtvalg = input$enhetsUtvalgAndel,
                        tidsenhet = input$tidsenhetAndel,
                        session=session,
                       outfile = file)
      })


    #AndelGrVar
    AndelerShus <- RyggFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                       datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                       minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                       ktr = as.numeric(input$ktrAndel),
                                       erMann=as.numeric(input$erMannAndel),
                                       hastegrad = as.numeric(input$hastegradAndel),
                                       tidlOp = as.numeric(input$tidlOpAndel),
                                       hovedkat = as.numeric(input$hovedInngrepAndel),
                                       session=session) #, lagFig = 0))

    tabAndelerShus <- cbind('Antall (n)' = round(AndelerShus$Ngr*AndelerShus$AggVerdier/100),
                            'Antall (N)' = AndelerShus$Ngr,
                            Andeler = AndelerShus$AggVerdier)

    output$andelerGrVarTab <- function() {
      antKol <- ncol(tabAndelerShus)
      kableExtra::kable(tabAndelerShus, format = 'html'
                        #, full_width=T
                        , digits = c(0,0,1) #,0,1)[1:antKol]
      ) %>%
        #column_spec(column = 1, width_min = '5em') %>%
        column_spec(column = 1:(antKol+1), width = '5em') %>%
        row_spec(0, bold = T)
    }
    output$lastNed_tabAndelGrVar <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_andelGrVar.csv')
      },
      content = function(file, filename){
        write.csv2(tabAndelerShus, file, row.names = T, fileEncoding = 'latin1', na = '')
      })

    output$tittelAndel <- renderUI({
      tagList(
        h3(AndelerShus$tittel),
        h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
      )}) #, align='center'
  }) #observe


    #------------------ Abonnement ----------------------------------------------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  # rv <- reactiveValues(
  #   subscriptionTab = rapbase::makeUserSubscriptionTab(session))
  #
  #
  # ## lag tabell over gjeldende status for abonnement
  # output$activeSubscriptions <- DT::renderDataTable(
  #   rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
  #   rownames = FALSE, options = list(dom = 't')
  # )

  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  subscription <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session, type = "subscription"))
  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$tab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = 6))), #Fjerner kolonne
    rownames = FALSE
  )


  ## lag side som viser status for abonnement, også når det ikke finnes noen
  # output$subscriptionContent <- renderUI({
  #   fullName <- rapbase::getUserFullName(session)
  #   if (length(rv$subscriptionTab) == 0) {
  #     p(paste("Ingen aktive abonnement for", fullName))
  #   } else {
  #     tagList(
  #       p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
  #               rapbase::getUserEmail(session), ":")),
  #       DT::dataTableOutput("activeSubscriptions")
  #     )
  #   }
  # })
  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(subscription$tab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
                 ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  observeEvent (input$subscribe, { #MÅ HA
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
    email <- rapbase::getUserEmail(session)
    if (input$subscriptionRep == "Kvartalsrapport") {
      synopsis <- "rygg/Rapporteket: kvartalsrapport"
      rnwFil <- "RyggMndRapp.Rnw" #Navn på fila
      #print(rnwFil)
    }

    fun <- "abonnementRygg"  #"henteSamlerapporter"
    paramNames <- c('rnwFil', 'brukernavn', "reshID")
    paramValues <- c(rnwFil, brukernavn, reshID) #input$subscriptionFileFormat)

    #abonnementRygg(rnwFil = 'RyggMndRapp.Rnw', brukernavn='hei', reshID=601161, datoTil=Sys.Date())

    rapbase::createAutoReport(synopsis = synopsis, package = 'rygg',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName)
    #rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
  })

  # ## slett eksisterende abonnement
  # observeEvent(input$del_button, {
  #   selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
  #   rapbase::deleteAutoReport(selectedRepId)
  #   rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  # })


  #----- Utsending -----------------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  dispatchment <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session = session, type = "dispatchment"),
    report = "RyggMndRapp",
    freq = "Månedlig-month",
    email = vector()
  )
  ## observér og foreta endringer mens applikasjonen kjører
  observeEvent(input$addEmail, {
    dispatchment$email <- c(dispatchment$email, input$email)
  })
  observeEvent(input$delEmail, {
    dispatchment$email <-
      dispatchment$email[!dispatchment$email == input$email]
  })
  observeEvent (input$dispatch, {
    package <- "rygg"
    type <- "dispatchment"
    owner <- rapbase::getUserName(session)
    ownerName <- rapbase::getUserFullName(session)
    interval <- strsplit(input$dispatchmentFreq, "-")[[1]][2]
    intervalName <- strsplit(input$dispatchmentFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- dispatchment$email

    # fun <- "abonnementRygg"  #"henteSamlerapporter"

    if (input$dispatchmentRep == "Kvartalsrapport") {
      synopsis <- "Kvartalsrapport, Rygg"
      fun <- "abonnementRygg"
      rnwFil <- "RyggMndRapp.Rnw" #Navn på fila
      reshIDuts <- input$dispatchmentResh
      organization <- reshIDuts #rapbase::getUserReshId(session)
      #print(reshIDuts)
      indReshUts <- match(reshIDuts, RegData$ReshId) #Velger sykehusresh
      paramNames <- c('rnwFil', 'brukernavn', "reshID")
      paramValues <- c(rnwFil, brukernavn, reshIDuts)
    }

    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              ownerName = ownerName,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    dispatchment$tab <- rapbase::makeAutoReportTab(session, type = "dispatchment")
    test <- dimnames(dispatchment$tab)
    # print(test[[]])
    # print(attributes(dispatchment$tab))
    #Author DataFlair

    alleAutorapporter <- rapbase::readAutoReportData()
    egneUts <-  rapbase::filterAutoRep(
      rapbase::filterAutoRep(alleAutorapporter, by = 'package', pass = 'rygg'),
      by = 'type', pass = 'dispatchment')

    # ider <- names(egneUts)
    # roller <- egneUts[[1]][['params']][[6]]$rolle
    # for (k in 2:length(ider)) {
    #   roller <- c(roller, egneUts[[k]][['params']][[6]]$rolle)
    # }
    # koblRoller <- cbind(ID = ider,
    #                     roller = roller)

    dispatchment$email <- vector()
  })


  ## ui: velg rapport
  output$reportUts <- renderUI({
    selectInput("dispatchmentRep", "Rapport:",
                c("Kvartalsrapport"),
                selected = dispatchment$report)
  })
  ## ui: velg rolle
  # output$rolleUts <- renderUI({
  #   selectInput("dispatchmentRole", "Rolle/nivå:",
  #               c("LU", "LC", "SC"),
  #               selected = dispatchment$rolle)
  # })
  ## ui: velg enhet
  output$ReshUts <- renderUI({
    selectInput("dispatchmentResh", "Avdelingstilhørighet:",
                sykehusValg[-1],
                selected = dispatchment$Resh)
  })

  ## ui: velg frekvens
  output$freqUts <- renderUI({
    selectInput("dispatchmentFreq", "Frekvens:",
                list(Årlig = "Årlig-year",
                      Kvartalsvis = "Kvartalsvis-quarter",
                      Månedlig = "Månedlig-month",
                      Ukentlig = "Ukentlig-week",
                      Daglig = "Daglig-DSTday"),
                selected = dispatchment$freq)
  })

  ## ui: legg til gyldig- og slett epost
  output$editEmail <- renderUI({
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
               input$email)) {
      tags$p("Angi mottaker over")
    } else {
      if (input$email %in% dispatchment$email) {
        actionButton("delEmail", "Slett epostmottaker",
                     icon = shiny::icon("trash"))
      } else {
        actionButton("addEmail", "Legg til epostmottaker",
                     icon = shiny::icon("pencil"))
      }
    }
  })

  ## ui: vis valgte mottakere
  output$recipients <- renderText(paste(dispatchment$email, sep = "<br>"))

  ## ui: lag ny utsending
  output$makeDispatchment <- renderUI({
    if (length(dispatchment$email) == 0) {
      NULL
    } else {
      actionButton("dispatch", "Lag utsending",
                   icon = shiny::icon("save"))
    }
  })

  ## lag tabell over gjeldende status for utsending
  output$activeDispatchments <- DT::renderDataTable(
    dispatchment$tab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE, columnDefs = list(list(visible = FALSE, targets = 9))),
    rownames = FALSE
  )

  ## ui: lag side som viser status for utsending, også når det ikke finnes noen
  output$dispatchmentContent <- renderUI({
    if (length(dispatchment$tab) == 0) {
      p("Det finnes ingen utsendinger")
    } else {
      tagList(
        h4("Aktive utsendinger:"),
        h5("Når du trykker på knappen for å gjøre endringer i ei utsending,
           slettes utsendinga fra lista og alle valg UNNTATT sykehustilhørighet/resh legger seg inn i skjemaet til venstre
           slik at du f.eks. kan legge til/slette e-postmottagere og endre frekvens."),
        DT::dataTableOutput("activeDispatchments")
      )
    }
  })

  # Rediger eksisterende auto rapport (alle typer)
  observeEvent(input$edit_button, {
    repId <- strsplit(input$edit_button, "_")[[1]][2]
    rep <- rapbase::readAutoReportData()[[repId]]
    if (rep$type == "subscription") {#abonnement

    }
    if (rep$type == "dispatchment") { #utsending
      dispatchment$freq <- paste0(rep$intervalName, "-", rep$interval)
      dispatchment$email <- rep$email
      rapbase::deleteAutoReport(repId)
      dispatchment$tab <-
        rapbase::makeAutoReportTab(session, type = "dispatchment")
      dispatchment$report <- rep$synopsis
    }
    if (rep$type == "bulletin") {

    }
  })


  ## slett eksisterende abonnement
  # observeEvent(input$del_button, {
  #   selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
  #   rapbase::deleteAutoReport(selectedRepId)
  #   rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)})

  # Slett eksisterende auto rapport (alle typer)
  observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(repId)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment")
  })




} #server
# Run the application
shinyApp(ui = ui, server = server)


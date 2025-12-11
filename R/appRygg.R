#Resultattjeneste for Degenerativ Rygg


#' Brukergrensesnitt (ui) til rygg-appen
#'
#' @return Brukergrensesnittet (ui) til rygg-appen
#' @export
ui_rygg <- function() {

library(rygg)
idag <- Sys.Date()
startDato <- paste0(as.numeric(format(idag-180, "%Y")), '-01-01')
datofra12 <- lubridate::floor_date(as.Date(idag)- months(12, abbreviate = T), unit='month')

regTitle = 'NKR: Nasjonalt kvalitetsregister for ryggkirurgi'

#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))
tidlOprValg <-	c('Alle'=99, 'Tidl. operert samme nivå'=1, 'Tidl. operert annet nivå'=2,
                 'Tidl. operert annet og sm. nivå'=3, 'Primæroperasjon'=4)
hastegradValg <- c('Alle' = 99, 'Elektiv' = 1, 'Akutt' = 2)
ktrValg <- c('3 mnd oppfølging' = 1, '12 mnd oppfølging' = 2)


hovedkatValg <- c('Alle'=99,
                  'Andre inngrep'=0,
                  'Prolapskirurgi'=1,
                  'Midtlinjebevarende dekompr.'=2,
                  'Laminektomi'=3,
                  'Eksp. intersp implantat'=4,
                  'Fusjonskirurgi'=5,
                  'Osteotomi, deformitet'=6,
                  'Revisjon,fjerne implantat'=7,
                  'Skiveprotese'=8, 'Spinal stenose'=9,
                  'Degen. spondylolistese og LSS'=10)


# Define UI for application
#pdf(file = NULL)

ui <- navbarPage(
  id = "hovedark",

  title = rapbase::title(regTitle),
  windowTitle = regTitle,
  theme = rapbase::theme(),


  #------------ Startside -----------------
  tabPanel(p("Startside", title='Oversikt over registreringer og resultater'),
           shinyjs::useShinyjs(),
           tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color#fluidRow(
           h2('Velkommen til Rapporteket for NKR, Rygg!', align='center'),
          #  h6('Versjon 11.aug 2025', col='lightgrey'),


           sidebarPanel(
             h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
             h4('I feltet til venstre på hver side kan man velge hvilken variabel man ønsker å se
                            resultater for. Der kan man også gjøre ulike filtreringer/utvalg av data.'),
             h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret.'),
             h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.'),
             h4(tags$b('Andeler: per sykehus og over tid'), ' viser andeler(prosent) per sykehus og utvikling over tid.
                             Man kan velge hvilken tidsskala man vi se på.'),
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
             br()
           ),
           mainPanel(
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
             rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
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
             h2((uiOutput("egetShTxt"))),
             br(),
             fluidRow(
               column(4,
                      h4('Antall skjema i kladd'),
                      uiOutput("iKladdPas"),
                      uiOutput("iKladdLege")
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
                      tableOutput('tabNokkeltallStart')
               ))
           )#main
  ), #tab

  # #------------- Registreringsoversikter (vise antall)--------------------

  tabPanel(
    p('Registreringsoversikter', title="Tabeller med registreringsoversikter"),
    value = 'Registreringsoversikter',

    sidebarPanel(
      width=3,
      h3('Tabellvalg'),
      conditionalPanel(condition = "input.ark == 'Antall operasjoner'",
                       dateInput(inputId = 'sluttDatoReg',
                                 label = 'Velg sluttdato', language="nb",
                                 value = Sys.Date(), max = Sys.Date() )
      ),
      conditionalPanel(
        condition = "input.ark == 'Antall operasjoner'",
        selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                    choices = rev(c('År'= 'Aar', 'Måned'='Mnd'))),
      br(),
      br(),
      br(),
      h4('Sjekk egne registreringer'),
      dateRangeInput(inputId = 'datovalgRegKtr', start = startDato, end = idag,
                     label = "Tidsperiode", separator="t.o.m.", language="nb"),
      downloadButton(outputId = 'lastNed_dataTilRegKtr', label='Last ned fødselsdato og operasjonsdato'),
      br()),

      conditionalPanel(
        condition = "input.ark == 'Antall skjema'",
        dateRangeInput(inputId = 'datovalgReg', start = startDato, end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb")
      ),

          ),

    mainPanel(
      tabsetPanel(id='ark',
                  tabPanel('Antall operasjoner',
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
  tabPanel(
    p("Registeradministrasjon", title='Registrators side for registreringer og resultater'),
    value = "Registeradministrasjon",
    h3('Egen side for registeradministratorer. Siden er bare synlig for SC-bruker'),
    tabsetPanel(
      tabPanel("Utsending av rapporter",
        # h4("Utsending av rapporter"),
        sidebarPanel(
          rapbase::autoReportOrgInput("RyggUtsending"),
          rapbase::autoReportInput("RyggUtsending"),

          #Tørrkjøring
          br(),
          shiny::actionButton(inputId = "run_autoreport",
                              label = "Kjør autorapporter"),
          shiny::dateInput(inputId = "rapportdato",
                           label = "Kjør rapporter med dato:",
                           value = Sys.Date()+1,
                           min = Sys.Date(),
                           max = Sys.Date() + 366
          ),
          shiny::checkboxInput(inputId = "dryRun", label = "Send e-post")
        ),
        mainPanel(
          rapbase::autoReportUI("RyggUtsending"),

          br(),
          p(em("System message:")),
          verbatimTextOutput("sysMessage"),
          p(em("Function message:")),
          verbatimTextOutput("funMessage")

        )
      ), #Utsending-tab

      shiny::tabPanel("Datakvalitet",
            sidebarPanel(
          numericInput(inputId = 'valgtTidsavvik',
                       label = 'Dager mellom registrerte operasjoner:',
                       value = 30,
                       min = 0,
                       max = NA,
                       step = 1
                       , width = '100px'
          )
        ),
        mainPanel(
          h3('Potensielle dobbeltregistreringer'),
          br(),
          h4('Funksjonen finner alle PID med to operasjoner gjort med valgt tidsintervall eller kortere. I tabellen
                       vises alle operasjoner for de aktuelle pasientene.'),
          downloadButton(outputId = 'lastNed_tabDblReg', label='Last ned tabell med mulige dobbeltregistreringer'),
          br(),
          tableOutput("tabDblReg")
        )
      ), #Datakvalitet-tab

      shiny::tabPanel("Datadump",
        # sidebarPanel(
        #   ),
        # mainPanel(
          h3('Last ned data'),
          br(),
          dateRangeInput(inputId = 'datovalgDatadump', start = startDato, end = idag,
                         label = "Tidsperiode", separator="t.o.m.", language="nb"),
          uiOutput('OppsumAntReg'),
          br(),
          br(),
          uiOutput("velgReshReg"),
          br(),
          downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump'),
          h5('Velger man startdato 1.januar 2019 eller senere, inneholder datadumpen kun registreringer fra versjon 3')
       #   )
      ), #Datadump-tab

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
             h4('Her kan man velge hvilken variabel man ønsker å se, samt gjøre ulike filtreringer.'),
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
                        # 'Operasjonsindikasjon, smertetype' = 'opIndSmeType',
                           'Operasjonskategori' = 'opKat',
                           'Radiologisk undersøkelse' = 'radUnders',
                           'Registreringsavvik, utf. pas.skjema - operasjon' ='regDiffOp',
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
             uiOutput("velgReshFord"),
             selectInput(inputId = "bildeformatFord",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
             br(),
             #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
             #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
             actionButton("reset_fordValg", label="Tilbakestill valg")
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
               choices = c('Kval.ind: Degen. spond, fusj. ved første operasjon' = 'degSponFusj1op',
                           'Kval.ind: Lite beinsmerter, ingen parese' = 'smBePreLav',
                           'Kval.ind: Betydelig forbedring av ODI etter prolapskirurgi' = 'OswEndr20ProKI',
                           'Kval.ind: Betydelig forbedring av ODI etter spinal stenose' = 'OswEndr30pstSSKI',
                           'Kval.ind: Tromboseprofylakse ved lett kirurig' = 'trombProfylLettKI',
                           'Kval.ind: Ventetid < 3 mnd. fra op. bestemt til utført' = 'ventetidSpesOp',
                           'Alder over 70 år' = 'alder70',
                           'Antibiotika' = 'antibiotika',
                           'Arbeidsstatus' = 'arbstatus',
                           'ASA-grad > II' = 'ASA',
                           'Degen. spondy. op. m/fusjon' = 'degSponFusj',
                           'Degen. spondy. 1. op. m/fusjon' = 'degSponFusj1op',
                           'Fedme (BMI>30)' = 'BMI',
                           'Flere enn to tidligere operasjoner' = 'tidlOp3',
                           'Fusjon, ryggsmerter uten utstråling, ingen dekompresjon' = 'opFusjonUtenDekompr',
                           'For sen registrering' = 'regForsinkelse',
                           'Forbedring av Oswestry-skår >= 20p' = 'OswEndr20',
                           'Fornøyde pasienter' = 'fornoydhet',
                           'Fremmedspråklig' = 'morsmal',
                           # 'Komplikasjoner, pasientrapportert' = 'kp3Mnd',
                           'Har degen. spondy. og spin.stenose' = 'degSponSSSten',
                           'Helt bra eller mye bedre' = 'nytte',
                           'Høyere utdanning' = 'utd',
                           'Komplikasjon, sårinfeksjon' = 'kpInf3mnd',
                           'Komplikasjoner ved operasjon' = 'peropKomp',
                           'Komplikasjon ved op.: Durarift' = 'peropKompDura',
                           'Misfornøyde pasienter' = 'misfornoyd',
                           'Minst 30% forbedring av Oswestry-skår' = 'OswEndr30pst',
                           'Mye verre/verre enn noen gang' = 'verre',
                           'Oppfølging, 3 mnd.' = 'oppf3mnd',
                           'Oppfølging, 12 mnd.' = 'oppf12mnd',
                           'Oppfølging, 3 og 12 mnd.' = 'oppf3og12mnd',
                           'Oswestry-skår < 23 poeng' = 'Osw22',
                           'Oswestry-skår > 48 poeng' = 'Osw48',
                           'Røykere' = 'roker',
                           'Skivedegenerasjon/spondylose uten nerveaffeksjon' = 'rfKunDegenerasjon',
                           'Smertestillende før operasjon' = 'smStiPre',
                           'Søkt erstatning før operasjon' = 'erstatningPre',
                           'Søkt uføretrygd før operasjon' = 'uforetrygdPre',
                           'Tromboseprofylakse gitt ifm. operasjon' = 'trombProfyl',
                           'Trygg kirurgi-prosedyre utført' = 'tryggKir',
                           'Varighet av rygg-/hoftesmerter >1 år' = 'symptVarighRyggHof',
                           'Varighet av utstrålende smerter >1 år' = 'sympVarighUtstr',
                           'Ventetid fra henvisning til time på poliklinikk' = 'ventetidHenvTimePol'
               ),
               selected = 'regForsinkelse'
             ),
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

  #----------Abonnement-----------------

  tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
           value = 'Abonnement',

           sidebarLayout(
             sidebarPanel(
               rapbase::autoReportInput("RyggAbb")
             ),
             shiny::mainPanel(
               rapbase::autoReportUI("RyggAbb")
             )
           )
  ) #tab abonnement



) #fluidpage, dvs. alt som vises på skjermen
} #UI

#----------------- Define server logic required  -----------------------
#' Server-del til appen
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return Server-delen til Rygg-appen
#' @export

server_rygg <- function(input, output, session) {
  #server <- function(input, output,session) {
  rapbase::appLogger(session, msg = 'Starter Rapporteket-Rygg')

  dataRegistry <- 'data'
  RegData <- RyggRegDataSQLV2V3(alleVarV3=1, alleVarV2=1)
  RegData <- RyggPreprosess(RegData = RegData)
  RegData <- RegData[order(RegData$OpDato, decreasing = TRUE), ]

  qEprom <- 'SELECT MCEID, TSSENDT, TSRECEIVED, NOTIFICATION_CHANNEL, STATUS,
                    DISTRIBUTION_RULE, REGISTRATION_TYPE from proms'
  ePROMadmTab <- rapbase::loadRegData(registryName=dataRegistry, query=qEprom)
  ind3mndeprom <- which(ePROMadmTab$REGISTRATION_TYPE %in% c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'))
  ind12mndeprom <- which(ePROMadmTab$REGISTRATION_TYPE %in% c('PATIENTFOLLOWUP12', 'PATIENTFOLLOWUP_12_PiPP', 'PATIENTFOLLOWUP_12_PiPP_REMINDER'))

  qskjemaoversikt <- 'SELECT * from skjemaoversikt'
  skjemaoversikt_orig <- rapbase::loadRegData(registryName=dataRegistry, query=qskjemaoversikt, dbType="mysql")
  skjemaoversikt <- merge(skjemaoversikt_orig, ePROMadmTab,
                          by.x='ForlopsID', by.y='MCEID', all.x = TRUE, all.y = FALSE)

  skjemaoversikt <- dplyr::rename(.data=skjemaoversikt, !!c(InnDato='HovedDato', ShNavn='Sykehusnavn'))

  qForlop <- 'SELECT AvdRESH, SykehusNavn, Fodselsdato, HovedDato, BasisRegStatus from forlopsoversikt'
  RegOversikt <- rapbase::loadRegData(registryName=dataRegistry, query=qForlop, dbType="mysql")
  RegOversikt <- dplyr::rename(RegOversikt, 'ReshId'='AvdRESH', 'InnDato'='HovedDato')

   map_avdeling <- data.frame(
    UnitId = unique(RegData$ReshId),
    orgname = RegData$ShNavn[match(unique(RegData$ReshId),
                                   RegData$ReshId)])

  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "rygg",
    map_orgname = shiny::req(map_avdeling),
    caller = "rygg"
  )


  # Duplikat er tatt hånd om i preprosess-fila
  sykehusValg  <- c(0, map_avdeling$UnitId[order(map_avdeling$orgname)])
  names(sykehusValg) <- c('Alle', map_avdeling$orgname[order(map_avdeling$orgname)])

  output$egetShTxt <- renderText(paste('Drift og resultater, ',
                                       as.character(RegData$ShNavn[match(user$org(), RegData$ReshId)])))

  observeEvent(user$role(), {
    if (user$role() == 'SC') {
      showTab(inputId = "hovedark", target = "Registeradministrasjon")
      shinyjs::show(id = 'velgReshReg')
      shinyjs::show(id = 'velgReshFord')
      shinyjs::show(id = 'lastNed_dataDump')
    } else {
      hideTab(inputId = "hovedark", target = "Registeradministrasjon")
      shinyjs::hide(id = 'velgReshReg')
      shinyjs::hide(id = 'velgReshFord')
      shinyjs::hide(id = 'lastNed_dataDump')
    }
  })


  observeEvent(input$reset_fordValg, shinyjs::reset("brukervalg_fordeling"))

  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  paaServer <- (context %in% c("DEV", "TEST", "QA", "QAC", "PRODUCTIONC", "PRODUCTION")) #rapbase::isRapContext()

  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', user$role(), '<br> ReshID: ', user$org()) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  #------ Dæsjbord ---------------------

  indKladd <- which(skjemaoversikt_orig$SkjemaStatus == 0)
  tabKladd <- skjemaoversikt_orig[skjemaoversikt_orig$SkjemaStatus == 0, c("AvdRESH", "SkjemaRekkeflg")]

  output$iKladdPas <- renderText(
    paste('Pasientskjema: ',
          sum(tabKladd$SkjemaRekkeflg==5 & tabKladd$AvdRESH == user$org())))

  output$iKladdLege <- renderText(
    paste('Legeskjema',
          sum(tabKladd$SkjemaRekkeflg==10 & tabKladd$AvdRESH == user$org())))

  output$forSen3mnd <- renderText(
    paste0('<b>', forsinketReg(RegData=RegData,
                               fraDato=Sys.Date()-400,
                               tilDato=Sys.Date()-100,
                               forsinkelse=100,
                               reshID = user$org()),'</b>',
           ' skjema ferdigstilt for sent for 3 mnd.ktr i perioden ',
           format.Date(Sys.Date()-400, '%d.%b%Y'), '-', format.Date(Sys.Date()-100, '%d.%b%Y'))
  )
  output$forSen12mnd <- renderText(
    paste0('<b>', forsinketReg(RegData=RegData,
                               fraDato=max(as.Date('2019-01-01'),Sys.Date()-745),
                               tilDato=Sys.Date()-380, forsinkelse=380, reshID = user$org()), '</b>',
           ' skjema ferdigstilt for sent for 12 mnd.ktr i perioden ',
           format.Date(max(as.Date('2019-01-01'),Sys.Date()-745), '%d.%b%Y'), '-', format.Date(Sys.Date()-380, '%d.%b%Y')))

  output$tabNokkeltallStart <- function() {
    tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd', reshID = user$org()))
    kableExtra::kable(tab,
                      full_width=F,
                      digits = c(0,0,1,1,1,1,0)) %>%
      kableExtra::column_spec(column = 1, width_min = '5em', width_max = '10em') %>%
      kableExtra::column_spec(column = 2:(ncol(tab)), width = '7em')  %>%
      kableExtra::row_spec(0, bold = T, align = 'c') %>%
      kableExtra::kable_styling(full_width = FALSE, position = 'left') #"hover",
  }

  output$tabAntOpphEget <- renderTable(
    tabAntOpphShMnd(RegData=RegData, datoTil=idag, reshID = user$org(), antMnd=12)
    ,rownames = T, digits=0, spacing="xs" )

  #-------Samlerapporter--------------------

  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporter(file, rnwFil="RyggMndRapp.Rnw",
                          reshID = user$org(), datoFra = startDato)
    }
  )


  #------Registreringsoversikter---------------------
  output$OppsumAntReg <- renderUI({
    Registreringer <- RyggUtvalgEnh(RegData=RegData,
                                    datoFra = input$datovalgDatadump[1],
                                    datoTil=input$datovalgDatadump[2])$RegData[,'PasientID']
    antallReg <- length(Registreringer)
    antallPers <- length(unique(Registreringer))
    HTML(paste0('<b> I perioden ',format.Date(input$datovalgDatadump[1], '%d. %B %Y'), ' - ',
                format.Date(input$datovalgDatadump[2], '%d. %B %Y'),
                ' er det totalt registrert ', antallReg, ' operasjoner.
                Disse er utført på tilsammen ',
                antallPers, ' personer.', '</b>' ))})


  observe({
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
                     Aar = paste0(t1, 'per år til og med ', input$sluttDatoReg, '<br />'))
      ))})

    AntSkjemaAvHver <- tabAntSkjema(RegData=RegData,
                                    datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2])
    #skjemastatus=as.numeric(input$skjemastatus))
    output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                       ,rownames = T, digits=0, spacing="xs" )
    output$lastNed_tabAntSkjema <- downloadHandler(
      filename = function(){'tabAntSkjema.csv'},
      content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, fileEncoding = 'latin1', na = '')})
  })

  output$velgReshReg <- renderUI({
    selectInput(inputId = 'velgReshReg', label='Velg sykehus',
                selected = 0,
                choices = sykehusValg) })

  # Hente oversikt over hvilke registrereinger som er gjort (opdato og fødselsdato), samt datadump
  observe({
    #reshKtr <- ifelse(is.null(input$velgReshReg), user$org(), input$velgReshReg )
    indKtr <- which(RegOversikt$ReshId == user$org())  # if (reshKtr == 0) {1:dim(RegOversikt)[1]} else {which(RegOversikt$ReshId == reshKtr)}
    dataRegKtr <- dplyr::filter(RegOversikt[indKtr, ],
                                as.Date(InnDato) >= input$datovalgRegKtr[1],
                                as.Date(InnDato) <= input$datovalgRegKtr[2])

    output$lastNed_dataTilRegKtr <- downloadHandler(
      filename = function(){'dataTilKtr.csv'},
      content = function(file, filename){write.csv2(dataRegKtr, file, row.names = F, fileEncoding = 'latin1', na = '')})

    if (user$role()=='SC') {


      fritxtVar <- c("AnnetMorsm", "DekomrSpesAnnetNivaaDekomrSpesAnnetNivaa", "Fritekstadresse",
                     "FusjonSpes", "OpAndreSpes", "OpAnnenOstetosyntSpes", "OpIndAnSpe", "RfAnnetspes",
                     "SpesifiserReopArsak", "SpesTrombProfyl", "SykdAnnetspesifiser", "SykdAnnetSpesifiser")
      RegDataV2V3 <- RegData[ ,-which(names(RegData) %in% fritxtVar)]
      dataDump <- RyggUtvalgEnh(RegData = RegDataV2V3,
                                datoFra = input$datovalgDatadump[1],
                                datoTil = input$datovalgDatadump[2],
                                enhetsUtvalg = 2,
                                reshID = ifelse(is.null(input$velgReshReg),0,input$velgReshReg))$RegData
      if (dim(dataDump)[1] > 0) {
        dataDump <- finnReoperasjoner(RegData = dataDump)}

      txtLog <- paste0('Datadump for Rygg: ',
                       'tidsperiode_', input$datovalgDatadump[1], '_', input$datovalgDatadump[2],
                       '_resh_', ifelse(is.null(input$velgReshReg),0,input$velgReshReg))

      output$lastNed_dataDump <- downloadHandler(
        filename = function(){'dataDump.csv'},
        content = function(file, filename){write.csv2(dataDump, file, row.names = F, fileEncoding = 'latin1', na = '')
          rapbase::repLogger(session = session, msg = txtLog)
        })
    }
  }) #observe

  #-----------Registeradministrasjon-----------

  #Datakvalitet (dobbeltregistreringer)
  observe({
    tabDblReg <- tabPasMdblReg(RegData=RegData, tidsavvik=input$valgtTidsavvik)
    output$tabDblReg <- renderTable(tabDblReg, digits=0)

    output$lastNed_tabDblReg <- downloadHandler(
      filename = function(){paste0('MuligeDobbeltReg.csv')},
      content = function(file, filename){write.csv2(tabDblReg, file, row.names = F, fileEncoding = 'latin1', na = '')})
  })

  #----------- Eksport ----------------
  ## brukerkontroller
  rapbase::exportUCServer("ryggExport", "rygg")
  ## veileding
  rapbase::exportGuideServer("ryggExportGuide", "rygg")


  #------------Fordelinger---------------------

  observeEvent(input$reset, {
    shinyjs::reset("enhetsUtvalg")
    shinyjs::reset("datovalg")
    shinyjs::reset("hastegrad")
    shinyjs::reset("erMann")
    shinyjs::reset("alder")
  })

  output$velgReshFord <- renderUI({
    selectInput(inputId = 'velgReshFord', label='Velg sykehus',
                selected = 0,
                choices = sykehusValg)
  })

  # output$velgReshReg <- renderUI({
  #   selectInput(inputId = 'velgReshReg', label='Velg sykehus',
  #               selected = 0,
  #               choices = sykehusValg) })


  output$fordelinger <- renderPlot({
    #reshIDford <- ifelse(user$role()=='SC', input$velgReshFord, user$org())
    #reshIDford <- ifelse(is.null(input$velgReshFord), user$org(), input$velgReshFord)
    RyggFigAndeler(RegData=RegData, preprosess = 0,
                   valgtVar=input$valgtVar,
                   reshID = ifelse(is.null(input$velgReshFord), user$org(), input$velgReshFord),
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
    # reshIDford <- ifelse(user$role()=='SC', input$velgReshFord, user$org())
    UtDataFord <- RyggFigAndeler(RegData=RegData, preprosess = 0,
                                 valgtVar=input$valgtVar,
                                 reshID = ifelse(is.null(input$velgReshFord), user$org(), input$velgReshFord),
                                 enhetsUtvalg=as.numeric(input$enhetsUtvalg),
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
        kableExtra::add_header_above(kolGruppering[1:(2+UtDataFord$medSml)]) %>%
        #kableExtra::add_header_above(c(" "=1, tittelKolGr[1] = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        kableExtra::column_spec(column = 1, width='5em') %>% #width_min = '3em', width_max = '10em') %>%
        kableExtra::column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
        kableExtra::row_spec(0, bold = T)
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
                    reshID = user$org(),
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

observe({
    AndelerTid <-
      RyggFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                  reshID = user$org(),
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
        kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        kableExtra::column_spec(column = 1, width_min = '7em') %>%
        kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
        kableExtra::row_spec(0, bold = T)
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
                        reshID = user$org(),
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
})

observe({    #AndelGrVar
    AndelerShus <- RyggFigAndelerGrVar(
      RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
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
        #kableExtra::column_spec(column = 1, width_min = '5em') %>%
        kableExtra::column_spec(column = 1:(antKol+1), width = '5em') %>%
        kableExtra::row_spec(0, bold = T)
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
})

  #------------------ Abonnement ----------------------------------------------
  orgs <- as.list(sykehusValg[-1])
  paramNamesAbb <- shiny::reactive(c('reshID'))
  paramValuesAbb <- shiny::reactive(c(user$org()))
  rapbase::autoReportServer(
    id = "RyggAbb",
    registryName = "rygg",
    type = "subscription",
    paramNames = paramNamesAbb,
    paramValues = paramValuesAbb,
    reports = list(
      Kvartalsrapp = list(
        synopsis = "NKR_Rygg/Rapporteket: Kvartalsrapport, abonnement",
        fun = "abonnementRygg",
        paramNames = c('rnwFil', 'reshID'),
        paramValues = c('RyggMndRapp.Rnw', "user$org()")
      )
    ),
    orgs = orgs,
    user = user
  )

  #-----Utsendinger

  ## liste med metadata for rapport
  org <- rapbase::autoReportOrgServer("RyggUtsending", orgs)
  # oppdatere reaktive parametre, for å få inn valgte verdier (overskrive de i report-lista)
  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))
  vis_rapp <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  rapbase::autoReportServer(
    id = "RyggUtsending",
    registryName = "rygg",
    type = "dispatchment",
    org = org$value,
    paramNames = paramNames,
    paramValues = paramValues,
    reports = list(
      KvartalsRapp = list(
        synopsis = "Rapporteket-Degenerativ Rygg: Kvartalsrapport",
        fun = "abonnementRygg",
        paramNames = c('rnwFil', "reshID"),
        paramValues = c('RyggMndRapp.Rnw', "org$value()")
      )
    ),
    orgs = orgs,
    eligible = vis_rapp,
    user = user
  )

  #Tørrkjøring
  kjor_autorapport <- shiny::observeEvent(input$run_autoreport, {
    dato <- input$rapportdato
    dryRun <- !(input$dryRun)
    withCallingHandlers({
      shinyjs::html("sysMessage", "")
      shinyjs::html("funMessage", "")
      shinyjs::html("funMessage",
                    rapbase::runAutoReport(group = "rygg",
                                           dato = dato, dryRun = dryRun))
    },
    message = function(m) {
      shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
    })
  })




} #server

# Run the application
# shinyApp(ui = ui, server = server)


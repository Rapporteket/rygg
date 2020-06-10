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
startDato <- paste0(as.numeric(format(idag-90, "%Y")), '-01-01') #'2019-01-01' #Sys.Date()-364
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
  RegData <- RyggRegDataSQL()
  qSkjemaOversikt <- 'SELECT * from SkjemaOversikt'
  SkjemaOversikt <- rapbase::LoadRegData(registryName="rygg", query=qSkjemaOversikt, dbType="mysql")
  qForlop <- 'SELECT AvdRESH, SykehusNavn, Fodselsdato, HovedDato, BasisRegStatus from ForlopsOversikt'
  RegOversikt <- rapbase::LoadRegData(registryName="rygg", query=qForlop, dbType="mysql")
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
tidlOprvalg <-	c('Alle'=99, 'Tidl. operert samme nivå'=1, 'Tidl. operert annet nivå'=2,
                   'Tidl. operert annet og sm. nivå'=3, 'Primæroperasjon'=4)
hastegradvalg <- c('Alle' = 99, 'Elektiv' = 1, 'Akutt' = 2)

sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Alle',sykehusNavn$x)


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
             h3("Rapport med månedsresultater"),
             h4("NB: Dette er en foreløpig versjon. Innholdet vil bli justert og utvidet."),
             h5('Rapporten kan man også få regelmessig på e-post.
                        Gå til fanen "Abonnement" for å bestille dette.'),
             br(),
             downloadButton(outputId = 'mndRapp.pdf', label='Last ned månedsrapport', class = "butt"),
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
             h5('Grunnet overgang til ny teknisk løsning, er det fortsatt mye "utdata" som mangler. Eksempelvis
                hovedkategorier og data fra oppfølgingsskjema.'),
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
                        br(),
                        br(),
                        h4('Last ned egne data for kontroll av registrering'),
                        dateRangeInput(inputId = 'datovalgRegKtr', start = startDato, end = idag,
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
                        selectInput(inputId = 'velgReshReg', label='Velg sykehus',
                                    selected = 0,
                                    choices = sykehusValg),
                        downloadButton(outputId = 'lastNed_dataTilRegKtr', label='Last ned fødselsdato og operasjonsdato'),
                        br(),
                        br(),
                        downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump')

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

  #----------------Kvalitetsindikatorer----------------------------------
  # tabPanel(p("Kvalitetsindikatorer", title='Kval.ind.: Per sykehus og utvikling over tid'),
  #          h2('Side som bare viser kvalitetsindikatorer', align='center'),
  #
  #          sidebarPanel(
  #          h4('Kan gjøre utvalg på: tidsperiode og tidsenhet')),
  #          mainPanel(
  #            h4('Viser to figurer/tabeller per indikator: Utvikling over tid og per sykehus'),
  #            br(),
  #            h2('På vent til hovedkategorier er definert')
  #                    )
  # ), #tab, KI

#-------Registeradministrasjon----------
    tabPanel(p("Registeradministrasjon", title='Registrators side for registreringer og resultater'),
             value = "Registeradministrasjon",
             h3('Egen side for registeradministratorer? (Bare synlig for SC-bruker'),
             #uiOutput('rolle'),
             h4('Alternativt kan vi ha elementer på startsida og/eller registreringsoversiktsida som bare er synlig for SC'),
           br(),
           br(),
           sidebarPanel(
             h4('Nedlasting av data til Resultatportalen:'),

             selectInput(inputId = "valgtVarRes", label="Velg variabel",
                         choices = c('Lite beinsmerte før operasjon' = 'beinsmLavPre',
                                     'Durarift' = 'peropKompDura',
                                     'Utstrålende smerter i mer enn ett år' = 'sympVarighUtstr')
             ),
             selectInput(inputId = 'hastegradRes', label='Operasjonskategori (hastegrad)',
                         choices = hastegradvalg
             ),
             selectInput(inputId = 'tidlOpRes', label='Tidligere operert?',
                         choices = tidlOprvalg
             ),
             # dateRangeInput(inputId = 'aarRes', start = startDato, end = Sys.Date(),
             #                label = "Operasjonaår", separator="t.o.m.", language="nb", format = 'yyyy'
             #                ),
             sliderInput(inputId="aarRes", label = "Operasjonsår", min = as.numeric(2016),
                         max = as.numeric(year(idag)), value = c(2018, year(idag), step=1, sep="")
             ),
             br(),
             downloadButton(outputId = 'lastNed_dataTilResPort', label='Last ned data')),

           fluidRow(h3('Hva mer skal med her...?'),
                    tags$div(
                      tags$li("Andel ikke besvart 3 mnd. - mangler variabel"),
                      tags$li("Andel ikke besvart 12 mnd. - mangler variabel"),
                      tags$li("Andel purringer 3 mnd. - mangler variabel"),
                      tags$li("Andel purringer 12 mnd. - mangler variabel")
                    ))
  ), #tab SC

#-------------Fordelinger---------------------
tabPanel(p('Fordelinger',
                    title='Alder, Innkomstmåte,... '),
                  sidebarPanel(
                    width = 3,
                    h4('Her kan man velge hvilken variabel man ønsker å se og gjøre ulike filtreringer.'),
                    br(),
                    selectInput(
                      inputId = "valgtVar", label="Velg variabel",
                      choices = c('Alder' = 'alder',
                                  #'Liggetid' = 'antDagerInnl',
                                  'Angst/depresjon (EQ5D) før operasjon' = 'EQangstPre',
                                  'Antibiotikaprofylakse?' = 'antibiotika',
                                  #Antall nivå operert' = antNivOpr:
                                  'Arbeidsstatus' = 'arbstatus', #Velger skjema separat
                                  #'Arbeidsstatus, 3 mnd. etter' = 'arbstatus3mnd',
                                  #'Arbeidsstatus 12 mnd. etter' = 'arbstatus12mnd',
                                  'ASA-grad' = 'ASA',
                                  'BMI (Body Mass Index)' = 'BMI',
                                  'EQ5D, preoperativt' = 'EQ5DPre',
                                  'Gangfunksjon (EQ5D) før operasjon' = 'EQgangePre',
                                  'Har pasienten søkt erstatning?' = 'erstatningPre',
                                  #Fornoyd3mnd: Fornøydhet 3 mnd etter operasjon
                                  #Fornoyd12mnd: Fornøydhet 12 mnd etter operasjon
                                  #Hovedinngrep = HovedInngrep
                                  'Komorbiditet' = 'komorbiditet',
                                  'Komplikasjoner, perop. ' = 'komplPer' ,
                                  #'komplikasjoner, pasientrapp. ' = 'komplPost',
                                  'Liggetid ved operasjon, totalt' = 'liggedogn',
                                  'Liggetid, postoperativt' = 'liggetidPostOp',
                                  'Morsmål' = 'morsmal',
                                  #Nytte3mnd: Hvilken nytte har du hatt av operasjonen? (svar 3 måneder etter)
                                  #Nytte12mnd: Hvilken nytte har du hatt av operasjonen? (svar 12 måneder etter)
                                  #'Operasjonsindikasjon' = 'opInd',
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
                                  #'Tidligere operasjoner, antall' = 'tidlOprAntall',
                                  'Søkt uføretrygd før operasjon' = 'uforetrygdPre',
                                  #Underkat: Fordeling av inngrepstyper. NB: hovedkategori MÅ velges
                                  'Utdanning (høyeste fullførte)' = 'utd'
                      )
                    ),
                    dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                                   label = "Tidsperiode", separator="t.o.m.", language="nb"),
                    selectInput(inputId = "erMann", label="Kjønn",
                                choices = kjonn
                    ),
                    sliderInput(inputId="alder", label = "Alder", min = 0,
                                max = 110, value = c(0, 110)
                    ),
                    selectInput(inputId = 'hastegrad', label='Operasjonskategori (hastegrad)',
                                choices = hastegradvalg
                    ),
                    selectInput(inputId = 'tidlOp', label='Tidligere operert?',
                                choices = tidlOprvalg
                    ),selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                choices = enhetsUtvalg,
                    )
                    #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                    #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel(
                        'Figur',
                        h3('Fordelingsfigurer'),
                        h5('Høyreklikk på figuren for å laste den ned'),
                        plotOutput('fordelinger')),
                      tabPanel(
                        'Tabell',
                        uiOutput("tittelFord"),
                        tableOutput('fordelingTab'),
                        downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
                      )
                    )
                  )
         ), #tab Fordelinger

#------------------Resultater, prosentvise--------------
# tabPanel('Resultater, prosentvise'), #tab, andeler
# tabPanel('Resultater, gjennomsnitt') #tab, gjennomsnitt


#------------------Abonnement-------------------------

tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av rapporter på e-post'),
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("subscriptionRep", "Rapport:",
                                    c("Månedsrapport")), #, "Samlerapport", "Influensaresultater")),
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

  raplog::appLogger(session, msg = 'Starter Rapporteket-Rygg')
  #reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)),
  #                           601161)})
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 601161)
  #rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'LU')
  output$rolle <- renderText(rolle)
  brukernavn <- ifelse(paaServer, rapbase::getUserName(session), 'inkognito')
  output$egetShnavn <- renderText(as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))
  output$egetShTxt <- renderText(paste('Drift og resultater, ',
                                       as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])))

  observe({if (rolle != 'SC') { #
    shinyjs::hide(id = 'velgReshReg')
    shinyjs::hide(id = 'lastNed_dataDump')
    #hideTab(inputId = "tabs", target = "Foo")
    shiny::hideTab(inputId = "tab1nivaa",
                   target = 'Registeradministrasjon') #
  }
  })
#print(dim(RegData)[1])
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
    tabAntOpphSh <- switch(input$tidsenhetReg,
           Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
           Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))

    output$tabAntOpphSh <- renderTable(tabAntOpphSh, rownames = T, digits=0, spacing="xs")
    output$lastNed_tabAntOpphSh <- downloadHandler(
      filename = function(){'tabAntOpphSh.csv'},
      content = function(file, filename){write.csv2(tabAntOpphSh, file, row.names = T, na = '')})

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
    content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, na = '')})
    })


  # #Velge ferdigstillelse og tidsintervall.
  # output$tabAntSkjema <- renderTable({})

# Hente oversikt over hvilke registrereinger som er gjort (opdato og fødselsdato)
  tilretteleggDataDumper <- function(data, datovalg, reshID, rolle){
    data <- dplyr::filter(data,
                          # InnDato >= datovalg[1],
                          # InnDato <= datovalg[2])
                          as.Date(InnDato) >= datovalg[1],
                          as.Date(InnDato) <= datovalg[2])
    if (rolle == 'SC') {
      valgtResh <- as.numeric(reshID)
      ind <- if (valgtResh == 0) {1:dim(data)[1]
      } else {which(as.numeric(data$ReshId) %in% as.numeric(valgtResh))}
      data <- data[ind,]
    } else {data[which(data$ReshId == reshID), ]}
  }

  observe({
    dataRegKtr <- tilretteleggDataDumper(data=RegOversikt, datovalg = input$datovalg,
                                         reshID=input$velgReshReg, rolle = rolle)
  #   RegOversikt <- dplyr::filter(RegOversikt,
  #                                as.Date(HovedDato) >= input$datovalgRegKtr[1],
  #                                as.Date(HovedDato) <= input$datovalgRegKtr[2])
  # tabDataRegKtr <- if (rolle == 'SC') {
  #   valgtResh <- as.numeric(input$velgReshReg)
  #   ind <- if (valgtResh == 0) {1:dim(RegOversikt)[1]
  #     } else {which(as.numeric(RegOversikt$AvdRESH) %in% as.numeric(valgtResh))}
  #   RegOversikt <- RegOversikt[ind,]
  #     } else {RegOversikt[which(RegOversikt$AvdRESH == reshID), ]}


  output$lastNed_dataTilRegKtr <- downloadHandler(
    filename = function(){'dataTilKtr.csv'},
    content = function(file, filename){write.csv2(dataRegKtr, file, row.names = F, na = '')})


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

  dataDump <- tilretteleggDataDumper(data=RegData, datovalg = input$datovalg,
                                     reshID=input$velgReshReg, rolle = rolle)
  output$lastNed_dataDump <- downloadHandler(
      filename = function(){'dataDump.csv'},
      content = function(file, filename){write.csv2(dataDump, file, row.names = F, na = '')})
  })
  #-----------Registeradministrasjon-----------

  if (rolle=='SC') {
  observe({
    tabdataTilResPort <- dataTilResPort(RegData=RegData, valgtVar = input$valgtVarRes,
                                        aar=as.numeric(input$aarRes[1]):as.numeric(input$aarRes[2]),
                                        hastegrad = input$hastegradRes, tidlOp = input$tidlOpRes)

    output$lastNed_dataTilResPort <- downloadHandler(
      filename = function(){'dataTilResPort.csv'},
      content = function(file, filename){write.csv2(tabdataTilResPort, file, row.names = T, na = '')})
  })
}
  #------------Fordelinger---------------------

  observeEvent(input$reset, {
    shinyjs::reset("enhetsUtvalg")
    shinyjs::reset("datovalg")
    shinyjs::reset("hastegrad")
    shinyjs::reset("erMann")
    shinyjs::reset("alder")
  })
  output$fordelinger <- renderPlot({
    RyggFigAndeler(RegData=RegData, preprosess = 0,
                   valgtVar=input$valgtVar,
                  reshID=reshID,
                  enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                  datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                  minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                  erMann=as.numeric(input$erMann),
                  hastegrad = as.numeric(input$hastegrad),
                  tidlOp = as.numeric(input$tidlOp),
                  session = session)
  }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  )

  observe({
    UtDataFord <- RyggFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                erMann=as.numeric(input$erMann),
                                hastegrad = as.numeric(input$hastegrad),
                                tidlOp = as.numeric(input$tidlOp),
                                lagFig = 0, session = session)

    tabFord <- lagTabavFig(UtDataFraFig = UtDataFord)

    output$tittelFord <- renderUI({
      tagList(
        h3(HTML(paste0(UtDataFord$tittel, '<br />'))),
        #h3(UtDataFord$tittel),
        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
      )}) #, align='center'

    # output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
    #
    #   #       kable_styling("hover", full_width = F)
    #   antKol <- ncol(tab)
    #   print(antKol)
    #   print(tab)
    #   kableExtra::kable(tab, format = 'html'
    #                     , full_width=F
    #                     , digits = c(0,1,0,1)[1:antKol]
    #   ) %>%
    #     add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
    #     column_spec(column = 1, width_min = '7em') %>%
    #     column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
    #     row_spec(0, bold = T)
    # }

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
        write.csv2(tab, file, row.names = F, na = '')
      })
  }) #observe


  #------------------ Abonnement ----------------------------------------------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))


  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                rapbase::getUserEmail(session), ":")),
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
    if (input$subscriptionRep == "Månedsrapport") {
      synopsis <- "rygg/Rapporteket: månedsrapport"
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
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })








} #server
# Run the application
shinyApp(ui = ui, server = server)


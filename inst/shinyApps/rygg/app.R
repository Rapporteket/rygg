#library(magrittr)
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
startDato <- startDato <- paste0(as.numeric(format(idag-50, "%Y")), '-01-01') #'2019-01-01' #Sys.Date()-364
sluttDato <- idag
datoTil <- as.POSIXlt(idag)
datofra12 <- lubridate::floor_date(as.Date(datoTil)- months(12, abbreviate = T), unit='month')
idag <- Sys.Date()

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))



context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA", "PRODUCTION")) #rapbase::isRapContext()
regTitle = ifelse(paaServer, 'Norsk Kvalitetsregister for Ryggkirurgi, testversjon med FIKTIVE data',
                  'Norsk Kvalitetsregister for Ryggkirurgi, testversjon med FIKTIVE data')


if (paaServer) {
  RegData <- RyggRegDataSQL()
  qSkjemaOversikt <- 'SELECT * from SkjemaOversikt'
  SkjemaOversikt <- rapbase::LoadRegData(registryName="rygg", query=qSkjemaOversikt, dbType="mysql")
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
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))
tidlOprvalg <-	c('Alle'=99, 'Tidl. operert samme nivå'=1, 'Tidl. operert annet nivå'=2,
                   'Tidl. operert annet og sm. nivå'=3, 'Primæroperasjon'=4)
hastegradvalg <- c('Alle' = 99, 'Elektiv' = 1, 'Akutt' = 2)


# Define UI for application
ui <- navbarPage(
  title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle), # lag logo og tittel som en del av navbar. - Funker det med fluidPage?
  # sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",

  #------------ Startside -----------------
  tabPanel(p("Startside", title='Registrators oversikt over registreringer og resultater'),
           shinyjs::useShinyjs(),
           tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color#fluidRow(
           #column(width=5,
           h2('Velkommen til Rapporteket for NKR, Rygg!', align='center'),




           sidebarPanel(
             h3('Her vil det komme nedlastbare dokumenter med samling av resultater'),
          br()
           ),
           mainPanel(
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
             rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                          organization = uiOutput("appOrgName")
                                          , addUserInfo = TRUE
             ),
             br(),
             br(),
             h4('Oppdatere : På Rapporteket kan man finne visualiseringer og oppsummeringer av de fleste variable som registreres
                  i registeret. I hver fane kan man velge hvilken variabel man vil se resultat for og om man vil gjøre
                filtreringer. Hold musepekeren over fanen for å se hvilke variable/tema som er visualisert i fanen.
                Fanene er i hovedsak organisert ut fra hvordan resultatene er visualisert. F.eks.
                finner man under "Andeler" resultater av typen "andel under 80 år" og
                "andel som fikk komplikasjon". Under "gjennomsnitt" finner man eksempelvis beregninger av
                "gjennomsnittsalder" eller gjennomsnittlig knivtid.'),             h4('Du er nå inne på Rapporteket for NGER. Rapporteket er registerets resultattjeneste.
                            Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                            På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                            på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                            Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
             h4('Du kan se på resultater for eget sykehus, nasjonale tall og eget sykehus sett opp
                              mot landet for øvrig. Resultatene som vises er
                              basert på operasjonsdato. Alle figurer og
                            tabeller kan lastes ned.'),
             br(),
             h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
             h4('I feltet til venstre på hver side kan man velge hvilken variabel man ønsker å se
                            resultater for. Der kan man også gjøre ulike filtreringer/utvalg av data.'),
             h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret.'),
             h4(tags$b('Kvalitetsindikatorer '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
             h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
             h4(tags$b('Andeler: per sykehus og over tid'), ' viser andeler(prosent) en per sykehus og utvikling over tid.
                            Man kan velge hvilken tidsskala man vi se på.'),
             h4(tags$b('Gjennomsnitt: per sykehus og over tid'), ' viser gjennomsnittsverdier per sykehus og utvikling over tid.
                            Man kan velge om man vil se gjennomsnitt eller median.'),
             h4('Gi gjerne innspill og tilbakemeldinger til registerledelsen vedrørende
                            innhold på Rapporteket'),
             br(),
             br(),
             h2(paste("Drift og resultater, egen avdeling")), #, uiOutput("egetShnavn"))), #, align='center' ),
             fluidRow(
               h5('Registreringer siste år:'),
               tableOutput("tabAntOpphEget")
             ),
             br(),
             fluidRow(
               column(4,
               h4('Antall skjema i kladd:'),
               uiOutput("iKladdPas"),
               uiOutput("iKladdLege")
               #h5(paste('Pasientskjema:', uiOutput("iKladdPas"))),
               #h5(paste('Lengeskjema:', uiOutput("iKladdLege")))
             ),
             column(4,
                    h4('Registreringsforsinkelse'),
                    h5('Andel/antall registrert/ferdigstilt for sent for 3månederskontroll: ...'),
             h5('Andel/antall registrert/ferdigstilt for sent for 12månederskontroll: ...'))
             ),

             fluidRow(h4('tabell med resultat Per måned siste år'),
                      tags$div(tags$li('Andel over 70 år'),
                                tags$li('Gjennomsnittsalder'),
                  tags$li('Andel kvinner'),
                       tags$li('Fornøyd med behandlingen, 3 mnd. etter'),
                               tags$li('Helt restituert/mye bedre, 3 mnd. etter'),
                                       tags$li('Verre 3 mnd. etter')
           ))
           )#main
  ), #tab

  # #------------- Registreringsoversikter (vise antall)--------------------

  tabPanel(p('Registreringsoversikter',title="Tabeller med registreringsoversikter"),
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
                        )
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
  tabPanel(p("Kvalitetsindikatorer", title='Kval.ind.: Per sykehus og utvikling over tid'),
           h2('Side som bare viser kvalitetsindikatorer', align='center'),

           sidebarPanel(
           h4('Kan gjøre utvalg på: tidsperiode og tidsenhet')),
           mainPanel(h4('Viser to figurer/tabeller per indikator: Utvikling over tid og per sykehus')
                     )
  ), #tab, KI


  tabPanel(p("Registeradministrasjon", title='Bare synlig for SC-bruker'),
           h3('Egen side for registeradministratorer? (Bare synlig for SC-bruker'),
           h4('Alternativt kan vi ha elementer på startsida og/eller registreringsoversiktsida som bare er synlig for SC'),
           br(),
           h5('Hva ønsker man skal være synlig kun for SC-bruker?'),
           br(),
           br(),
           sidebarPanel(
             h4('F.eks. nedlasting av data til Resultatportalen:'),

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
                      tags$li("Andel ikke besvart 3 mnd. "),
                      tags$li("Andel ikke besvart 12 mnd."),
                      tags$li("Andel purringer 3 mnd."),
                      tags$li("Andel purringer 12 mnd.")
                    ))
  ), #tab SC

tabPanel('Fordelinger'),
tabPanel('Resultater, prosentvise'),
tabPanel('Resultater, gjennomsnitt')

) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required  -----------------------
server <- function(input, output,session) {

  raplog::appLogger(session, msg = 'Starter Rapporteket-Rygg')
  #reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)),
  #                           601161)})
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 601161)
  #rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'LU')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(session), 'inkognito')
  output$egetShnavn <- renderText(as.character(RegData$ShNavn[match(reshID, RegData$ReshId)]))


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


  observe({
    output$tabAntOpphEget <- renderTable(
      tabAntOpphShMnd(RegData=RegData, datoTil=datoTil, reshID = reshID, antMnd=12)
      ,rownames = T, digits=0, spacing="xs" )

     tabdataTilResPort <- dataTilResPort(RegData=RegData, valgtVar = input$valgtVarRes,
                                        aar=as.numeric(input$aarRes[1]):as.numeric(input$aarRes[2]),
                                        hastegrad = input$hastegradRes, tidlOp = input$tidlOpRes)
                                        #datoFra = input$datoFraRes, hovedkat = hovedkatRes,

   # fil <- dataTilResPort(RegData=RegData, valgtVar = 'beinsmLavPre', aar)


    output$lastNed_dataTilResPort <- downloadHandler(
      filename = function(){'dataTilResPort.csv'},
      content = function(file, filename){write.csv2(tabdataTilResPort, file, row.names = T, na = '')})

  })



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
    t1 <- 'Tabellen viser operasjoner '
    h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                   Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                   Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))
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

} #server
# Run the application
shinyApp(ui = ui, server = server)


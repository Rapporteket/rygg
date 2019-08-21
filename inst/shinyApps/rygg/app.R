library(shiny)
library(knitr)
#library(magrittr)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)

startDato <- '2018-01-01' #Sys.Date()-364
idag <- Sys.Date()
sluttDato <- idag

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

regTitle = 'Norsk Kvalitetsregister for Ryggkirurgi, testversjon med FIKTIVE data'

#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))


# Define UI for application
#fluidPage( #"Hoved"Layout for alt som vises på skjermen
ui <- navbarPage( title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle), # lag logo og tittel som en del av navbar. - Funker det med fluidPage?
  theme = "bootstrap.css",
  # sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  # velg css (foreløpig den eneste bortsett fra "naken" utgave)
  #theme = "rap/bootstrap.css",

  #------------ Viktigste resultater-----------------
  tabPanel(p("Viktigste resultater", title='Oversikt over registreringer og resultater'),
           h2('Velkommen til ny versjon av Rapporteket for NKR, Rygg!', align='center'),
           sidebarPanel(
           h4('På Rapporteket kan man finne visualiseringer og oppsummeringer av de fleste variable som registreres
                  i registeret. I hver fane kan man velge hvilken variabel man vil se resultat for og om man vil gjøre
                filtreringer. Hold musepekeren over fanen for å se hvilke variable/tema som er visualisert i fanen.
                Fanene er i hovedsak organisert ut fra hvordan resultatene er visualisert. F.eks.
                finner man under "Andeler" resultater av typen "andel under 80 år" og
                "andel som fikk komplikasjon". Under "gjennomsnitt" finner man eksempelvis beregninger av
                "gjennomsnittsalder" eller gjennomsnittlig knivtid.'),
           br()),
           mainPanel(
             h2(paste("Drift og resultater,", egetShnavn)), #, align='center' ),
             fluidRow('Drift',
               h5('Registreringer siste år:'),
               tableOutput("tabAntOpphEget")
             ),
             fluidRow(
               column(4,
               h4('Antall skjema i kladd:'),
               h5(paste('Pasientskjema:', tabKladd[1])),
               h5(paste('Lengeskjema:', tabKladd[2]))
             ),
             column(4,
                    h4('Registreringsforsinkelse'),
               h5('Andel/antall registrert for sent'))
             )
             #fluidRow()
           )
  ), #tab

  # #------------- Tabeller (vise antall)--------------------

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
                                    #tableOutput("tabAntOpphSh"),
                                    downloadButton(outputId = 'lastNed_tabAntOpphSh', label='Last ned')
                                  )
                         ),
                         tabPanel('Antall skjema',
                                  h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                  p("Velg tidsperiode i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntSkjema"),
                                    downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                  )
                                           # h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                                           # p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                           # tableOutput("tabAvdSkjema12"))
             )))
           ) #tab

) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required  -----------------------
server <- function(input, output) {

  library(rygg)
  library(lubridate)
  library(zoo)
  library(tools)
#  system.file('NakkeMndRapp.Rnw', package='Nakke')
  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
  registryName <- "rygg"
  dbType <- "mysql"
  query <- paste0('SELECT  ...')
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
} else {
 load(file = 'A:/Rygg/RyggData.RData')
}


  RegData <- RyggPreprosess(RegData = RegData)
 SkjemaOversikt <- dplyr::rename(.data=SkjemaOversikt, !!c(InnDato='HovedDato', ShNavn='Sykehusnavn'))

  # Parametre:
  reshIDdummy <- 601161
  reshID <- reshIDdummy
  egetShnavn <- as.character(RegData$ShNavn[match(reshID, RegData$ReshId)])
  indEget <- which(RegData$ReshId == reshID)
  datoTil <- as.POSIXlt(Sys.Date())
  datofra12 <- lubridate::floor_date(as.Date(datoTil)- months(12, abbreviate = T), unit='month')

#------ Dæsjbord ---------------------

      #skjemanavn <- c('Pasient preop.','Lege preop.')
  vec <- factor(SkjemaOversikt$SkjemaRekkeflg, levels= c(5,10))
    indDato <- which(as.Date(SkjemaOversikt$InnDato) >= datofra12)
      indSkjemastatus <- which(SkjemaOversikt$SkjemaStatus==0)
      #indSkjema <- which(SkjemaOversikt$SkjemaRekkeflg %in% c(5,10,15))
      indEgetSkjema <- which(SkjemaOversikt$AvdRESH == reshID)
      tabKladd <- table(vec[Reduce(intersect, list(indDato, indSkjemastatus,indEgetSkjema))]) #, indSkjema
      names(tabKladd) <- c('Pasientskjema','Legeskjema.')
      t(tabKladd)

  observe({
    output$tabAntOpphEget <- renderTable(
      tabAntOpphShMnd(RegData=RegData, datoTil=datoTil, reshID = reshID, antMnd=12)
      ,rownames = T, digits=0, spacing="xs" )

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


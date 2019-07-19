library(shiny)
library(shiny)
library(magrittr)
library(rygg)


library(shiny)
library(rapbase)
library(markdown)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Degenerativ Rygg"

#----------Hente data og evt. parametre som er statistke i appen----------
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
    registryName <- "rygg"
    dbType <- "mysql"
    query <- paste0('SELECT  ...')
    HovedSkjema <- RyggRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
} #hente data på server

#     if (!exists('RegData')) {
#data('RyggTulledata', package = 'rygg')
reshID <- 601161
dato <- '2019-07-18'
SkjemaOversikt <- read.table(paste0('A:/Rygg/SkjemaOversikt',dato,'.csv'),
                             sep=';', header=T, encoding = 'UTF-8') #IKKE sensitive data. Kan legges i pakken.

#SkjemaOversikt <- plyr::rename(SkjemaOversikt, replace=c('SykehusNavn'='ShNavn'))
#      }
rolle <- 'SC' #LU

RegData <- RyggPreprosess(RegData)
SkjemaOversikt <- RyggPreprosess(RegData = SkjemaOversikt)


#Definere utvalgsinnhold
sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
names(sykehusValg) <- sykehusNavn$x
# sykehusValg <- c(reshID,unique(RegData$ReshId)[sykehusNavn$ix])
# names(sykehusValg) <- c(RegData$ShNavn[match(reshID, RegData$ReshId)], sykehusNavn$x)


enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
diag <- 0:3
names(diag) <- c('Alle', 'Godartede ovarialcyster', 'Endometriose, livmorvegg', 'Endo u livmorvegg')

opMetode <- c('Alle'=0,
              'Laparoskopi'=1,
              'Hysteroskopi'=2,
              'Begge'=3,
              'Tot. lap. hysterektomi (LCD01/LCD04)'=4,
              'Lap. subtotal hysterektomi (LCC11)'=5,
              'Lap. ass. vag. hysterektomi (LCD11)'=6)

alvorKompl <- c(#"Alle"=0,
    "Lite alvorlig"=1,
    "Middels alvorlig"=2,
    "Alvorlig"=3,
    "Dødelig"=4)
hastegrad <- c('Alle'=0,
               'Elektiv'=1,
               'Akutt'=2,
               'Ø-hjelp'=3)


ui <- tagList(
    navbarPage(
        title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                    regTitle),
        windowTitle = regTitle,
        theme = "rap/bootstrap.css",

        #-----Registreringsoversikter------------
        tabPanel("Registreringsoversikter",

                 sidebarPanel(width=3,
                              h3('Utvalg'),
                              conditionalPanel(condition = "input.ark == 'Antall operasjoner'",
                                               dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                         value = Sys.Date(), max = Sys.Date() )
                              ),
                              conditionalPanel(
                                  condition = "input.ark == 'Antall operasjoner'",
                                  selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                              choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                              conditionalPanel(
                                  condition = "input.ark == 'Antall registrerte skjema'",
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
                                          h2("Antall opphold per avdeling"),
                                          uiOutput("undertittelReg"),
                                          p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                          br(),
                                          fluidRow(
                                              tableOutput("tabAntOpphSh")
                                              ,downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned')
                                          )
                                 ),

                                 tabPanel('Antall registrerte skjema',
                                          h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                          p("Velg tidsperiode i menyen til venstre"),
                                          br(),
                                          fluidRow(
                                              tableOutput("tabAntSkjema")
                                              #,downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                          )
                                 )
                     )
                 )

        ), #tab Registreringsoversikter

        tabPanel("Samlerapport"
                 ,
                 tabPanel("Fordeling av mpg",
                          sidebarLayout(
                              sidebarPanel(width = 3,
                                           selectInput(inputId = "varS",
                                                       label = "Variabel:",
                                                       c("mpg", "disp", "hp", "drat", "wt", "qsec")),
                                           sliderInput(inputId = "binsS",
                                                       label = "Antall grupper:",
                                                       min = 1,
                                                       max = 10,
                                                       value = 5),
                                           downloadButton("downloadSamlerapport", "Last ned hele sida!")
                              ),
                              mainPanel(
                                  uiOutput("samlerapport")
                              )
                          )
                 )
        )

    ) # navbarPage
) # tagList

# Define server logic required
server <- function(input, output) {

    # Last inn data
    #regData <- getFakeRegData()

    # Gjenbrukbar funksjon for å bearbeide Rmd til html
    htmlRenderRmd <- function(srcFile, params = list()) {
        # set param needed for report meta processing
        # params <- list(tableFormat="html")
        system.file(srcFile, package="rygg") %>%
            knitr::knit() %>%
            markdown::markdownToHTML(.,
                                     options = c('fragment_only',
                                                 'base64_images',
                                                 'highlight_code')) %>%
            shiny::HTML()
    }

    #----------Tabeller, registreringsoversikter ----------------------

    output$undertittelReg <- renderUI({
        br()
        t1 <- 'Tabellen viser operasjoner '
        h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                       Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                       Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))
        ))})
    observe({
        tabAntOpphShMndAar <- switch(input$tidsenhetReg,
                                     Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
                                     Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))
        #Aar=xtable::xtable(tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg)), digits=0)
        output$tabAntOpphSh <- renderTable(tabAntOpphShMndAar, rownames = T, digits=0, spacing="xs")
        output$lastNed_tabAntOpph <- downloadHandler(
            filename = function(){paste0('tabAntOpph.csv')},
            content = function(file, filename){write.csv2(tabAntOpphShMndAar, file, row.names = T, na = '')
            })



        #RegData som har tilknyttede skjema av ulik type
        AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaOversikt, datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                        skjemastatus=as.numeric(input$skjemastatus))
        output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                           ,rownames = T, digits=0, spacing="xs" )
        output$lastNed_tabAntSkjema <- downloadHandler(
            filename = function(){'tabAntSkjema.csv'},
            content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, na = '')
            })
    })


    ## last ned
    output$downloadSamlerapport <- downloadHandler(
        filename = function() {
            "ryggSamlerapport.html"
        },
        content = function(file) {
            srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                                 package = "rygg"))
            tmpFile <- "tmpSamlerapport.Rmd"
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(srcFile, tmpFile, overwrite = TRUE)
            out <- rmarkdown::render(tmpFile,
                                     output_format =  rmarkdown::html_document(),
                                     params = list(var = input$varS,
                                                   bins = input$binsS),
                                     output_dir = tempdir())
            file.rename(out, file)
        }
    )


}

# Run the application
shinyApp(ui = ui, server = server)

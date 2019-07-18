library(shiny)
library(rapbase)
library(markdown)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Degenerativ Rygg"

ui <- tagList(
  navbarPage(
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Veiledning",
      mainPanel(width = 12,
        #htmlOutput("veiledning", inline = TRUE)
        includeMarkdown('veiledning.md'),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                            organization = uiOutput("appOrgName"))
      )
    ),
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

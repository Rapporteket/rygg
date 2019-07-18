---
title: 'Veiledning'
author: 'Rapporteket'
date: '18. juli 2019'
output: 
  html_document: 
    keep_md: yes
---

# Lag et register i Rapporteket
Beskrivelsen under er ikke nødvendigvis utfyllende og forutsetter kjennskap til RStudio og bruk av git og GitHub. Som en ekstra støtte anbefales [R pacakges](http://r-pkgs.had.co.nz/) av Hadley Wickham og spesielt [beskrivelsen av git og GitHub](http://r-pkgs.had.co.nz/git.html#git-rstudio).

*TEST*

## Prøv templatet
1. Installér pakken [rapRegTemplate](https://github.com/Rapporteket/rapRegTemplate) i RStudio (`devtools::install_github("Rapporteket/rapRegTemplate")`)
1. Hent ned prosjektet [rapRegTemplate](https://github.com/Rapporteket/rapRegTemplate) til RStudio (for mer info, se [her](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects))
1. Åpne fila inst/shinyApps/app1/ui.R og start Shiny-applikasjonen ("Run App")
1. Navigér i applikasjonen for å se på struktur og farger (innhold mangler)


### Alternativ 2: uten Docker for Rapporteket
1. Åpne fila R/GetFakeRegData.R
1. Se at funksjonen returnerer et kjedelig og irrelevant innebygget datasett :-(
1. Prøv funksjonen fra kommandolinja (Console i RStudio), _e.g._ `df <- getFakeRegData()`
1. Sjekk at du får returnert ei dataramme med X observasjoner for Y variabler, _e.g._ `attributes(df)`

## Lag innhold i Shiny-applikasjonen, steg 1
Utgangspunket for de neste stegene er bruk av det innebygde datasettet "mtcars", jf. "Alternativ 2" over.

1. I shiny-applikasjonen, navigér til arkfanen "Figur og tabell"
1. Åpne fila inst/shinyApps/app1/ui.R
1. Bla ned til linja `tabPanel("Figur og tabell"`
1. Kommenter inn linjene under, lagre fila og last applikasjonen på nytt ("Reload App")
1. Sjekk at det er kommet inn GUI-elementer i arkfanen "Figur og tabell" som før var tom
1. Prøv gjerne de brukervalg som er i venstre kolonne
1. Oppgave: gjør endringer i inst/shinyApps/app1/ui.R (på de linjene som nettopp er kommentert inn) slik at maks antall grupper endres fra 10 til 12 i applikasjonen 


## Lag innhold i Shiny-applikasjonen, steg 3
Bruk samme tilnærming som over, men for "Samlerapport". Her er det en del nye elementer, bl.a.

- bruk av en Rmd-fil som rapportmal
- funksjonalitet for nedlasting av rapporten


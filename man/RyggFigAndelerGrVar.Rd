% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggFigAndelerGrVar.R
\name{RyggFigAndelerGrVar}
\alias{RyggFigAndelerGrVar}
\title{Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)}
\usage{
RyggFigAndelerGrVar(
  RegData = 0,
  valgtVar = "alder70",
  datoFra = "2007-01-01",
  datoTil = "3000-12-31",
  aar = 0,
  minald = 0,
  maxald = 130,
  erMann = "",
  hovedkat = 99,
  tidlOp = "",
  hentData = 0,
  preprosess = 1,
  hastegrad = 99,
  enhetsUtvalg = 0,
  grVar = "ShNavn",
  tittel = 1,
  ktr = 0,
  Ngrense = 10,
  reshID = 0,
  outfile = "",
  ...
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.}

\item{datoFra}{Tidligste operasjonsdato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste operasjonsdato i utvalget (vises alltid i figuren).}

\item{aar}{\itemize{
\item Operasjonsår. Kan velge flere
}}

\item{minald}{Alder, fra og med (Standardverdi: 0)}

\item{maxald}{Alder, til og med (Standardverdi: 110)}

\item{erMann}{Kjønn, standard: alt annet enn 0/1 gir begge kjønn
0: Kvinner
1: Menn}

\item{hovedkat}{Hvilken type hovedinngrep, numerisk 0-9, standard: 99, dvs. alle. Valgmuligheter:
\itemize{
\item 0:'Andre inngrep',
\item 1:'Prolapskirurgi',
\item 2:'Midtlinjebevarende dekompr.',
\item 3:'Laminektomi',
\item 4:'Eksp. intersp implantat',
\item 5:'Fusjonskirurgi',
\item 6:'Osteotomi, deformitet',
\item 7:'Fjerning/rev. av implantat',
\item 8:'Skiveprotese',
\item 9:'Spinal stenose',
\item 10:'Degen. spondylolistese'
\item 99: Alle
}}

\item{tidlOp}{tidligere operert? 1: samme nivå, 2: annet nivå, 3: annet og sm. nivå', 4: Primæroperasjon}

\item{hentData}{Gjøre spørring mot database? 0: Nei, bruke RegData (Standard), 1: Ja}

\item{preprosess}{Preprosesser data 0: Nei, 1: Ja (Standard)}

\item{hastegrad}{Hastegrad av operasjon 1: Elektivt, 2: Akutt, 3: Halvøyeblikkelig}

\item{enhetsUtvalg}{Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.}

\item{grVar}{Tekstvariabel som angir hva skal resultatene grupperes på.
ShNavn-sykehus/avdeling
Fylke- Pasienten bor i det akutelle fylket
BoHF - Pasienten bor i boområdene til det angitte HF.
BoRHF - Pasienten bor i boområdene til det angitte RHF.}

\item{tittel}{Vise tittel i figuren eller ikke (0/1). standard:1
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{Ngrense}{Minste antall registreringer for at ei gruppe skal bli vist}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives}
}
\value{
Figur med...
}
\description{
Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
f.eks. sykehus.
Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
Den kan også benyttes til å lage figurer som viser dekningsgrad. Datasettet må da inkluderes i
pakken og navnes DeknXXYY.Rdata, hvor XX er Rygg eller Nakke og YY er årstall.
}
\details{
Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet'
Argumentet \emph{valgtVar} har følgende valgmuligheter:
\itemize{
\item alder70: Pasienter over 75år
\item Antibiotika: Er det gitt antibiotikaprofylakse?
\item ArbstatusPre: Mottar sykepenger, før operasjon?
\item Arbstatus: Mottar sykepenger, etter operasjon?
\item ASA: ASA-grad > II
\item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
\item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
\item BMI: Pasienter med fedme (BMI>30)
\item degSponFusj: Degenerativ spondylolistese operert  med fusjonskirurgi
\item degSponSSSten: Degenerativ spondylolistese og sentral spinal stenose
\item ErstatningPre: Søkt/planlegger å søke erstatning?
\item Fornoyd: Fornøyde pasienter
\item KpInf3Mnd: Sårinfeksjon, pasientrapportert
\item Kp3Mnd: Pasientrapporterte komplikasjoner
\item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd
\item Nytte: Klart bedre
\item OswEndrLav: Forbedring av Oswestry-skår < 13 poeng
\item OswEndr20: Forbedring av Oswestry-skår > 20 poeng
\item OswEndr30pst: Mer enn 30\% forbedring i Oswestry-skår
\item Osw22: Oswestry-skår < 22 poeng
\item Osw48: Oswestry-skår > 48 poeng
\item PeropKomp: Komplikasjon ved operasjon
\item PeropKompDura: Komplikasjon ved operasjon: Durarift
\item Roker: Røyker du?
\item Saardren: Sårdren
\item SmStiPre: Bruker smertestillende før operasjonen
\item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år
\item SympVarighUtstr: Varighet av utstrålende smerter > 1 år
\item tidlOp3: Mer enn to tidligere operasjoner
\item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?
\item Utd: Andel høyskole-/universitetsutdannede
\item Verre Mye verre/verre enn noen gang, 3 mnd.
}
}

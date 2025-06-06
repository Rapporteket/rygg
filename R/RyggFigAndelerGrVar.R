#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#' Den kan også benyttes til å lage figurer som viser dekningsgrad. Datasettet må da inkluderes i
#' pakken og navnes DeknXXYY.Rdata, hvor XX er Rygg eller Nakke og YY er årstall.
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder70: Pasienter over 75år
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item ArbstatusPre: Mottar sykepenger, før operasjon?
#'     \item Arbstatus: Mottar sykepenger, etter operasjon?
#'     \item ASA: ASA-grad > II
#'	 \item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
#'	 \item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
#'     \item BMI: Pasienter med fedme (BMI>30)
#'     \item degSponFusj: Degenerativ spondylolistese operert  med fusjonskirurgi
#'     \item degSponSSSten: Degenerativ spondylolistese og sentral spinal stenose
#'     \item ErstatningPre: Søkt/planlegger å søke erstatning?
#'     \item Fornoyd: Fornøyde pasienter
#'     \item KpInf3Mnd: Sårinfeksjon, pasientrapportert
#'     \item Kp3Mnd: Pasientrapporterte komplikasjoner
#'     \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd
#'     \item Nytte: Klart bedre
#'	 \item OswEndrLav: Forbedring av Oswestry-skår < 13 poeng
#'	 \item OswEndr20: Forbedring av Oswestry-skår > 20 poeng
#'	 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår
#'	 \item Osw22: Oswestry-skår < 22 poeng
#'	 \item Osw48: Oswestry-skår > 48 poeng
#'     \item PeropKomp: Komplikasjon ved operasjon
#'     \item PeropKompDura: Komplikasjon ved operasjon: Durarift
#'     \item Roker: Røyker du?
#'     \item Saardren: Sårdren
#'     \item SmStiPre: Bruker smertestillende før operasjonen
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år
#'     \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år
#'     \item tidlOp3: Mer enn to tidligere operasjoner
#'     \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?
#'     \item Utd: Andel høyskole-/universitetsutdannede
#'     \item Verre Mye verre/verre enn noen gang, 3 mnd.
#'		}
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på.
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#'                BoHF - Pasienten bor i boområdene til det angitte HF.
#'                BoRHF - Pasienten bor i boområdene til det angitte RHF.
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
#' @importFrom rapFigurer figtype
#' @return Figur med...
#'
#' @export

RyggFigAndelerGrVar <- function(RegData=0, valgtVar='alder70', datoFra='2007-01-01', datoTil='3000-12-31', aar=0,
                                minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hentData=0,
                                preprosess=1, hastegrad=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0,
                                Ngrense=10, reshID=0, outfile='', ...) {
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('AndelPrShus: ',valgtVar))
  }

  if (length(grep('dekn',valgtVar)) == 1) { #Bruke vedlagte dekningsdata
    #Dekningsgradsfigur

    if (valgtVar == 'dekn23Nakke') {
      Tittel <- 'Dekningsgrad, NKR Degenerativ Nakke, 2023'
      xAkseTxt <- 'dekningsgrad, Nakke'
    }
    if (valgtVar == 'dekn23Rygg') {
      Tittel <- 'Dekningsgrad, NKR Degenerativ Rygg, 2023'
      xAkseTxt <- 'dekningsgrad, Rygg'
    }
    if (valgtVar == 'dekn21Rygg') {
      RegData <- read.table('Rygg_dg2021.csv', sep=';', header=T, stringsAsFactors = FALSE, dec = ",")
      Tittel <- 'Dekningsgrad, NKR Degenerativ Rygg, 2021'
      xAkseTxt <- 'dekningsgrad, Rygg'
    }
    if (valgtVar == 'dekn19Nakke') {
      RegData <- read.table('./data-raw/DG2019Nakke.csv', sep=';', header=T, stringsAsFactors = FALSE)
      Tittel <- 'Dekningsgrad, NKR Degenerativ Nakke, 2019'
      xAkseTxt <- 'dekningsgrad, Rygg'
    }

    Ngr <- RegData$Total
    Ngrtxt <- as.character(Ngr)
    #indLandet <- which(RegData$ShNavn== 'Hele landet')
    AndelHele <- 100*sum(RegData$RegNKR)/sum(RegData$Total)   #RegData$DG_nkr[indLandet]
    AndelerGr <- 100*RegData$RegNKR/RegData$Total #RegData$DG_nkr #[-indLandet]
    fargepalett='BlaaOff'
    utvalgTxt <- ''
    sortAvtagende <- T
    AntGr <- length(AndelerGr)
    GrNavn <- RegData$ShNavn
    hovedgrTxt <- 'Hele landet'
    N <- sum(RegData$Total) #RegData$N[indLandet]
    KImaalGrenser <- c(0,60,80,100)
    grVar <- 'ShNavn'
    Ngrense <- 0

  } else {

    if (hentData == 1) {
      RegData <- RyggRegDataSQLV2V3()  #RyggRegDataSQL()
    }

    # Preprosessere data
    if ((preprosess==1) & (dim(RegData)[1] >1)){
      RegData <- RyggPreprosess(RegData=RegData)
    }
    #------- Tilrettelegge variable
    RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr,
                                       hovedkat= hovedkat, figurtype = 'andelGrVar')
    RegData <- RyggVarSpes$RegData
    sortAvtagende <- RyggVarSpes$sortAvtagende
    KImaalGrenser <- RyggVarSpes$KImaalGrenser

    RegData[ ,grVar] <- factor(RegData[ ,grVar])

    #------- Gjøre utvalg
    if (reshID==0) {enhetsUtvalg <- 0}

    #Vise riktig utvalg for kvalitetsindikatorer
    if (RyggVarSpes$hovedkat[1] != 99) {
      hovedkat <- RyggVarSpes$hovedkat
    }
    #print(RyggVarSpes$hovedkat)

    if (valgtVar == 'trombProfylLettKI') {
      erMann=1
      # hovedkat <- 1:2
    }
    if (valgtVar %in% c('OswEndr20ProKI', 'OswEndr30pstSSKI')) {
      hastegrad = 1
      tidlOp = 4}

    RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                                minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                                hovedkat=hovedkat, hastegrad=hastegrad, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg)
    fargepalett <- RyggUtvalg$fargepalett
    hovedgrTxt <- RyggUtvalg$hovedgrTxt
    utvalgTxt <- RyggUtvalg$utvalgTxt
    ind <- RyggUtvalg$ind
    RegData <- RyggUtvalg$RegData
    #}
    #---------------Beregninger
    # Variabelen Variabel er definert som indikatorvariabel for den valgte variabelen.
    #Ikke dekningsgradsfigur
    if(dim(RegData)[1] > 0) {
      RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
      RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
      Ngr <- table(RegData[ ,grVar])
      Ngrtxt <- as.character(Ngr)
    }	else {Ngr <- 0}

    N <- dim(RegData)[1]
    AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
    AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
    AndelerGr <- round(100*tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)/Ngr,2)

    GrNavn <- names(Ngr)
    xAkseTxt <- "Andel opphold (%)"

    indGrUt <- which(Ngr < Ngrense)
    if (sum(indGrUt)>0) {
      AndelGrUt <- sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
      AndelerGr <- c(AndelerGr[-indGrUt],AndelGrUt) #AndelerGr[indGrUt] <- NA
      GrUtNavn <- paste0(length(indGrUt), ' avd. med N<',Ngrense)
      Ngrtxt <- c(Ngr[-indGrUt],sum(Ngr[indGrUt]))  #Ngrtxt[indGrUt] <- paste0('<', Ngrense)
      GrNavn <- c(GrNavn[-indGrUt], GrUtNavn) #paste0(c(GrNavn[-indGrUt], GrUtNavn),' (',Ngrtxt , ')')
    }
    if (tittel==0) {Tittel<-''} else {Tittel <- RyggVarSpes$tittel}

    if (!is.na(KImaalGrenser[1]) & KImaalGrenser[1] == 'landsgj'){
      landsgj <- round(100*prop.table(table(RegData$Variabel))[2], 1)
      KImaalGrenser <- c(0,landsgj,100)}
    fargepalett <- RyggUtvalg$fargepalett
  }
  sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE)
  AndelerGrSort <- AndelerGr[sortInd]
  GrNavnSort <- GrNavn[sortInd]
  Ngrtxt <- Ngrtxt[sortInd]

  andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %')
  andeltxt <- andeltxtUsort[sortInd]


  FigDataParam <- list(AggVerdier=AndelerGrSort,
                       AggTot=AndelHele,
                       N=N,
                       Ngr=as.numeric(Ngrtxt),
                       soyletxt=andeltxt,
                       grtxt=GrNavnSort,
                       Tittel=Tittel,
                       utvalgTxt=utvalgTxt,
                       fargepalett =fargepalett

  )


  #-----------Figur---------------------------------------
  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (!is.null(dim(RegData))) { #>0
      tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=Tittel)
    text(0.5, 0.6, tekst, cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}

  } else {

    #--------------------------FIGUR---------------------------------------------------
    #Innparametre: ...
    #----------- Figurparametre ------------------------------
    cexShNavn <- 0.9 #0.85

    FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=fargepalett)
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.8)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
    pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                       xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                       las=1, cex.names=cexShNavn*0.9))
    ybunn <- 0.1
    ytopp <- max(pos)+0.4

    #Legge på målnivå
    if (!is.na(KImaalGrenser[1])) {
      antMaalNivaa <- length(KImaalGrenser)-1
      rekkef <- 1:antMaalNivaa
      if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
      fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef]
      tetth <- c(100, 70,15)[rekkef]
      maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
      if (antMaalNivaa==3) {maalOppTxt[2] <- 'Moderat' }
      rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
           ytop=max(pos)+0.4, col = fargerMaalNiva[1:antMaalNivaa],
           density = tetth, angle = 60, border = NA) #add = TRUE, #pos[AntGrNgr+1],

      legPos <- ifelse(AntGr < 31, ifelse(AntGr < 15, -1, -2.5), -3.5)
      legend(x=xmax, y=ytopp, xjust=1, yjust=0, ncol=antMaalNivaa+1,
             density = c(NA, tetth),
             angle = c(NA,rep(60, antMaalNivaa)),
             fill=c('white', fargerMaalNiva[1:antMaalNivaa]),
             xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
             legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa]))
    }
    pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4],
                       xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                       las=1, cex.names=cexShNavn*0.9, add=T))
    mtext('Andel (%)', side=1, line=2)
    #Linje for hele landet/utvalget:
    lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
    legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
           legend=paste0(hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
           bty='o', bg='white', box.col='white')
    mtext(at=pos+max(pos)*0.0045, paste0(GrNavnSort,' (',Ngrtxt , ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg


    title(Tittel, line=1, font.main=1, cex.main=1.3)

    text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
         las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

    mtext(at=max(pos)+0.4*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=1, adj=1, line=0.25)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
  }
  return(invisible(FigDataParam))
}


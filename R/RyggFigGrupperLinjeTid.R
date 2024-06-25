#' Tidstrend av andel opphold
#'
#' Funksjonen lager linjediagram som viser utvikling over tid av valgt variabel
#' for valgte grupper, filtrert på de utvalg som er gjort.
#' Per nå (23.mai 2024) må funksjonen kjøres manuelt. Ønsker å gjøre den mer generisk. Avventer registerets behov.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item xx
#'     \item OswEndr30pst12mnd: Forbedring av Oswestry-skår > 30 prosent
#'    }
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#' @param enhetsUtvalg 0: hele landet, 2: eget
#'
#' @return Figur som viser tidstrend, enten andel for hver av valgte grupper eller for en valgt responsvariabel
#'
#' @export
RyggFigGrupperLinjelTid <- function(RegData, valgtVar = 'gruppeAndel' , gr1='', gr2='', gr3 = '',
                            datoFra='2011-01-01', datoTil=Sys.Date(), aar=0,
                            tidsenhet='Aar', hovedkat = 99, ktr = 0, tidlOp = 99, tittel = 1,
                            minald=0, maxald=130, erMann=99, reshID=0, outfile='', hastegrad=99,
                            enhetsUtvalg=0, preprosess=1, hentData=0, lagFig=1, ... ) {
  valgtVar='OswEndr30pst12mnd'
  gr1 <- 2 # gr kun tilpasset hovedkategorier
  gr2 <- 5
  tidsenhet <- 'Aar'

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('AndelPrTidsenhet: ',valgtVar))
  }

  if (hentData == 1) {
    RegData <- RyggRegDataSQLV2V3()
  }

  # Preprosessering av data. I samledokument gjøre dette i samledokumentet.
  if (preprosess==1){
    RegData <- RyggPreprosess(RegData=RegData)	#, reshID=reshID)
  }

    #------- Tilrettelegge responsvariabel
    RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, #datoTil=datoTil,
                                       figurtype = 'andelTid')
    RegData <- RyggVarSpes$RegData
    sortAvtagende <- RyggVarSpes$sortAvtagende
    varTxt <- RyggVarSpes$varTxt
    tittel <- RyggVarSpes$tittel

  #------- Gjøre utvalg
  if (reshID==0) {enhetsUtvalg <- 0}

  RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                              minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                               hastegrad=hastegrad, tidlOp=tidlOp, hovedkat = hovedkat,
                              enhetsUtvalg=enhetsUtvalg) #, grType=grType
  RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, datoFra = '2010-01-01', datoTil='2023-12-31', hovedkat = 10)
  utvalgTxt <- RyggUtvalg$utvalgTxt
  RegData <- RyggUtvalg$RegData

  RegDataGr1 <- RyggUtvalgEnh(RegData=RegData, hovedkat = gr1)$RegData
  RegDataGr2 <- RyggUtvalgEnh(RegData=RegData, hovedkat = gr2)$RegData
  Ngr <- c(dim(RegDataGr1)[1], dim(RegDataGr2)[1])

  #Hvis for få observasjoner..
  if (min(Ngr < Ngrense)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste0('variabel: ', valgtVar))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {

   # grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
  # yAkseTxt <- 'Andel (%)'
  # vektor <- c('Aar','Halvaar','Kvartal','Mnd')
  # xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
  #                    [which(tidsenhet==vektor)])



    #------------------------Klargjøre tidsenhet--------------
    RegDataMTidsenh <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
    RegData <- RegDataMTidsenh$RegData
    tidtxt <- RegDataMTidsenh$tidtxt
    xAkseTxt <- switch(tidsenhet,
                       Aar='Operasjonsår',
                       Halvaar = 'Operasjonsår og -halvår',
                       Kvartal = 'Operasjonsår og -kvartal',
                       Mnd='Operasjonsår og -måned')

    #--------------- Gjøre beregninger ------------------------------

    RegDataGr1Utv <- RyggUtvalgEnh(RegData=RegData, hovedkat = gr1)
    RegDataGr1 <- RegDataGr1Utv$RegData
    RegDataGr2Utv <- RyggUtvalgEnh(RegData=RegData, hovedkat = gr2)
    RegDataGr2 <- RegDataGr2Utv$RegData

    NTidGr1 <- tapply(RegDataGr1$Variabel, RegDataGr1$TidsEnhet, length) #Tot. ant. per år
    NTidVarGr1 <- tapply(RegDataGr1$Variabel, RegDataGr1$TidsEnhet, sum, na.rm=T) #Ant. hendelser per år
    NTidGr2 <- tapply(RegDataGr2$Variabel, RegDataGr2$TidsEnhet, length) #Tot. ant. per år
    NTidVarGr2 <- tapply(RegDataGr2$Variabel, RegDataGr2$TidsEnhet, sum, na.rm=T) #Ant. hendelser per år
    ResGr1 <- NTidVarGr1/NTidGr1*100
    ResGr2 <- NTidVarGr2/NTidGr2*100

    if (valgtVar == 'gruppeAndel') {
      tittel <- 'Operasjonstype'
      NTidAlle <- tapply(RegData$Variabel, RegData$TidsEnhet, length) #Tot. ant. per år
      ResGr1 <- NTidGr1/NTidAlle*100
      ResGr2 <- NTidGr2/NTidAlle*100
    }

    #-----------Figur---------------------------------------
    if (lagFig == 1) {

      #Plottspesifikke parametre:
      outfile <- 'ODI30vsOptypeAar.pdf'

      FigTypUt <- rapFigurer::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
      farger <- FigTypUt$farger
      fargeGr1 <- farger[1]
      fargeGr2 <- farger[3]
      NutvTxt <- length(utvalgTxt)
      hmarg <- 0.04+0.01*NutvTxt
      par('fig' = c(0,1,0,1-hmarg))
      cexleg <- 1	#St?rrelse p? legendtekst
      xskala <- 1:length(tidtxt)
      xmax <- max(xskala)


      ymax <- min(119, 1.25*max(c(ResGr1, ResGr2),na.rm=T))
      plot(xskala, ResGr1,  font.main=1,  type='o', pch="'", col='white',
           xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,
           cex=2, xlab='Operasjonsår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

      #Legge på linjer i plottet.
      grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

      axis(side=1, at = xskala, labels = tidtxt)

      title(tittel, line=1, font.main=1)

      lines(xskala, ResGr1, col=fargeGr1, lwd=3)
      #points(xskala, ResGr1, pch="'", cex=2, col=fargeGr1)
      #text(xskala, ResGr1, pos=3, NTidGr1, cex=0.9, col=fargeGr1)
      if (gr2 != 0) {
        lines(xskala, ResGr2, col=fargeGr2, lwd=3, lty=3)
        #points(xskala, ResGr2, pch="'", cex=2, col=fargeGr2)
      }

      Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
      #text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste0(RegDataGr1Utv$utvalgTxt[2], ' (N=', sum(NTidGr1, na.rm = T), ')'),
                                       paste0(RegDataGr2Utv$utvalgTxt[2], ' (N=', sum(NTidGr2, na.rm = T), ')')#, Ttxt
                                     ), bty='n', ncol=1, cex=cexleg,
               col=c(fargeGr1, fargeGr2), lwd=3, lty = c(1,3))


      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[3], line=c(3+0.8*((NutvTxt-1):0)))

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
      #------------------------------------------------------------------------------

    }	#end else statement
  }



  #                  }
  return(invisible(FigDataParam))

}	#end function


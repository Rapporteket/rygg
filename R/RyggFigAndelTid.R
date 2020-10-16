#' Tidstrend av andel opphold
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder70: Alder over 70år,
#'     \item degSponFusj: Andel av degenerativ spondylolistese er operert  med fusjonskirurgi (hovedinngr=5)
#'     \item KpInf3Mnd: Sårinfeksjon, pasientrapportert
#'     \item OswEndr20: Forbedring av Oswestry-skår > 20 poeng
#'    }
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år.
#'
#' @export
RyggFigAndelTid <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil=Sys.Date(), aar=0,
                            tidsenhet='Aar', hovedkat = 99, ktr = 0, tidlOp = 99, tittel = 1,
                        minald=0, maxald=130, erMann=99, reshID=0, outfile='', hastegrad=99,
                        enhetsUtvalg=0, preprosess=1, hentData=0, lagFig=1, offData=0,... ) {

   if ("session" %in% names(list(...))) {
      raplog::repLogger(session = list(...)[["session"]], msg = paste0('AndelPrTidsenhet: ',valgtVar))
   }

      if (hentData == 1) {
            RegData <- RyggRegDataSQL()
      }
      if (offData == 1) {
            utvalgsInfo <- RegData$utvalgsInfo
            KImaal <- RegData$KImaalGrenser
            sortAvtagende <- RegData$sortAvtagende
            tittel <- RegData$tittel
            RegData <- RegData$RyggRegData01Off
      }

      # Preprosessering av data. I samledokument gjøre dette i samledokumentet. Off01-data er preprosessert.
      if (offData==1) {preprosess <- 0}
      if (preprosess==1){
            RegData <- RyggPreprosess(RegData=RegData)	#, reshID=reshID)
      }


      #------- Tilrettelegge variable
      varTxt <- ''
      if (offData == 0) {
            RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar,
                                               datoTil=datoTil, ktr=ktr, figurtype = 'andelTid')
            RegData <- RyggVarSpes$RegData
            sortAvtagende <- RyggVarSpes$sortAvtagende
            varTxt <- RyggVarSpes$varTxt
            KImaal <- RyggVarSpes$KImaalGrenser
            tittel <- RyggVarSpes$tittel
      }


      #------- Gjøre utvalg
      smltxt <- ''
      medSml <- 0

      #
         if(ktr==2) {datoFra <- as.Date(min(as.Date(datoTil)-500, as.Date(datoFra)))}

      if (offData == 0) {
            if (reshID==0) {enhetsUtvalg <- 0}
            RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                                      minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                                      hovedkat = hovedkat, hastegrad=hastegrad, tidlOp=tidlOp,
                                      enhetsUtvalg=enhetsUtvalg) #, grType=grType
            smltxt <- RyggUtvalg$smltxt
            medSml <- RyggUtvalg$medSml
            utvalgTxt <- RyggUtvalg$utvalgTxt
            ind <- RyggUtvalg$ind
      }
      if (offData == 1) {RyggUtvalg <- RyggUtvalgOff(RegData=RegData, aldGr=aldGr, aar=aar, erMann=erMann,
                                                   grType=grType)

            utvalgTxt <- c(RyggUtvalg$utvalgsTxt, utvalgsInfo)
            ind <- list(Hoved = 1:dim(RegData)[1], Rest = NULL)
      }
      RegData <- RyggUtvalg$RegData

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

      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))


      NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per år
      NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per år
      AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100
      NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
      NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
      AggVerdier$Rest <- NAarHendRest/NAarRest*100
      Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)

      #grtxt <- paste0(rev(RyggVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)')
      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      yAkseTxt <- 'Andel (%)'
      vektor <- c('Aar','Halvaar','Kvartal','Mnd')
      xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
                         [which(tidsenhet==vektor)])

      hovedgrTxt <- RyggUtvalg$hovedgrTxt

      FigDataParam <- list(AggVerdier=AggVerdier, N=N,
                           Ngr=list('Hoved' = NAarHoved, 'Rest'= NAarHendRest),
                           Nvar = Ngr,
                           KImaal = KImaal,
                           #soyletxt=soyletxt,
                           grtxt2=grtxt2,
                           varTxt=varTxt,
                           tidtxt=tidtxt, #RyggVarSpes$grtxt,
                           tittel=tittel,
                           retn='V',
                           xAkseTxt=xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=RyggUtvalg$utvalgTxt,
                           fargepalett=RyggUtvalg$fargepalett,
                           medSml=medSml,
                           hovedgrTxt=hovedgrTxt,
                           smltxt=RyggUtvalg$smltxt)


      if (lagFig == 1) {
#            RyggFigTidAndel(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt=RyggUtvalg$hovedgrTxt,
#                           smltxt=RyggUtvalg$smltxt, Ngr = Ngr, KImaal = KImaal, N=N, retn='V',
#                           utvalgTxt=utvalgTxt, tidtxt=tidtxt, varTxt=varTxt, grtxt2=grtxt2, medSml=medSml,
#                           xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt,
#                           outfile=outfile)


                  #-----------Figur---------------------------------------
                  #Hvis for f? observasjoner..
                  if (N$Hoved < 10 | (medSml ==1 & N$Rest<10)) {
                        FigTypUt <- figtype(outfile)
                        farger <- FigTypUt$farger
                        plot.new()
                        title(main=paste0('variabel: ', valgtVar))	#, line=-6)
                        legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                        text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
                        text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
                        if ( outfile != '') {dev.off()}

                  } else {

                        #Plottspesifikke parametre:
                        FigTypUt <- figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
                        farger <- FigTypUt$farger
                        fargeHoved <- farger[3]
                        fargeRest <- farger[1]
                        NutvTxt <- length(utvalgTxt)
                        hmarg <- 0.04+0.01*NutvTxt
                        par('fig' = c(0,1,0,1-hmarg))
                        cexleg <- 1	#St?rrelse p? legendtekst
                       # ylabtext <- "Andel (%)"
                        xskala <- 1:length(tidtxt)
                        xmax <- max(xskala)


                        ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
                        plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
                             xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
                             cex=2, xlab='Operasjonsår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

                        #Legge på linjer i plottet.
                        grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

                        axis(side=1, at = xskala, labels = tidtxt)

                        title(tittel, line=1, font.main=1)


                        lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
                        points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
                        text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)
                        if (medSml==1) {
                        lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
                        points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)
                        }
                        #KImål
                        if (valgtVar=='SympVarighUtstr') {
                        lines(xskala, rep(KImaal[2],length(xskala)), col= '#FF7260', lwd=3)
                        text(max(xskala), KImaal[2], pos=4, 'Mål', cex=0.9, col='#FF7260')
                        }

                        Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
                        if (medSml == 1) {
                              text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
                              legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                                             paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1, cex=cexleg,
                                     col=c(fargeHoved, fargeRest, NA), lwd=3)
                        } else {
                              legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
                                     col=c(fargeHoved, NA), lwd=3, bty='n')
                        }

                        #Tekst som angir hvilket utvalg som er gjort
                        mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

                        par('fig'=c(0, 1, 0, 1))
                        if ( outfile != '') {dev.off()}
                        #------------------------------------------------------------------------------

                  }	#end else statement
            }



#                  }
      return(invisible(FigDataParam))

}	#end function




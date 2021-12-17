#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggFigAndelerGrVar
#' @inheritParams RyggUtvalgEnh
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder:
#'     \item Liggedogn: Liggetid
#'     \item OswEndr: Endring i Oswestry fra før til etter
#'     \item SmBeinEndr: Forbedring av beinsmerter fra før til etter
#'     \item SmRyEndr: Forbedring av ryggsmerter fra før til etter
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


RyggFigGjsnGrVar <- function(RegData, valgtVar, valgtMaal='Gjsn', datoFra='2007-01-01', datoTil='3000-12-31', aar=0,
                             minald=0, maxald=130, erMann=99, hovedkat=99, tidlOp=99, hentData=0, preprosess=1,
                             hastegrad=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0, Ngrense=10, medKI=1, reshID=0,
                             outfile='',...) {


  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0("AndelGrVar: ", valgtVar))
  }

if (hentData == 1) {
  RegData <- RyggRegDataSQL()
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøres dette i samledokumentet)
if (preprosess == 1){
       RegData <- RyggPreprosess(RegData=RegData) #, reshID=reshID)
     }

#------- Tilrettelegge variable
	RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype = 'gjsnGrVar')
	RegData <- RyggVarSpes$RegData
    sortAvtagende <- RyggVarSpes$sortAvtagende
    KImaal <- RyggVarSpes$KImaal
    KImaaltxt <- RyggVarSpes$KImaaltxt


#------- Gjøre utvalg
RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                            minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                            hovedkat = hovedkat, hastegrad=hastegrad, tidlOp=tidlOp,
                            enhetsUtvalg=enhetsUtvalg) #, grType=grType
utvalgTxt <- RyggUtvalg$utvalgTxt
hovedgrTxt <- RyggUtvalg$hovedgrTxt
RegData <- RyggUtvalg$RegData

'%i%' <- intersect


#---------------Beregninger
RegData[ ,grVar] <- as.character(RegData[ ,grVar])
#Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
#grupper komme med uansett om de ikke har registreringer.

RegData$grVar <- RegData[,grVar]
#Ngr <- table(RegData$grVar)
if(dim(RegData)[1]>0) {
  Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

t1 <- switch(valgtMaal,
			Med = 'Median ',
			Gjsn = 'Gjennomsnittlig ')

tittel <- paste0(t1, RyggVarSpes$tittel)

N <- dim(RegData)[1]

indGrUt <- which(Ngr < Ngrense) #Grupper som har for få observasjoner
ShUt <- names(Ngr[indGrUt])

antGrUt <- length(indGrUt)
antGr <- length(Ngr)-antGrUt
if (antGrUt==0) { indGrUt <- length(Ngr)+1}
indUt <- which(RegData$grVar %in% ShUt)
indMed <- which(!(RegData$grVar %in% ShUt))
Nut <- ifelse(antGrUt==0, 0, length(indUt)) #ifelse(antGrUt==0, NULL, length(indUt))
#antGrUt <- ifelse(Nut==0, NULL, length(indGrUt))
Ngr <- Ngr[-indGrUt]
GrNavn <- names(Ngr)


KIHele <- c(0,0)
KIned <- c(0,0)
KIhele <- c(0,0)
MidtUt <- NULL

#Kommer ut ferdig sortert!

if (valgtMaal=='Med') {
      medKI <- 0 #!!!!!!!!MÅ SJEKKE NÆRMERE AT KONFIDENSINTERVALLER FOR MEDIAN BLIR RIKTIGE!!
      #For routine use, I recommend the groupwiseMedian function in the rcompanion package,
      #with either the BCa or the percentile method.
      #The BCa (bias corrected, accelerated) is often cited as the best for theoretical reasons.
      #The percentile method is also cited as typically good.
      #If you get the “extreme order statistics used as endpoints” warning message, use a different test.
      #For small data sets, the interval from BCa may be wider than for some other methods.
      #Stats <- groupwiseMedian(data = RegData[indMed,], var='Variabel', group='grVar' , percentile=TRUE, R=4999) #bca=TRUE
      #Bootstrap krever relativt mange trekkinger (5000+) og tar lang tid.
      #Det ser ut til at N-tilnærminga er ok også for små N.
      Median <- tapply(RegData$Variabel[indMed], RegData[indMed ,grVar], median, na.rm=T)
      length(tapply(RegData$Variabel[indMed], RegData[indMed ,grVar], median, na.rm=T))
      #N-tilnærming:
      KIgr <- matrix(0,antGr, 2)
      for (k in 1:antGr) {
            obsNed <- ceiling(as.numeric(Ngr[k])/2 - 1.96*sqrt(as.numeric(Ngr[k])/4))
            obsOpp <- ceiling(as.numeric(Ngr[k])/2 + 1.96*sqrt(as.numeric(Ngr[k])/4))
            KIgr[k,] <- sort(RegData$Variabel[which(RegData$grVar == names(Ngr[k]))])[c(obsNed,obsOpp)]
      }

      MedianUt <- median(RegData$Variabel[indUt])
      KIUt <- sort(RegData$Variabel[indUt])[ceiling(Nut/2 +c(-1,1)*1.96*sqrt(Nut/4))]

      sortInd <- order(c(Median, MedianUt), decreasing=RyggVarSpes$sortAvtagende, na.last = FALSE)

      MidtHele <- median(RegData$Variabel)
      KIHele <- sort(RegData$Variabel)[ceiling(N/2 +c(-1,1)*1.96*sqrt(N/4))]

      Midt <- c(Median, MedianUt)[sortInd] #as.numeric(Gjsn[sortInd])
      KIned <- c(KIgr[,1], KIUt[1])[sortInd]
      KIopp <- c(KIgr[,2], KIUt[2])[sortInd]


	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
#roughly a 95% confidence interval for the difference in two medians.
	}

if (valgtMaal=='Gjsn') {	#Gjennomsnitt er standard, men må velges.
      Gjsn <- tapply(RegData$Variabel[indMed], RegData[indMed, grVar], mean, na.rm=T)
	SE <- tapply(RegData$Variabel[indMed], RegData[indMed, grVar], sd, na.rm=T)/sqrt(Ngr[Ngr >= Ngrense]) #

	MidtUt <- ifelse(length(indUt>0), mean(RegData$Variabel[indUt]), NA)
	KIUt <- if (length(indUt)>0) {MidtUt + sd(RegData$Variabel[indUt])/sqrt(Nut)*c(-2,2)} else {0}

	sortInd <- order(c(Gjsn, MidtUt), decreasing=RyggVarSpes$sortAvtagende, na.last = FALSE)

	MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
	KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)

	Midt <- c(Gjsn, MidtUt)[sortInd] #as.numeric(Gjsn[sortInd])
	KIned <- c(Gjsn - 2*SE, KIUt[1])[sortInd]
	KIopp <- c(Gjsn + 2*SE, KIUt[2])[sortInd]
	}

#navnNut <- ifelse(Nut<Ngrense, NULL,'')
Ngr <- c(as.character(Ngr), Nut) #Ngr[sortInd]
Ngrtxt <- paste0(' (', Ngr,')' ) #[-indGrUt]
GrNavnSort <- paste0(c(GrNavn, paste0(antGrUt, ' avdelinger med N<', Ngrense))[sortInd], Ngrtxt[sortInd])
soyletxt <- sprintf('%.1f', Midt)
antGr <- length(GrNavnSort)

AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)




#Se RyggFigSoyler for forklaring av innhold i lista GjsnGrVarData
GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                         N=list(Hoved=N),
                         Ngr=Ngr,
                         grtxt2='',
				 medKI=medKI,
				 KImaal = KImaal,
                         soyletxt=soyletxt,
                         grtxt=GrNavnSort,
				 valgtMaal=valgtMaal,
                         tittel=tittel,    #RyggVarSpes$tittel,
                         #yAkseTxt=yAkseTxt,
                         retn='H',
                         xAkseTxt=RyggVarSpes$xAkseTxt,
                         utvalgTxt=RyggUtvalg$utvalgTxt,
                         fargepalett=RyggUtvalg$fargepalett)


#FigDataParam skal inn som enkeltparametre i funksjonskallet
lagFig <- 1
if (lagFig == 1) {
cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
AggTot <- MidtHele

#---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 )
    #|(grVar=='' & length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3)))
    {
	#-----------Figur---------------------------------------
      FigTypUt <- rapFigurer::figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	tekst <- 'For få registreringer'
	text(0.5, 0.6, tekst, cex=1.2)
	if ( outfile != '') {dev.off()}

} else {


	#Plottspesifikke parametre:
	#Høyde må avhenge av antall grupper
	hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
	FigTypUt <- rapFigurer::figtype(outfile, height=hoyde, fargepalett=RyggUtvalg$fargepalett)
	#Tilpasse marger for å kunne skrive utvalgsteksten
	NutvTxt <- length(utvalgTxt)
	vmarg <- switch('H', V=0.04, H=min(1,max(0, strwidth(GrNavnSort, units='figure', cex=cexgr)*0.75)))
	#NB: strwidth oppfører seg ulikt avh. av device...
	par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med


	farger <- FigTypUt$farger
	fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
	fargeRest <- farger[3]
	graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
	lwdRest <- 3	#tykkelse på linja som repr. landet
	cexleg <- 0.9	#Størrelse på legendtekst


	#Definerer disse i beregningsfunksjonen?
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- ifelse(valgtMaal=='Andel', min(xmax, 100), xmax) 	#100 som maks bare hvis andelsfigur..
	  ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved)

	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                     xlab=RyggVarSpes$xAkseTxt, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
	  indOK <- which(AggVerdier$Hoved>=0)
	  posOK <- pos[indOK]
	  posOver <- max(pos)+0.35*log(max(pos))
	  posDiff <- 1.2*(pos[1]-pos[2])
	  posOK <- pos[indOK]
	  minpos <- min(posOK)-0.7
	  maxpos <- max(posOK)+0.7

	  if (medKI == 1) {	#Legge på konf.int for hele populasjonen
	        #options(warn=-1)	#Unngå melding om KI med lengde 0
	        KIHele <- AggVerdier$KIHele
	        AntGr <- length(which(AggVerdier$Hoved>0))
	        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
	                c(minpos, maxpos, maxpos, minpos))
	  }

	if (grVar %in% c('ShNavn')) {
	      #GrNavnSort <- rev(GrNavnSort)
	      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      #Linje for hele landet/utvalget:
	      lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55),
	      #Linje for kvalitetsindikatormål:
	      if (!is.na(KImaal)) {
	            lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55),
	            text(x=KImaal, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0))
	      }
	      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
	              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
	      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
	      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
	      }


	if (medKI == 1) {	#Legge på konf.int for hver enkelt gruppe/sykehus
	  #Bytte ut 0'er med NA for å unngå advarsel?
	  #fjernes <- (AggVerdier$KIned==0 & AggVerdier$KIopp==0)
	  med <- !(AggVerdier$KIned==0 & AggVerdier$KIopp==0)
	  arrows(x0=AggVerdier$KIned[med], y0=pos[med], x1=AggVerdier$KIopp[med], y1=pos[med],
	         length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	      # arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIopp, y1=pos,
	      #        length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	      # arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIned, y1=pos,
	      #        length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	}
	#------Tegnforklaring (legend)--------
	if (valgtMaal %in% c('Gjsn', 'Med')) { #Sentralmålfigur
	  if (medKI == 0) { #Hopper over hvis ikke valgtMaal er oppfylt
	            TXT <- paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N)
      	      legend(xmax/4, posOver+posDiff, TXT, fill=NA,  border=NA, lwd=2.5, xpd=TRUE, #inset=c(-0.1,0),
      	             col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      } else {
	            TXT <- c(paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N),
	                     paste0('95% konf.int., ', hovedgrTxt,  ' (', # 'sykehus..
	                            sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
      	      legend(xmax/4, posOver+2*posDiff, TXT, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
      	             col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      }
	}

      #Legge på gruppe/søylenavn
      #if (figurtype  == 'andeler') {GrNavnSort <- paste(GrNavnSort, grtxt2, sep='\n')}

            mtext(at=pos+0.05, text=GrNavnSort, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	  	title(tittel, line=1.5) #cex.main=1.3)

	#Tekst som angir hvilket utvalg som er gjort
	avst <- 0.8
	utvpos <- 3	#Startlinje for teksten
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

	par('fig'=c(0, 1, 0, 1))
	if ( outfile != '') {dev.off()}

}  #Figur
}

return(invisible(GjsnGrVarData))

}




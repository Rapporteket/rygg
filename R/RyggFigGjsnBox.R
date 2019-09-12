#' Utvikling over tid eller som funksjon av variabelen for gjennomsnitt/median av valgt variabel
#'
#' Figuren viser gjennomsnitt/median per år eller gruppe (eksempelvis pre-verdier) for valgt variabel.
#' I bakgrunn vises konfidensintervall for resten av landet. Eventuelt videreutvikle denne til å ha
#' en inn-parameter som sier noe om X-aksen skal være år, måned, pre-verdi eller noe annet.
#'
#' Skala for de ulike livskvalitetsmålene er definert slik:
#'    \itemize{
#'		\item EQ5D: Skala fra -0.594 tl 1, jo høyere jo bedre.
#'		\item Oswestry: Skala fra 0 til 100, hvor lavest er friskest
#'     	\item Smerte: Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10..
#'    }
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item EQ5DEndr: Endring av EQ5D fra før til 3 eller 12 mnd. etter operasjonen
#'		\item EQ5DPre: EQ5D før operasjon
#'          \item EQ5DEndrPre: EQ5D, endring vs. prescore
#'     	\item Liggedogn: Liggetid på sykehuset
#'     	\item OswEndr: Endring i Oswestry-skår fra før til etter operasjon
#'          \item OswEndrPre: Oswestry (ODI-Oswestry Disability Index.) Endring vs. prescore.
#'     	\item OswTotPre: Oswestry-skår før operasjon
#'     	\item SmBeinEndr: Endring i beinsmerter fra før til etter operasjon
#'          \item SmBeinEndrPre: Smerter i beina. Endring vs. prescore
#'     	\item SmBePre: Grad av beinsmerter før operasjon
#'     	\item SmRyggEndr: Endring av ryggsmerter fra før til etter operasjon
#'          \item SmRyggEndrPre: Smerter i ryggen. Endring vs. prescore
#'     	\item SmRyPre: Ryggsmerter før operasjon
#'      \item TidOpReg MANGLER UTFYLT-variabel. (Ønsker å se på tid fra operasjon til registrering - )
#'    }
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param valgtMaal
#'        'Gjsn': gir middelverdi (standard)
#'        'Med': gir median
#' @param ktr Hvilken oppfølging man ønsker å se på. Valg: 3mnd - 3 mnd kontroll(standard), 12mnd - 12 mnd kontroll
#'
#' @return Linjediagram som viser utvikling over tid for valgt variabel
#'
#' @export
RyggFigGjsnBox <- function(RegData, outfile, valgtVar, tidlOp='', erMann='', hovedkat=99, aar=0,
                           tidsenhet = 'Mnd', hastegrad=99, minald=0, maxald=130, ktr=0, tittel=1,
                           datoFra='2007-01-01', datoTil='3000-01-01',
                           valgtMaal='Gjsn',enhetsUtvalg=0, hentData=0, preprosess=1, reshID=0){

      if (hentData == 1) {
            RegData <- RyggRegDataSQL()   #(datoFra, datoTil)
      }

      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess == 1){
            RegData <- RyggPreprosess(RegData=RegData)
      }


  #--------------- Definere variable ------------------------------

  RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype='gjsnBox')
  RegData <- RyggVarSpes$RegData
  KIekstrem <- RyggVarSpes$KIekstrem


  RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, reshID=reshID,
                              minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                              hovedkat=hovedkat, hastegrad=hastegrad, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
  RegData <- RyggUtvalg$RegData
  utvalgTxt <- RyggUtvalg$utvalgTxt
  medSml <- RyggUtvalg$medSml
#------------------------Klargjøre tidsenhet--------------
#  AggVerdier <- list(Hoved = 0, Rest =0)
  ind <- RyggUtvalg$ind
  N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))
  Ngr <- list(Hoved = 0, Rest =0)
  if (N$Hoved>9) {
            RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
            RegData <- RegDataFunk$RegData
            tidNum <- min(RegData$TidsEnhetSort, na.rm=T):max(RegData$TidsEnhetSort, na.rm = T) #as.numeric(levels(RegData$TidsEnhetSort))

#--------------- Gjøre beregninger ------------------------------



#Resultat for hovedgruppe
Ngr$Hoved <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], length)
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData$TidsEnhet[ind$Hoved],RegData$Variabel[ind$Hoved],  notch=TRUE, plot=FALSE)
	Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
	Konf <- MedIQR$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
#roughly a 95% confidence interval for the difference in two medians.
} else {	#Gjennomsnitt blir standard.
	Midt <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], mean, na.rm=T)
	SD <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], sd, na.rm=T)
	Konf <- rbind(Midt - 2*SD/sqrt(Ngr$Hoved), Midt + 2*SD/sqrt(Ngr$Hoved))
}


if (length(KIekstrem) == 0) {	#Hvis ikke KIekstrem definert i variabeldefinisjonen
	KIekstrem <- c(0,max(RegData$Variabel, na.rm=T))
}
	Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
	Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

#Resten (gruppa det sammenliknes mot)
MidtRest <- NULL
KonfRest <- NULL
if (medSml ==  1) {
Ngr$Rest <- tapply(RegData[ind$Rest ,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
	if (valgtMaal=='Med') {
		MedIQRrest <- plot(RegData$TidsEnhet[ind$Rest],RegData$Variabel[ind$Rest],  notch=TRUE, plot=FALSE)
		MidtRest <- as.numeric(MedIQRrest$stats[3, ])
		KonfRest <- MedIQRrest$conf
	} else {
	MidtRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], mean)	#ind$Rest
	SDRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], sd)
	Ngr$Rest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
	KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(Ngr$Rest), MidtRest + 2*SDRest/sqrt(Ngr$Rest))
	}
}

}
t1 <- switch(valgtMaal,
             Med = 'Median ',
             Gjsn = 'Gjennomsnittlig ')
tittel <- paste0(t1, RyggVarSpes$tittel)

if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c(maaltxt, 'KImin', 'KImaks',
                       paste0(maaltxt, 'Resten'), 'KImin, Resten', 'KImaks, Resten')[1:(3*(medSml+1))]
#UtData <- list(paste0(toString(RyggVarSpes$tittel),'.'), ResData )
#names(UtData) <- c('tittel', 'Data')

FigDataParam <- list(AggVerdier=ResData,
                     N=N,
                     Ngr=Ngr,
                     #KImaal <- KImaal,
                     #KImaaltxt <- KImaaltxt,
                     #soyletxt=soyletxt,
                     grtxt=levels(RegData$TidsEnhet),
                     #grtxt2=grtxt2,
                     #varTxt=varTxt,
                     #tidtxt=tidtxt, #RyggVarSpes$grtxt,
                     tittel=RyggVarSpes$tittel,
                    # xAkseTxt=xAkseTxt,
                     #yAkseTxt=yAkseTxt,
                     utvalgTxt=utvalgTxt,
                     fargepalett=RyggUtvalg$fargepalett,
                     medSml=medSml,
                     hovedgrTxt=RyggUtvalg$hovedgrTxt,
                     smltxt=RyggUtvalg$smltxt)



    #-----------Figur---------------------------------------
if (length(ind$Hoved)<10 | ((medSml == 1) & (length(ind$Rest) < 10))) {
figtype(outfile)
	plot.new()
	title(main=tittel)
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
#	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {

xmin <- min(tidNum)-0.5
xmax <- max(tidNum)+0.5
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
ytxt <- maaltxt #paste0(maaltxt, ytxt1, sep='')

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]
#
plot(tidNum,Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
		#cex=0.8, cex.lab=0.9, cex.axis=0.9,
		ylab=c(ytxt,'med 95% konfidensintervall'),
		xlab='Operasjonstidspunkt', xaxt='n',
		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = tidNum, labels = levels(RegData$TidsEnhet))
#Sammenlikning:
if (medSml==1) {
      AntTidsenh <- max(which(!is.na(KonfRest[1,])))
      polygon( c(tidNum[1]-0.01,tidNum[1:AntTidsenh], tidNum[AntTidsenh]+0.012,
				tidNum[AntTidsenh]+0.012, tidNum[AntTidsenh:1], tidNum[1]-0.01),
		c(KonfRest[1,c(1,1:AntTidsenh, AntTidsenh)], KonfRest[2,c(AntTidsenh,AntTidsenh:1,1)]),
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste0('95% konfidensintervall for ', RyggUtvalg$smltxt, ', N=', N$Rest)) #sum(Ngr$Rest, na.rm=T)
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(c(Ngr$Hoved, Ngr$Rest), na.rm=T), cex=cexgr)/2	#length(tidNum)/30
rect(tidNum-b, Midt-h, tidNum+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(tidNum, Midt, Ngr$Hoved, col=fargeHovedRes, cex=cexgr)

#Konfidensintervall:
indKonf <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=tidNum, y0=Midt-h, x1=tidNum, length=0.08, code=2, angle=90,
		y1=replace(Konf[1, ], indKonf, Midt[indKonf]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=tidNum, y0=Midt+h, x1=tidNum, y1=replace(Konf[2, ], indKonf, Midt[indKonf]+h),
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

title(main=c(tittel, RyggUtvalg$hovedgrTxt), font.main=1, line=1)
#Tekst som angir hvilket utvalg som er gjort
if (length(utvalgTxt)>0) {
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))}

if ( outfile != '') {dev.off()}

return(invisible(FigDataParam))

}	#end if statement for 0 observations
}	#end function

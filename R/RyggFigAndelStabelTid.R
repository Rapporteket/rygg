#' Søylediagram med en søyle for hvert år hvor variabelens kategorier er stablet oppå hverandre.
#'
#' Denne funksjonen lager et vertikalt søylediagram hvor søylene er delt inn i variabelens kategorier. Søylene
#' har følgelig alle høyde 1.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Fornoyd: Hvilken nytte mener du at du har hatt av operasjonen?
#'     \item Nytte: Hvor stor nytte pasienten har hatt av operasjonen
#'     \item TidlOp: Tidligere operasjon
#'    }
#' Argumentet \emph{hovedkat} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Annet
#'     \item 1: Prolaps
#'     \item 2: Foramenotomi
#'     \item 3: Laminektomi
#'     \item 4: Eksp. intraspin. impl.
#'     \item 5: Fusjon
#'     \item 6: Skiveprotese
#'     \item 7: Fjerning/revisjon
#'    }
#'    Velges ingen av disse, vil alle data vises.
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region
#'     \item 7: Egen region
#'	   \item 8: Egen region mot resten
#'    	}
#' @param ktr Oppfølging nr. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#'
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#' Videreutvikling: Ikke vis søyler (år) som har < 10 registreringer.
#'
#' @export
RyggFigAndelStabelTid <- function(RegData, outfile, valgtVar, hovedkat=99, preprosess=0, hentData=0,
                                  minald=0, maxald=130,
                               erMann=99, ktr=1, tidlOp=0, aar=0, enhetsUtvalg=0, tittel=1, reshID=0) {

if (hentData == 1) {
  RegData <- RyggRegDataSQLV2V3()       #(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess == 1){
       RegData <- RyggPreprosess(RegData=RegData)
     }


ktrtxt <- NULL

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)

	if (valgtVar=='Fornoyd') {
		t1 <- 'Hvor fornøyd er du med behandlinga du har fått på sykehuset?'
		if (ktr==1) {RegData$Var <- RegData$Fornoyd3mnd}
		if (ktr==2) {RegData$Var <- RegData$Fornoyd12mnd}
		ktrtxt <- paste0(c(3,12)[ktr], ' mnd etter operasjon')
		RegData <- RegData[which(RegData$Var %in% 1:5),]
		RegData$Var <- factor(RegData$Var, levels=1:5)
		Skala <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd')
		}
	if (valgtVar=='Nytte') {
		t1 <- 'Hvilken nytte mener du at du har hatt av operasjonen?'
		if (ktr==1) {RegData$Nytte <- RegData$Nytte3mnd}
		if (ktr==2) {RegData$Nytte <- RegData$Nytte12mnd}
		ktrtxt <- paste0('(',c(3,12)[ktr], ' mnd etter operasjon)')
		RegData$Var <- RegData$Nytte
		RegData <- RegData[which(RegData$Var %in% 1:7),]
		RegData$Var[which(RegData$Nytte %in% 1:2)] <- 1	#Bedre
		RegData$Var[which(RegData$Nytte %in% 3:5)] <- 2	#Omtrent uendret
		RegData$Var[which(RegData$Nytte %in% 6:7)] <- 3	#Verre
		RegData$Var <- factor(RegData$Var, levels=1:3)
		Skala <- c('Frisk/mye bedre', 'Omtrent uendret', 'Klart verre')
		}
	if (valgtVar=='TidlOp') {
		RegData$Var <- RegData$TidlOpr
		RegData <- RegData[which(RegData$Var %in% 1:4),]
		RegData$Var[RegData$Var == 4] <- 0
		RegData$Var <- factor(RegData$Var, levels=0:3)
		t1 <- 'Tidligere operert? '
		Skala <- c('Primæroperasjon', 'Samme nivå', 'Annet nivå',
					'Annet og sm. nivå')
	}

#Gjør utvalg
RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, #datoFra=datoFra, datoTil=datoTil,
                            minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                            hovedkat = hovedkat, tidlOp=tidlOp,
                            enhetsUtvalg=enhetsUtvalg) #, grType=grType
smltxt <- RyggUtvalg$smltxt
medSml <- RyggUtvalg$medSml
utvalgTxt <- RyggUtvalg$utvalgTxt
ind <- RyggUtvalg$ind
RegData <- RyggUtvalg$RegData
hovedgrTxt <- RyggUtvalg$hovedgrTxt

RegData$OpAar <- factor(RegData$OpAar)



TittelUt <- c(t1, ktrtxt)
if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}



#-------------------------Beregninger-----------------------------------------
#if (egenavd==1) {
Aartxt <- as.numeric(levels(RegData$OpAar))	#as.numeric(names(table(RegData$OpAar[ind$Hoved])))
RegData <- RegData[which(RegData$OpAar %in% Aartxt), ]

NVarAarRest <- ftable(RegData[ind$Rest, c('Var','OpAar')])	#ftable(list(RegDataRL$Var, RegDataRL$OpAar))
NAarRest <- colSums(NVarAarRest)
AndelerRest <- prop.table(NVarAarRest,2)*100
NVarAar <- ftable(RegData[ind$Hoved, c('Var','OpAar')])	#ftable(list(RegData$Var, RegData$OpAar))
NAar <- colSums(NVarAar)
AndelerHoved <- prop.table(NVarAar,2)*100

#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ((length(ind$Hoved) < 10) | (medSml ==1 & length(ind$Rest)<10)) {
FigTypUt <- rapbase::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
farger <- FigTypUt$farger
	plot.new()
	title(TittelUt)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 10 registreringer i egen- eller sammenlikningsutvelg', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {


#-----------Figur---------------------------------------
#Plottspesifikke parametre:
FigTypUt <- rapbase::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
farger <- c(rev(FigTypUt$farger),'black')
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
#antDesTxt <- paste('%.', antDes, 'f', sep='')
#grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
#vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

koord <- barplot(AndelerHoved, beside=F, las=1, main=Tittel, #names.arg=Aartxt, cex.names=0.95,
        col=farger, ylab="Andel (%)", ylim=c(0,132),	 #xlim=c(0, length(Aartxt)*1.2),
	cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, space=.25, border=NA)
axis(side=2, at=c(0,20,40,60,80,100))
legend('top', legend=rev(Skala), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
		xjust=0.5, fill=farger[length(Skala):1], border=farger[length(Skala):1], ncol=1)
text(koord, 102.7, NAar, cex=0.75)
mtext(at=koord, cex=0.9, side=1, line=0, adj=0.5, Aartxt)	#
mtext(side=1, line=1,'Operasjonsår', cex=0.9)
mtext(at=min(koord)-0.5, cex=0.8, side=1, line=2, adj=0, paste0('Tall over søylene angir antall operasjoner i ', hovedgrTxt))
if (medSml==1) {
	points(koord, AndelerRest[1,], cex=1.2, lwd=1, col='white', bg='white', pch=21)
	mtext(at=min(koord)-0.5, cex=0.8, side=1, line=3, adj=0,
		paste('Hvitt merke: Andel "', Skala[1], '", resten av landet', sep=''))
	}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[4], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}
#------------------------------------------------------------------------------
rownames(AndelerHoved) <- Skala
UtData <- list(toString(TittelUt),AndelerHoved, AndelerRest )
names(UtData) <- c('Tittel', 'AndelerHoved', 'AndelerRest')
return(invisible(UtData))
}

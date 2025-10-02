#' Juster datamaterialet for alder og kjønn
#'
#' Standardiserer beregning av en "andels"variabel mot variablene  Alder (år) og ErMann (0/1) fra valgt standardpopulasjon
#'
#' @param RegData Ei dataramme som minimum må inneholde variabelen det skal beregnes alders- og
#'                kjønnsstandardiserte rater for, samt alder, kjønn og bo-områder.
#' @param stdPop standardpopulasjon: 'Norge'-juster mot befolkningfil fra SSB.
#'            'Register'-juster mot alderssammensetning i registeret
#' @param antAldgr antall aldersgrupper man ønsker å benytte for justering mot alderssammensetning i befolkninga
#' @param katVariable grupperingsvariable, eks. 'Aar' eller 'ShNavn'
#'
#' @return Funksjon som alders- og kjønnsstandardiserer data basert på en valgt standardpopulasjon.
#' @export
StandAlderKjonn  <- function(RegData, stdPop, antAldgr, katVariable) {
      #Må ha kategorivariable som input...
      #katVariable <- c('OpAar', 'grVar')


      #Sjekk om RegData inneholder bo-områder.
		 #Legger aldersgrupper på RegData
             #antAldgr <- 3
             kvantgr <- (1/antAldgr)*(1:(antAldgr-1))
		 aldInndel <- quantile(RegData$Alder, kvantgr, na.rm = T)  #c(25, 50, 75)
		 aldgr <- c(19,aldInndel,85)
		 RegData$AlderGr <- cut(RegData$Alder,aldgr) #grensene er øverste grense i aldersintervallet

		 #Finne andeler/vekter i disse aldersgruppene ut fra populasjonen det skal standardiseres mot
		 #Innb2015aldkj <- read.table('./Innbyggere2015aldkj.csv', sep=';', header = T, encoding = 'UTF-8')
		 #StdPop2015AK <- aggregate(AntInnb ~ ErMann+Alder, data=Innb2015aldkj, FUN=sum)
		 #write.table(StdPop2015AK, file = 'StdPop2015.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')
		 if (stdPop == 'Norge') {
		 #Norges befolkning
		      StdPopAK <- read.table('C:/ResultattjenesteGIT/nkr/StdPop2015AK.csv', sep=';', header = T, encoding = 'UTF-8')
		      StdPopAK$AlderGr <- cut(StdPopAK$Alder, aldgr)
		      PopAldKjGr <- aggregate(AntInnb~ErMann+AlderGr, data=StdPopAK, FUN=sum)
		 }
		 #Registerpopulasjon:
		 if (stdPop == 'Register') {
		 PopAldKjGr <- aggregate(RegData$PasientID, RegData[ ,c('ErMann','AlderGr')], FUN=length)}

		 names(PopAldKjGr)[3] <- 'Ant'
		 PopAldKjGr$Vekt <- prop.table((PopAldKjGr$Ant))#PopAldKjGr$AntInnb/sum(PopAldKjGr$AntInnb)


		 #RegData$OpAar <- factor(RegData$OpAar, exclude = "") #Ikke nødvendig å lage faktor?
		 #RegData$ErMann <- factor(RegData$ErMann, exclude = "")
		 grupperingsVar <- c(katVariable, 'ErMann', 'AlderGr') #c('grVar', 'OpAar', 'ErMann', 'AlderGr')
		 teller <- aggregate(RegData$Variabel,  by=RegData[ ,grupperingsVar], drop=FALSE,
		           FUN = sum)
		 nevner <- aggregate(RegData$Variabel,  by=RegData[ ,grupperingsVar], drop=FALSE,
		                     FUN = sum)
		 AndelAKGr <-aggregate(RegData$Variabel,  by=RegData[ ,grupperingsVar], drop=FALSE,
						FUN = function(x) AndelStGr = sum(x)/length(x))
					#	aggregate(Variabel ~ ErMann+AlderGr+OpAar+grVar, data=RegData, drop=FALSE, #Skal ha: 2*3*4*AntGr=504
					#	   FUN = function(x) AndelStGr = sum(x)/length(x)) #Variabel er en 0/1-variabel.
		 names(AndelAKGr)[which(names(AndelAKGr)=='x')] <- 'Andel'

		  #Alternativt:
		 #Nvar <- tapply(RegData$Variabel, RegData[ ,grupperingsVar], sum, na.rm=T) #Variabel er en 0/1-variabel.
		 #Ngr <- table(RegData[ ,grupperingsVar])
		 #if(N > 0) {Ngr <- table(RegData[ ,grupperingsVar])}	else {Ngr <- 0}
		 AndelOgVekt <- cbind(AndelAKGr, Vekt = PopAldKjGr$Vekt)
		 AndelVekt <- cbind(AndelOgVekt, AndelVektGr = AndelOgVekt$Andel*AndelOgVekt$Vekt)
		 indNAN <- which(is.nan(AndelVekt$AndelVektGr))
		 AndelVekt$AndelVektGr[indNAN] <- 0

	 antKatVar <- length(katVariable)
	 if (antKatVar==1) {
	       AndelerGrStand <- aggregate(AndelVekt$AndelVektGr, by=list(AndelVekt$OpAar), drop=FALSE, FUN = function(x) 100*sum(x))
                  names(AndelerGrStand)[1] <- 'OpAar'
	                        } else {
	       AndelerGrStand <- aggregate(AndelVekt$AndelVektGr, by=AndelVekt[ ,katVariable], drop=FALSE, FUN = function(x) 100*sum(x))}

				#aggregate(AndelVektGr ~ OpAar+grVar, data=AndelVekt, FUN = function(x) 100*sum(x))
	 dimnavn <- names(table(AndelerGrStand[,katVariable[1]]))
	 teller <- 2
	 while (teller <= antKatVar ){
	       dimnavn <- list(dimnavn, names(table(AndelerGrStand[,katVariable[teller]])))
	       teller <- teller+1}
	 if (antKatVar ==1 ) dimnavn <- list(dimnavn,'Norge')
	 AndelerGr <- matrix(AndelerGrStand$x, nrow=3, ncol=dim(AndelerGrStand)[1]/3,
	                     dimnames=dimnavn)
	                           #list(min(AndelAKGr$OpAar):max(AndelAKGr$OpAar), levels(RegData$grVar)))
		 return(StandAndeler=AndelerGr)
		 }

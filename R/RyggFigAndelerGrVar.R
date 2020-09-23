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

RyggFigAndelerGrVar <- function(RegData, valgtVar='alder70', datoFra='2007-01-01', datoTil='3000-12-31', aar=0,
                                minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hentData=0,
                                preprosess=1, hastegrad=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0,
                                Ngrense=10, reshID=0, outfile='') {

      if (hentData == 1) {
            RegData <- RyggRegDataSQL()
      }

      # Preprosessere data
      if ((preprosess==1) & (dim(RegData)[1] >1)){
            RegData <- RyggPreprosess(RegData=RegData)
      }
      #------- Tilrettelegge variable
      RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr,
                                         datoTil=datoTil, figurtype = 'andelGrVar')
      RegData <- RyggVarSpes$RegData
      sortAvtagende <- RyggVarSpes$sortAvtagende
      #varTxt <- RyggVarSpes$varTxt
      KImaalGrenser <- RyggVarSpes$KImaalGrenser #c(0,20,40) #,xmax)
      #KImaal <- RyggVarSpes$KImaal
      if (length(grep('dekn',valgtVar)) == 1) { #Bruke vedlagte dekningsdata
            #Dekningsgradsfigur
            #RegData <- read.table(paste0('../data/',valgtVar, '.csv'), sep=';', header=T, stringsAsFactors = FALSE)  # na.strings = "NULL", encoding = 'UTF-8',
            #save(deknNakke17, file = '../data/deknNakke17.Rdata')
            Ngr <- 100
            indLandet <- which(RegData$ShNavn== 'Hele landet')
            AndelHele <- RegData$DekningsgradNKR[indLandet]
            AndelerGr <- RegData$DekningsgradNKR[-indLandet]
            fargepalett='BlaaOff'
            utvalgTxt <- ''
            medSml=0
            AntGr <- length(AndelerGr)
            GrNavn <- paste0(RegData$ShNavn,' (',RegData$Totalt , ')')[-indLandet]
            hovedgrTxt <- 'Hele landet'
            N <- RegData$Totalt[indLandet]
            xAkseTxt <- RyggVarSpes$xAkseTxt
            KImaalGrenser <- c(0,60,80,100)

      } else {
            #if (!is.null(dim(RegData))) {
            RegData[ ,grVar] <- factor(RegData[ ,grVar])

            #------- Gjøre utvalg
            smltxt <- ''
            medSml <- 0

            if (reshID==0) {enhetsUtvalg <- 0}
            RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                                        minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                                        hovedkat=hovedkat, hastegrad=hastegrad, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg)
            fargepalett <- RyggUtvalg$fargepalett
            smltxt <- RyggUtvalg$smltxt
            medSml <- RyggUtvalg$medSml
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
            }	else {Ngr <- 0}
            N <- dim(RegData)[1]
            AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
            AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
            AndelerGr <- round(100*tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)/Ngr,2)
            Ngrtxt <- as.character(Ngr)
            GrNavn <- names(Ngr)
            xAkseTxt <- "Andel opphold (%)"

            indGrUt <- which(Ngr < Ngrense)
            if (sum(indGrUt)>0) {
                  AndelGrUt <- sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
                  AndelerGr <- c(AndelerGr[-indGrUt],AndelGrUt) #AndelerGr[indGrUt] <- NA
                  #GrNavn <- c(names(Ngr)[-indGrUt], paste0(length(indGrUt), ' avd. med N<',Ngrense), )
                  GrUtNavn <- paste0(length(indGrUt), ' avd. med N<',Ngrense)
                  Ngrtxt <- c(Ngrtxt[-indGrUt],sum(Ngr[indGrUt]))  #Ngrtxt[indGrUt] <- paste0('<', Ngrense)
                  GrNavn <- c(GrNavn[-indGrUt], GrUtNavn) #paste0(c(GrNavn[-indGrUt], GrUtNavn),' (',Ngrtxt , ')')
            }
      }
      sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE)
      AndelerGrSort <- AndelerGr[sortInd]
      GrNavnSort <- GrNavn[sortInd]
      Ngrtxt <- Ngrtxt[sortInd]

      andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %')
      andeltxt <- andeltxtUsort[sortInd]

      if (tittel==0) {Tittel<-''} else {Tittel <- RyggVarSpes$tittel}

      if (valgtVar == 'OswEndr20' & hovedkat == 1) {KImaalGrenser <- c(0, AndelHele, 100)}
      if (valgtVar == 'OswEndr30pst' & hovedkat == 9) {KImaalGrenser <- c(0, AndelHele, 100)}

      FigDataParam <- list(AggVerdier=AndelerGrSort,
                               AggTot=AndelHele,
                               N=N,
                               Ngr=as.numeric(Ngrtxt),
      #                          grtxt2='',
                                soyletxt=andeltxt,
                                grtxt=GrNavnSort,
                                tittel=RyggVarSpes$tittel,
      #                          xAkseTxt=xAkseTxt, #NIRVarSpes$xAkseTxt,
      #                          KImaal = KImaal,
      #                          KImaaltxt = KImaaltxt,
      #                          grTypeTxt=RyggUtvalg$grTypeTxt,
                                utvalgTxt=utvalgTxt,
                                fargepalett=RyggUtvalg$fargepalett
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
         paste0(GrNavnSort,' (',Ngrtxt , ')')
         pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                            xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                            las=1, cex.names=cexShNavn*0.9))
         #Legge på målnivå
         #OswEndr20
         if (!is.na(KImaalGrenser[1])) {
            antMaalNivaa <- length(KImaalGrenser)-1
            rekkef <- 1:antMaalNivaa
            if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
            fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef] #c('green','yellow')# #c('#ddffcc', '#ffffcc') #, '#fff0e6') #Grønn, gul, rød
            maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
            rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
                 ytop=max(pos)+0.4, col = fargerMaalNiva[1:antMaalNivaa], border = NA) #add = TRUE, #pos[AntGrNgr+1],
            legend(x=0, y=-4, pch=c(NA,rep(15, antMaalNivaa)), col=c(NA, fargerMaalNiva[1:antMaalNivaa]),
                   ncol=antMaalNivaa+1,
                   xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
                   legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
         }
         pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                            xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                            las=1, cex.names=cexShNavn*0.9, add=T))
         mtext('Andel (%)', side=1, line=2)
         ybunn <- 0.1
         ytopp <- rev(pos)[AntGr]+1
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

         # if (indGrUt[1]>0){
         # mtext(paste0('* ',length(indGrUt),  ' avdelinger har mindre enn ', Ngrense,' registreringer og er fjernet fra figuren'),
         #              side=1, at=-0.2*xmax, las=1, cex=0.8, adj=0, col=farger[1], line=3)}

         par('fig'=c(0, 1, 0, 1))
         if ( outfile != '') {dev.off()}
         #----------------------------------------------------------------------------------
      }










      #       #-----------Figur---------------------------------------
      # if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
      #       FigTypUt <- rapFigurer::figtype(outfile)
      #       farger <- FigTypUt$farger
      #       plot.new()
      #       if (!is.null(dim(RegData))) { #>0
      #             tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
      #       } else {tekst <- 'Ingen registrerte data for dette utvalget'}
      #       title(main=Tittel)
      #       text(0.5, 0.6, tekst, cex=1.2)
      #       legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      #       if ( outfile != '') {dev.off()}
      #
      # } else {
      #
      #       #--------------------------FIGUR---------------------------------------------------
      #       #Innparametre: ...
      #       #----------- Figurparametre ------------------------------
      #       cexShNavn <- 1 #0.85
      #
      #       FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=fargepalett)
      #       farger <- FigTypUt$farger
      #       #Tilpasse marger for å kunne skrive utvalgsteksten
      #       NutvTxt <- length(utvalgTxt)
      #       vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
      #       #NB: strwidth oppfører seg ulikt avh. av device...
      #       par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
      #
      #       xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
      #       paste0(GrNavnSort,' (',Ngrtxt , ')')
      #       pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
      #                          xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
      #                          las=1, cex.names=cexShNavn*0.9))
      #       #Legge på målnivå
      #       if (!is.na(KImaalGrenser[1])) {
      #             antMaalNivaa <- length(KImaalGrenser)-1
      #             rekkef <- 1:antMaalNivaa
      #             if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
      #             fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef] #c('green','yellow')# #c('#ddffcc', '#ffffcc') #, '#fff0e6') #Grønn, gul, rød
      #             maalOppTxt <- c('Høy', 'Moderat', 'Lav')[rekkef]
      #             rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
      #                  ytop=max(pos)+0.4, col = fargerMaalNiva[1:antMaalNivaa], border = NA) #add = TRUE, #pos[AntGrNgr+1],
      #             legend(x=0, y=-4, pch=c(NA,rep(15, antMaalNivaa)), col=c(NA, fargerMaalNiva[1:antMaalNivaa]),
      #                    ncol=antMaalNivaa+1,
      #                    xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
      #                    legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
      #       }
      #       pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
      #                          xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
      #                          las=1, cex.names=cexShNavn*0.9, add=T))
      #       mtext('Andel (%)', side=1, line=2)
      #       ybunn <- 0.1
      #       ytopp <- rev(pos)[AntGr]+1
      #       #Linje for hele landet/utvalget:
      #       lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
      #       legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
      #              legend=paste0(hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
      #              bty='o', bg='white', box.col='white')
      #       mtext(at=pos+max(pos)*0.0045, paste0(GrNavnSort,' (',Ngrtxt , ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
      #
      #
      #       title(Tittel, line=1, font.main=1, cex.main=1.3)
      #
      #       text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
      #            las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus
      #
      #       mtext(at=max(pos)+0.4*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=1, adj=1, line=0.25)
      #
      #       #Tekst som angir hvilket utvalg som er gjort
      #       mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
      #
      #       # if (indGrUt[1]>0){
      #       # mtext(paste0('* ',length(indGrUt),  ' avdelinger har mindre enn ', Ngrense,' registreringer og er fjernet fra figuren'),
      #       #              side=1, at=-0.2*xmax, las=1, cex=0.8, adj=0, col=farger[1], line=3)}
      #
      #       par('fig'=c(0, 1, 0, 1))
      #       if ( outfile != '') {dev.off()}
      #       #----------------------------------------------------------------------------------
      # }
      return(invisible(FigDataParam))
}


#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Pasienter over 75år
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item ArbstatusPre: Mottar sykepenger, før operasjon?
#'     \item Arbstatus: Mottar sykepenger, 3 mnd etter operasjon?    (ENDRET fra Arbstatus3mnd, Arbstatus12mnd)
#'     \item ASA: ASA-grad > II
#'     \item BMI: Pasienter med fedme (BMI>30)
#'     \item ErstatningPre: Søkt/planlegger å søke erstatning?
#'     \item Fornoyd: Fornøyde pasienter (ENDRET fra Fornoyd3mnd, Fornoyd12mnd  )
#'     \item Kp3Mnd: Pasientrapporterte komplikasjoner
#'     \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd (ENDRET fra Misfor3mnd, Misfor12mnd)
#'     \item Nytte: Klart bedre    (ENDRET fra Nytte3mnd, Nytte12mnd)
#'	 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår, 3 mnd. (ENDRET fra Osw30_3mnd, Osw30_12mnd)
#'     \item PeropKomp: Komplikasjon ved operasjon
#'     \item PeropKompDura: Komplikasjon ved operasjon: Durarift
#'     \item Roker: Røyker du?
#'     \item Saardren: Sårdren
#'     \item SmStiPre: Bruker smertestillende før operasjonen
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år
#'     \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år
#'     \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?
#'     \item Utd: Andel høyskole-/universitetsutdannede
#'     \item Verre Mye verre/verre enn noen gang, 3 mnd. (ENDRET fra Verre3mnd, Verre12mnd)
#'	 \item ..
#'	 \item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
#' \item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
#' \item DegSponSSSten: Pasienter med Degenerativ spondylolistese og sentral spinal stenose
#' \item OswEndrLav: Mer enn 20 poeng forbedring i Oswestry-skår, 3 mnd/12mnd.
#' \item OswEndr20:
#' \item Osw48: Oswestry-skår fortsatt over 48
#' \item KpInf3Mnd: Sårinfeksjoner
#' \item Morsmal: Fremmedspråklige (ikke norsk som morsmål)
#'		}
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på.
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#'                BoHF - Pasienten bor i boområdene til det angitte HF.
#'                BoRHF - Pasienten bor i boområdene til det angitte RHF.
#' @param valgtVar Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.
#' @param AKjust Alders-og kjønnsjustering når grVar er boområder. Basert på 3 aldersgrupper gruppert ut fra alderskvartilene.
#'          0:ikke juster, 1:juster for alder og kjønn
#'
#' @return Figur som viser andel av valgt variabel for hvert av de siste tre årene
#'
#' @export

RyggFigAndelerGrVarAarVarKval <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='3000-12-31',
                                          minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hastegrad=99, hentData=0,
                                          preprosess=1,grVar='SykehusNavn', tittel=1, ktr=0, outfile='',
                                          AKjust=0) {

      if (hentData == 1) {
            RegData <- RyggRegDataSQLV2V3()
      }

      # Preprosessere data
      if (preprosess){
            RegData <- RyggPreprosess(RegData=RegData)
      }


      #----------- Figurparametre ------------------------------
      cexShNavn <- 1
      if ((AKjust==1) & !(grVar %in% c('BoHF', 'BoRHF'))) { AKjust=0}
      RegData[ ,grVar] <- factor(RegData[ ,grVar])
      sortering <- TRUE #dvs. decreasing = TRUE. Størst andel nederst
      #ktr kan ha verdiene 0, 1 eller 2
      ktrtxt <- c(', 3 mnd etter', ', 12 mnd. etter')[ktr]

      RegData$Variabel <- 0

      if (valgtVar == 'SympVarighUtstr') {
            #PasientSkjema. Andel med SympVarighUtstr 4 el 5
            #Kode 1:5,tom: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
            RegData <- RegData[which(RegData$SympVarighUtstr %in% 1:5), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
            VarTxt <- 'med varighet minst 1år'
            TittelUt <- 'Utstrålende smerter i minst ett år før operasjon'
      }

      #Erstatte med test for datoTil vs år. Eller legg på sjekk når beregner AarMax
      RyggUtvalg <- RyggUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                               erMann=erMann, hovedkat=hovedkat, tidlOp=tidlOp, hastegrad=hastegrad)
      RegData <- RyggUtvalg$RegData

      AarMax <- max(RegData$OpAar)	#Siste år
      AarMaxTxt <- as.character(AarMax)
      RegData <- RegData[which(RegData$OpAar %in% c((AarMax-2):AarMax)), ]

      RyggUtvalg <- RyggUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                               erMann=erMann, hovedkat=hovedkat, tidlOp=tidlOp, hastegrad=hastegrad)
      RegData <- RyggUtvalg$RegData
      utvalgTxt <- RyggUtvalg$utvalgTxt
      #SJEKK:
      RegData <- RegData[which(!is.na(RegData[ ,grVar])), ]
      names(RegData)[which(names(RegData) == grVar)] <- 'grVar'
      #grVar kan være sykehus, boområde osv.
      #Hvis siste år for få reg - ta også bort resultater fra foregående år.
      NminTot <- 50 #Ikke i bruk
      Ngrense <- switch(grVar,	#Minste antall registreringer for at ei gruppe skal bli vist
                        ShNavn = 10,
                        BoHF = 30,
                        BehHF = 30)
      N <- dim(RegData)[1] #table(RegData$OpAar)      #Antall per år

      #----------------------------------------------------------------------------------------------
      katVariable <- c('OpAar', 'grVar')
      Nvar <- cbind(
            tapply(RegData$Variabel, RegData[ ,katVariable], sum, na.rm=T), #Variabel er en 0/1-variabel.
            'Norge' = tapply(RegData$Variabel, RegData$OpAar, sum, na.rm=T))
      if(N > 0) {Ngr <- cbind(table(RegData[ ,katVariable]), 'Norge' = table(RegData$OpAar))
      } else {Ngr <- 0}

      #Sjekk for AK-justering
      if (AKjust == 1) { #Alders-og kjønnsjustering
            #Nvar <- tapply(RegData$Variabel, RegData[ ,c('OpAar', 'grVar')], sum, na.rm=T) #Variabel er en 0/1-variabel.
            StandGrVar <- StandAlderKjonn(RegData=RegData, stdPop='Register', antAldgr=3, katVariable=katVariable)
            StandNorge <- StandAlderKjonn(RegData=RegData, stdPop='Register', antAldgr=3, katVariable='OpAar')
            AndelerGr <- cbind(StandGrVar, StandNorge)
      } else {
            AndelerGr <- round(100*Nvar/Ngr,2)
      }
      #Må ta bort punkt/søyler for de som har for få registreringer for det aktuelle året.
      indGrUt <- as.numeric(which(Ngr < Ngrense)) #Alle som har for få. Indeks er kolonnevis
      #     if (length(indGrUt)==0) { indGrUt <- NULL}
      AndelerGr[indGrUt] <- NA	#dummy0	#Alle andeler med for lav N
      sortInd <- order(as.numeric(AndelerGr[AarMaxTxt,]), decreasing=sortering)
      AndelerSisteSort <- AndelerGr[AarMaxTxt,sortInd]

      AntGrNgr <- length(which(Ngr[AarMaxTxt, ] >= Ngrense))	#"Gyldige" grupper
      Ngrtxt <- Ngr[AarMaxTxt, ]	#paste0('N=', as.character(Ngr[AarMaxTxt, ]))
      Ngrtxt[which(Ngr[AarMaxTxt, ] < Ngrense)] <- paste0('<', Ngrense) #paste0('N<', Ngrense)
      AndelerGrSort <- AndelerGr[ ,sortInd]
      GrNavnSort <- colnames(AndelerGrSort) #names(AndelerGrSort)    #paste0(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd])

      andeltxt <- paste0(sprintf('%.0f',AndelerGrSort[AarMaxTxt,]), ' %')
      if (length(indGrUt)>0) {andeltxt[(AntGrNgr+1):(length(GrNavnSort))] <- paste0('N<', Ngrense, ' siste år')} #''

      #AndelHele <- round(100*sum(RegData$Variabel[which(RegData$OpAar==AarMax)])/
      #                        length(which(RegData$OpAar==AarMax)), 2) #round(100*sum(RegData$Variabel)/N, 2)

      #--------------------------------------------------------------

      if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

      #-----------Figur---------------------------------------
      # Lager ikke figur hvis ALLE N er mindre enn grensa eller hvis ugyldig parameterkombinasjon.
      if 	( max(Ngr) < Ngrense) {
            FigTypUt <- rapbase::figtype(outfile)
            farger <- FigTypUt$farger
            plot.new()
            if (dim(RegData)[1]>0) {
                  tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
            } else {tekst <- 'Ingen registrerte data for dette utvalget'}
            title(main=Tittel)
            text(0.5, 0.6, tekst, cex=1.2)
            legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
            if ( outfile != '') {dev.off()}

      } else {

            #--------------------------FIGUR---------------------------------------------------
            #Innparametre: ...

            hoyde <- ifelse(grVar=='BoHF', 3*600, 3*800)
            FigTypUt <- rapbase::figtype(outfile, height=hoyde, fargepalett=RyggUtvalg$fargepalett)
            farger <- FigTypUt$farger
            #Tilpasse marger for å kunne skrive utvalgsteksten
            NutvTxt <- length(utvalgTxt)
            vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.85)
            #NB: strwidth oppfører seg ulikt avh. av device...
            par('fig'=c(vmarg, 0.85, 0, 1)) #1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

            xmax <- min(max(AndelerGrSort, na.rm = T),100)*1.1     #1.15
            xaksetxt <- 'Andel (%)' #ifelse(AKjust==1, 'Andel (%), justert for alder og kjønn'
            yaksetxt <- ''
            yaksetxt <- switch(grVar,
                               'BoHF' = 'Boområde/opptaksområde',
                               'ShNavn' = 'Behandlende sykehus')
            soyleFarger <- rep(farger[3], AntGrNgr)
            soyleFarger[which(names(AndelerSisteSort)=='Norge')] <- farger[4]

            pos <- barplot(as.numeric(AndelerSisteSort), horiz=T, border=NA, col=soyleFarger, #farger[3], #main=Tittel,
                           xlim=c(0,xmax), ylim=c(0.05, 1.27)*length(GrNavnSort), font.main=1, xlab=xaksetxt,
                           las=1, cex.names=cexShNavn*0.9)
            mtext(yaksetxt, side=2, line=ifelse(grVar=='BoHF',8,11))
            ybunn <- 0.1
            ytopp <- pos[AntGrNgr]+ 0.4	#-length(indGrUt)]
            indMed <- 1:AntGrNgr
            Aar1txt <- as.character(AarMax-2)
            Aar2txt <- as.character(AarMax-1)
            #Naar <- rowSums(Ngr, na.rm=T)
            #ResAar <- 100*rowSums(Nvar, na.rm=T)/Naar
            points(y=pos[indMed], x=AndelerGrSort[Aar1txt, indMed], cex=1.5)
            points(y=pos[indMed], x=AndelerGrSort[Aar2txt, indMed], cex=1.5, pch=19)
            legend('top', inset=c(0.1,0), xjust=1,cex=0.9, bty='o', bg='white', box.col='white',
                   lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2, 1.2, 1.8), col=c('black','black',farger[3]),
                   #lwd=c(NA,NA,NA,2), pch=c(1,19,15,NA), pt.cex=c(1.2, 1.2, 2, 1), col=c('black','black',farger[3],farger[1]),
                   ncol=3, legend = c(Aar1txt, Aar2txt, AarMax)
                   # legend=c(paste0(Aar1txt, ' (', sprintf('%.1f', ResAar[1]), ' %, ', 'N=', Naar[1],')'),
                   #         paste0(Aar2txt, ' (', sprintf('%.1f', ResAar[2]), ' %, ', 'N=', Naar[2],')'),
                   #        paste0(AarMax, ' (', sprintf('%.1f', ResAar[3]), ' %, ', 'N=', Naar[3],')'),
                   #       paste0('Hele landet, ',AarMax))
            )
            overPos <- max(pos)+0.4*log(max(pos))
            #mtext(at=overPos, paste0('(N, ', AarMax, ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
            mtext(at=c(overPos, pos), c(2013, Ngr[1,sortInd]), line=0.5, adj=1, cex=0.85, las=1, side=4)
            mtext(at=c(overPos+0.3*log(max(pos)), overPos, pos), c('N', 2014, Ngr[2,sortInd]), line=3, adj=1, cex=0.85, las=1, side=4)
            mtext(at=c(overPos, pos), c(2015, Ngr[3,sortInd]), line=5.5, adj=1, cex=0.85, las=1, side=4)
            #lines(x=rep(ResAar[3], 2), y=c(ybunn, ytopp), col=farger[1], lwd=2)

            mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
            #          title(Tittel, line=1, font.main=1, cex.main=1.3)

            text(x=xmax*0.01, y=pos+0.1, andeltxt, #x=AndelerGrSort+xmax*0.01
                 las=1, cex=0.8, adj=0) #, col=farger[1])	#Andeler, hvert sykehus

            #Tekst som angir hvilket utvalg som er gjort
            #          mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


            par('fig'=c(0, 1, 0, 1))
            if ( outfile != '') {dev.off()}
            #----------------------------------------------------------------------------------
      }
}

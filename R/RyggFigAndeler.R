#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling)
#' av valgt variabel filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling
#'     \item AntDagerInnl: Liggetid
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item AntNivOpr: Antall nivå operert'
#'     \item ArbstatusPre: Arbeidsstatus før operasjon'
#'     \item -Arbstatus3mnd: Arbeidsstatus 3 mnd. etter operasjon
#'     \item -Arbstatus12mnd: Arbeidsstatus 12 mnd. etter operasjon
#'     \item ASA: ASA-grad
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item EqangstPre: Helsetilstand: Angst
#'     \item EqgangePre: Helsetilstand: Gange
#'     \item ErstatningPre: Har pasienten søkt erstatning?
#'     \item -Fornoyd3mnd: Fornøydhet 3 mnd etter operasjon
#'     \item -Fornoyd12mnd: Fornøydhet 12 mnd etter operasjon
#'     \item HovedInngrep: Hovedinngrep
#'     \item Komorbiditet: Komorbiditet
#'     \item KomplPer: Peroperative komplikasjoner
#'     \item KomplPost: Pasientrapporterte komplikasjoner
#'     \item Liggedogn: Liggetid ved operasjon
#'     \item Morsmal: Morsmål
#'     \item -Nytte3mnd: Hvilken nytte har du hatt av operasjonen? (svar 3 måneder etter)
#'     \item -Nytte12mnd: Hvilken nytte har du hatt av operasjonen? (svar 12 måneder etter)
#'     \item OpInd: Operasjonsindikasjon
#'     \item OpIndPareseGrad: Operasjonsindikasjon, paresegrad
#'     \item OpIndSmeType: Operasjonsindikasjon, smertetype
#'     \item OpKat: Operasjonskategori
#'     \item RadUnders: Radiologisk undersøkelse
#'     \item Roker: Røyker du?
#'     \item Saardren: Sårdren
#'     \item SivilStatus: Sivilstatus
#'     \item SmHyppPre: Hyppighet av smertestillende før operasjonen
#'     \item SmStiPre: Bruk av smertestillende før operasjonen
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter
#'     \item SympVarighUtstr: Varighet av utstrålende smerter
#'     \item TidlOpr: Tidligere ryggoperert?
#'     \item TidlOprAntall: Antall tidligere operasjoner
#'     \item UforetrygdPre: Har pasienten søkt uføretrygd?
#'     \item Underkat: Fordeling av inngrepstyper. NB: hovedkategori MÅ velges
#'     \item Utd: Høyeste fullførte utdanning
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
#'
#' @inheritParams RyggUtvalgEnh
 #' @param RegData En dataramme med alle nødvendige variabler fra registeret
 #' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
 #' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
 #' @param tittel Vise tittel i figuren eller ikke (0/1). standard:1
 #' hvilken enhet i spesialisthelsetjenesten brukeren tilhører
 #' @param preprosess Preprosesser data 0: Nei, 1: Ja (Standard)
 #' @param hentData Gjøre spørring mot database? 0: Nei, bruke RegData (Standard), 1: Ja
 #'
 #' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
 #'
 #' @export

RyggFigAndeler  <- function(RegData, valgtVar='alder', datoFra = '2007-01-01', datoTil = '2999-12-31',
                            aar = 0, hentData = 0, preprosess = 1,minald = 0, maxald = 110, erMann = '',
                            hovedkat = 99, hastegrad = 99, tidlOp = 99, ktr = 0, tittelMed = 1, outfile = '',
                            reshID = 0, enhetsUtvalg = 0, lagFig=1, ...){


  # if ("session" %in% names(list(...))) {
  #   rapbase::repLogger(session = list(...)[["session"]], msg = paste0('Fordelingsfigur: ',valgtVar))
  # }

  #if (reshID==0){stopifnot(enhetsUtvalg ==0 )}

  if (hentData == 1) {
    RegData <- RyggRegDataSQLV2V3()       #(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess == 1){
    RegData <- RyggPreprosess(RegData = RegData)
  }

  #------------Parameterdefinisjon -------------------------
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- NULL	#Spesifiseres evt. for hver enkelt variabel
  flerevar <- 0
  antDes <- 1

  RyggVarSpes <- RyggVarTilrettelegg(RegData = RegData, valgtVar = valgtVar, ktr = ktr, figurtype = 'andeler')
  RegData <- RyggVarSpes$RegData
  flerevar <- RyggVarSpes$flerevar
  variable <- RyggVarSpes$variable
  grtxt <- RyggVarSpes$grtxt
  retn <- RyggVarSpes$retn
  subtxt <- RyggVarSpes$subtxt
  antDes <- RyggVarSpes$antDes

#-----Gjør utvalg
  RyggUtvalg <- RyggUtvalgEnh(RegData = RegData, reshID = reshID, datoFra = datoFra, datoTil = datoTil,
                              minald = minald, maxald = maxald, erMann = erMann, aar = aar,
                              hovedkat = hovedkat, hastegrad = hastegrad, tidlOp = tidlOp,enhetsUtvalg = enhetsUtvalg)
  RegData <- RyggUtvalg$RegData
  utvalgTxt <- RyggUtvalg$utvalgTxt
  ind <- RyggUtvalg$ind
  medSml <- RyggUtvalg$medSml
  hovedgrTxt <- RyggUtvalg$hovedgrTxt
  smltxt <- RyggUtvalg$smltxt

  if (tittelMed==0) {tittel<-''} else {tittel <- RyggVarSpes$tittel
  }

  #--------------- Gjøre beregninger ------------------------------
  #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
  #DENNE KAN SKILLES UT SOM EGEN FUNKSJON beregnFordeling eller beregnAndeler
  #parametre: medSml, flerevar, RegData(inkl. VariabelGr), variable=NULL, ind ($Rest og $Hoved)
  #beregner: AggVerdier, N (antall i hovedgruppe og rest),
  #         Nfig hvis ulikt antall for ulike grupper (når gruppene er variable)

  AggVerdier <- list(Hoved = 0, Rest = 0)
  N <- list(Hoved = 0, Rest = 0)
  AntHend <- list(Hoved = switch(as.character(flerevar),
                     '0' = table(RegData$VariabelGr[ind$Hoved]),
                     '1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm = T)),
                     Rest = 0)
  N$Hoved <- switch(as.character(flerevar),
                    '0' = sum(AntHend$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                    '1' = length(ind$Hoved))
  AggVerdier$Hoved <- 100*AntHend$Hoved/N$Hoved

  if (medSml==1) {
    AntHend$Rest <- switch(as.character(flerevar),
                      '0' = table(RegData$VariabelGr[ind$Rest]),
                      '1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm = T))
    N$Rest <- switch(as.character(flerevar),
                     '0' = sum(AntHend$Rest),	#length(ind$Rest)- Kan inneholde NA
                     '1' = length(ind$Rest))
    AggVerdier$Rest <- 100*AntHend$Rest/N$Rest
  }

  Nfig <- N


#   #Denne må håndteres i Shiny:
#   if (valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) {
#     text(0.5, 0.6, 'Velg Hovedkategori:
# 			Prolapskirurgi, Foramenotomi, Fusjonskirurgi eller
# 		Fjerning/rev. av implantat for å se på inngrepstyper', cex = 1.2)}

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=Nfig,
                       Nvar=AntHend,
                       Ngr=Nfig,
                       #KImaal <- RyggUtvalg$KImaal,
                       grtxt=grtxt,
                       grtxt2=grtxt2,
                       #grTypeTxt=grTypeTxt,
                       tittel=tittel,
                       retn=retn,
                       subtxt = subtxt,
                       utvalgTxt=utvalgTxt,
                       fargepalett=RyggUtvalg$fargepalett,
                       medSml=medSml,
                       hovedgrTxt=hovedgrTxt,
                       smltxt=smltxt)

   if (lagFig == 1) {
    rapFigurer::FigFordeling(AggVerdier, tittel=tittel, hovedgrTxt=hovedgrTxt,
                 smltxt=smltxt, N=Nfig, Nfig=Nfig, retn=retn, utvalgTxt=utvalgTxt,
                 grtxt=grtxt, grtxt2=grtxt2, medSml=medSml,
                 subtxt=subtxt, outfile=outfile) #pstTxt=pstTxt,

#
#
#   #-----------Figur---------------------------------------
#   #Hvis for få observasjoner..
#   #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
#   if ((valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) | N$Hoved < 10 |
#       (medSml == 1 & N$Rest<10)) {
#     FigTypUt <- rapbase::figtype(outfile, fargepalett = RyggUtvalg$fargepalett)
#     farger <- FigTypUt$farger
#     plot.new()
#     title(tittel)	#, line = -6)
#     legend('topleft',utvalgTxt, bty = 'n', cex = 0.9, text.col = farger[1])
#     #Må heller gi denne som feilmelding før kommer til figur:
#     if (valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) {
#       text(0.5, 0.6, 'Velg Hovedkategori:
# 			Prolapskirurgi, Foramenotomi, Fusjonskirurgi eller
# 		Fjerning/rev. av implantat for å se på inngrepstyper', cex = 1.2)} else {
# 		  text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex = 1.2)}
#     if ( outfile != '') {dev.off()}
#
#   } else {
#
#     #-----------Figur---------------------------------------
#     #Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
#
# #function(AggVerdier, subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr)
#     #Plottspesifikke parametre:
#     FigTypUt <- rapbase::figtype(outfile, fargepalett = RyggUtvalg$fargepalett)
#     #Tilpasse marger for å kunne skrive utvalgsteksten
#     NutvTxt <- length(utvalgTxt)
#     antDesTxt <- paste0('%.', antDes, 'f')
#     grtxtpst <-
#       paste0(rev(grtxt), ifelse(length(grtxt) < 11, ' \n(', ' ('), rev(sprintf(antDesTxt, AggVerdier$Hoved)), '%)')
#     vmarg <- switch(retn, V = 0, H = max(0, strwidth(grtxtpst, units = 'figure', cex = cexgr)*0.7))
#     #vmarg <- max(0, strwidth(grtxtpst, units = 'figure', cex = cexgr)*0.7)
#     par('fig' = c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
#
#     farger <- FigTypUt$farger
#     fargeHoved <- farger[1]
#     fargeRest <- farger[3]
#     antGr <- length(grtxt)
#     lwdRest <- 3	#tykkelse på linja som repr. landet
#     cexleg <- 1	#Størrelse på legendtekst
#
#     #Horisontale søyler
#     if (retn == 'H') {
#       xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm = T)*1.15
#       pos <- barplot(rev(as.numeric(AggVerdier$Hoved)), horiz = TRUE, beside = TRUE, las = 1, xlab = "Andel pasienter (%)", #main = tittel,
#                      col = fargeHoved, border = 'white', font.main = 1, xlim = c(0, xmax), ylim = c(0.05,1.4)*antGr)	#
#       mtext(at = pos+0.05, text = grtxtpst, side = 2, las = 1, cex = cexgr, adj = 1, line = 0.25)
#
#       if (medSml == 1) {
#         points(as.numeric(rev(AggVerdier$Rest)), pos, col = fargeRest,  cex = 2, pch = 18) #c("p","b","o"),
#         legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
#                         paste0(smltxt, ' (N=', N$Rest,')')),
#                border = c(fargeHoved,NA), col = c(fargeHoved,fargeRest), bty = 'n', pch = c(15,18), pt.cex = 2,
#                lwd = lwdRest,	lty = NA, ncol = 1, cex = cexleg)
#       } else {
#         legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
#                border = NA, fill = fargeHoved, bty = 'n', ncol = 1, cex = cexleg)
#       }
#     }
#
#     if (retn == 'V' ) {
#       #Vertikale søyler eller linje
#       if (length(grtxt2) == 0) {grtxt2 <- paste0('(', sprintf(antDesTxt, AggVerdier$Hoved), '%)')}
#       ymax <- max(c(AggVerdier$Hoved, AggVerdier$Rest), na.rm = T) * 1.15
#       pos <- barplot(as.numeric(AggVerdier$Hoved), beside = TRUE, las = 1, ylab = "Andel pasienter (%)",
#                      xlab = subtxt, col = fargeHoved, border = 'white', ylim = c(0, ymax))	#sub=subtxt,
#       mtext(at = pos, grtxt, side = 1, las = 1, cex = cexgr, adj = 0.5, line = 0.5)
#       mtext(at = pos, grtxt2, side = 1, las = 1, cex = cexgr, adj = 0.5, line = 1.5)
#       if (medSml == 1) {
#         points(pos, as.numeric(AggVerdier$Rest), col = fargeRest,  cex = 2, pch = 18) #c("p","b","o"),
#         legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')),
#                border = c(fargeHoved,NA), col = c(fargeHoved,fargeRest), bty = 'n', pch = c(15,18), pt.cex = 2,
#                lty = c(NA,NA), lwd = lwdRest, ncol = 2, cex = cexleg)
#       } else {
#         legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
#                border = NA, fill = fargeHoved, bty = 'n', ncol = 1, cex = cexleg)
#       }
#     }
#
#     if (tittelMed==1) {title(tittel, line = 1, font.main = 1)}
#
#     #Tekst som angir hvilket utvalg som er gjort
#     mtext(utvalgTxt, side = 3, las = 1, cex = 0.9, adj = 0, col = farger[1], line = c(3-(1-tittelMed)+0.8*((NutvTxt-1):0)))
#
#     par('fig' = c(0, 1, 0, 1))
#     if ( outfile != '') {dev.off()}
   }

  return(invisible(FigDataParam))

}

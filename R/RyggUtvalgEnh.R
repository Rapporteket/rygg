
#' Title
#'
#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#' Gjøres ingen utvalg, vil alle data vises.
#'
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'   \itemize{
#'     \item 0: Hele landet (Standard)
#'     \item 1: Egen enhet mot resten av landet
#'     \item 2: Egen enhet
#'     \item 3: IKKE def - Egen enhet mot egen sykehustype
#'     \item 4: IKKE def - Egen sykehustype
#'     \item 5: IKKE def - Egen sykehustype mot resten av landet
#'     \item 6: IKKE def - Egen enhet mot egen region
#'     \item 7: IKKE def - Egen region
#'     \item 8: IKKE def - Egen region mot resten
#'   }

#' @param datoFra Tidligste operasjonsdato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste operasjonsdato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 110)
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param hovedkat Hvilken type hovedinngrep, numerisk 0-9, standard: 99, dvs. alle. Valgmuligheter:
#'   \itemize{
#'     \item 0:'Andre inngrep',
#'     \item 1:'Prolapskirurgi',
#'     \item 2:'Midtlinjebevarende dekompr.',
#'     \item 3:'Laminektomi',
#'     \item 4:'Eksp. intersp implantat',
#'     \item 5:'Fusjonskirurgi',
#'     \item 6:'Osteotomi, deformitet',
#'     \item 7:'Fjerning/rev. av implantat',
#'     \item 8:'Skiveprotese',
#'     \item 9:'Spinal stenose',
#'     \item 10:'Degen. spondylolistese'
#'     \item 99: Alle
#'   }
#' @param aar - Operasjonsår. Kan velge flere
#' @param hastegrad Hastegrad av operasjon 1: Elektivt, 2: Akutt, 3: Halvøyeblikkelig
#' @param tidlOp tidligere operert? 1: samme nivå, 2: annet nivå, 3: annet og sm. nivå', 4: Primæroperasjon
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#' @param fargepalett Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#' @inheritParams RyggFigAndeler
#'
#' @return Returnerer filtrert versjon av RegData
#' @export
RyggUtvalgEnh <- function(RegData, datoFra='2009-01-01', datoTil='3000-01-01', minald=0, maxald=110,
                          erMann='', hovedkat=99, aar=0, tidlOp=99, hastegrad=99, #hastegrad=99,
                          enhetsUtvalg=0, reshID=0, fargepalett='BlaaOff') {

# Definer intersect-operator
      "%i%" <- intersect

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,3,4,6,7)) {	#Ta med 2,4 og 7? Oppr. 3 og 6
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'3' = RegData[which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),],	#sml. shgruppe
						'4' = RegData[which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),],	#kun egen shgruppe
						'6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}

Ninn <- dim(RegData)[1]

indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indAar <- if (aar[1] > 2000) {which(RegData$Aar %in% as.numeric(aar))} else {indAar <- 1:Ninn}
indDato <- which(RegData$InnDato >= as.Date(datoFra) & RegData$InnDato <= as.Date(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
#Hovedkategori, flervalgsutvalg
      indHovedInngr <- if (hovedkat[1] %in% 0:8) {which(RegData$HovedInngrepV2V3 %in% as.numeric(hovedkat))
            } else {indHovedInngr <- 0}

      ##Spinal stenose:
      # if (length(intersect(c(8:9), hovedkat)>0)) {indSS <- with(RegData, which((RfSentr == 1 | RfLateral == 1)
      #                                                            & RfSpondtypeIsmisk==0
      #               & (OpDeUlamin==1 | OpLaminektomi==1 | OpDeFasett==1)
      #               & (HovedInngrep %in% c(2:5,7))))}
      if (is.element(9, hovedkat)) {indHovedInngr <- union(indHovedInngr, which(RegData$LSSopr==1))}
      #Degenerativ spondylolistese:
      if (is.element(10, hovedkat)) {indHovedInngr <- union(indHovedInngr,
                                                           intersect(which(RegData$LSSopr==1), which(RegData$RfSpondtypeDegen==1)))}
      if (!(hovedkat %in% 0:10)) {indHovedInngr <- 1:Ninn}

indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
indhastegrad <- if (hastegrad %in% 1:2) {
      RegData$OpKat[RegData$OpKat==3] <- 1
      which(RegData$OpKat == hastegrad)} else {1:Ninn}
indMed <- indAld %i% indDato %i% indAar  %i% indKj %i% indHovedInngr %i% indTidlOp %i% indhastegrad
RegData <- RegData[indMed,]

#Definifjon av spinal stenose:
#      COMPUTE filter_$=((RfSentr = 1 or RfLateral = 1)
#                        & (RfSpondtypeIsmisk = 0)
#                        & (OpDeUlamin=1 or OpLaminektomi=1 or OpDeFasett=1)
#                        & (HovedInngrep=2 or HovedInngrep=3 or HovedInngrep=4  or HovedInngrep=5 or HovedInngrep=7) )

# hkatnavn <- c( #0:9
# 	'Operasjonskategori: "ukjent"',	#hkat=0
# 	'Prolapskirurgi',
# 	'Foramenotomi',
# 	'Laminektomi',
# 	'Interspinøst implantat',
# 	'Fusjonskirurgi',
# 	'Skiveprotese',
# 	'Fjerning/rev. av implantat',
# 	'Spinal stenose',
# 	'Degen. spondylolistese')
HovedInngrepV2V3_txt <- c('Udef.', 'Prolaps', 'Dekomp.', 'Laminektomi', 'Eksp. intersp impl.',
                          'Fusjon', 'Deformitet', 'Revisjon', 'Skiveprotese') #0:8
hkatnavn <- c(HovedInngrepV2V3_txt, 'Spinal stenose', 'Degen. spondylolistese og LSS')

TidlOprtxt <-	c('Tidl. operert samme nivå', 'Tidl. operert annet nivå', 'Tidl. operert annet og sm. nivå', 'Primæroperasjon')
hastegradTxt <- paste0('Operasjonskategori: ', c('Elektiv', 'Akutt')) #, '1/2-Akutt'))

N <- dim(RegData)[1]

utvalgTxt <- c(paste0('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
	#År, flervalgsutvalg, ikke ha med egen tekst for dette?
#	if (aar[1] > 2000 ){
#	      AarMed <- min(RegData$Aar, na.rm=T):max(RegData$Aar, na.rm=T)
#	      if (length(AarMed)>1) {paste0('År: ', AarMed[1], ':', max(AarMed))} else {paste0('År: ', AarMed)}},
	if ((minald>0) | (maxald<110)) {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
						' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
	if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
	if (hovedkat[1] %in% 0:10) {paste0('Hovedinngrep: ', paste(hkatnavn[as.numeric(hovedkat)+1], collapse=','))},
      if (hastegrad %in% 1:2) {hastegradTxt[hastegrad]},
      if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)

SykehustypeTxt <- c('univ. sykehus', 'lokalsykehus', 'priv. sykehus')
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
		hovedgrTxt <- as.character(RegData$ShNavn[indEgen1]) } else {
		hovedgrTxt <- switch(as.character(enhetsUtvalg),
			'0' = 'Hele landet',
			'4' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'5' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}

	ind <- list(Hoved=0, Rest=0)
	if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
			medSml <- 0
			smltxt <- 'Ingen sml'
			ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
			ind$Rest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
				ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				ind$Hoved <- switch(as.character(enhetsUtvalg),
						'5' = which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),	#shgr
						'8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'3' = paste0('andre ', SykehustypeTxt[RegData$Sykehustype[indEgen1]]),	#RegData inneh. kun egen shgruppe
				'5' = 'andre typer sykehus',
				'6' = paste0(RegData$Region[indEgen1], ' forøvrig'),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			ind$Rest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
				'5' = which(RegData$Sykehustype != RegData$Sykehustype[indEgen1]),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett,
			ind=ind, medSml=medSml, smltxt=smltxt, hovedgrTxt=hovedgrTxt) #shtxt=shtxt, grTypeTxt=grTypeTxt
return(invisible(UtData))
}

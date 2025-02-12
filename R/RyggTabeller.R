#' Tabell som viser antall operasjoner per måned og sykehus siste x måneder
#'
#' Hvis reshID oppgis kommer månedlig oversikt kun for det aktuelle sykehuset. RegData må inneholde InnDato og Aar.
#' Tabellen returneres som en xtable
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
#' @inheritParams RyggFigAndeler
#' @return Antall operasjoner per måned og sykehus.
#' @export
#'
#'
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0){
      #RegData må inneholde ..
  if (reshID!=0){RegData <- RegData[which(RegData$ReshId==reshID), ]}
      datoFra <- lubridate::floor_date(as.Date(datoTil) %m-% months(antMnd), unit='month')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[intersect(which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')),
                               which(as.Date(RegData$InnDato, tz='uTC') > as.Date(datoFra, tz='UTC'))), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      if (reshID==0 & !is.na(datoTil)) {
        tabAvdMnd1 <- addmargins((tabAvdMnd1))}
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1, digits=0)
	return(tabAvdMnd1)
}

#' Tabell som viser antall opphold per sykehus og år, siste 5 år.
#'
#' @param RegData Registerdata
#' @param datoTil sluttdato for visningsperiode
#' @return Antall opphold per sykehus og år, siste 5 år
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date()){
  RegData <- RegData[which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')), ]
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}


#' Hvor mange skjema av hver type (lege, pasient, oppfølging)
#'
#' Tabell som viser oversikt over antall skjema av hver type. Kan velge kladd/ferdigstilt
#'
#' @param RegData Tabellen skjemaoversikt
#' @param datoFra angi start for tidsperioden
#' @param datoTil angi slutt for tidsperioden

#' @export
tabAntSkjema <- function(RegData, datoFra = '2019-01-01', datoTil=Sys.Date()){

  indDato <- which(as.Date(RegData$InnDato) >= datoFra & as.Date(RegData$InnDato) <= datoTil)
  RegData <- RegData[indDato, ]
  RegData$ShNavn <- as.factor(RegData$ShNavn)
  Registreringer <- table(RegData$ShNavn)
  TreMnd <- table(RegData$ShNavn[RegData$Ferdigstilt1b3mnd==1])
  TolvMnd <- table(RegData$ShNavn[RegData$Ferdigstilt1b12mnd==1])

  tab <- cbind('Basisskjema' = Registreringer,
               'Oppfølging, 3mnd' = TreMnd,
               'Oppfølging, 12mnd' = TolvMnd)

  tab <- xtable::xtable(rbind(tab,
                              'TOTALT, alle enheter:'= colSums(tab)),
                        digits=0)

return(tab)
}


#' Vise figurdata som tabell
#'
#' @param UtDataFraFig beregnede og definerte verdier ut fra valgt variabel. Standard utdata fra figurberegningsfunksjonen
#' @param figurtype andeler, andelGrVar, andelTid, gjsnGrVar, gjsnTid
#' @export

lagTabavFig <- function(UtDataFraFig, figurtype='andeler'){ #lagTabavFigAndeler

  attach(UtDataFraFig, warn.conflicts = F)

  if (figurtype %in% c('andeler','gjsnGrVar', 'andelTid')){

  tab <-cbind(Nvar$Hoved,
              Ngr$Hoved,
              AggVerdier$Hoved,
              if (medSml==1){cbind(
                Nvar$Rest,
                Ngr$Rest,
                AggVerdier$Rest)}
              )}

  if (figurtype %in% c('andeler', 'andelTid')) {
    colnames(tab) <- c(paste0('Antall', c(' (n)',
                                          ' (N)')),
                       'Andel (%)',
                     if (medSml==1) {
                       c(paste0('Antall', c(' (n)',
                                            ' (N)')),
                         'Andel (%)')})
                 }

  if (figurtype == 'gjsnTid'){
    tab <- AggVerdier
    colnames(tab) <-  grtxt
    tab <- t(tab)
  }

    if(figurtype=='gjsnGrVar') {
    kolnavn <- c('Antall (N)', SentralmaalTxt)
    if (medSml==1) {
      colnames(tab) <-  c(kolnavn, paste0(smltxt, c(', Antall (N)', ', Andel (%)')))}
    }
  if (figurtype == 'andeler') {rownames(tab) <- grtxt}
  return(tab)
}


#' Vise figurdata som tabell, sentralmål per sykshus
#'
#' @param UtDataFraFig
#'
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr,
              UtDataFraFig$AggVerdier$Hoved
  )
  colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
  detach(UtDataFraFig)

  return(tab)
}


#' Nøkkeltall ( div oversiktstall)
#' @inheritParams RyggUtvalgEnh
#'
#' @export
tabNokkeltall <- function(RegData, utvid=0, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=2, reshID=0) {
  datoFra <- switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                    Kvartal = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01'),
                    Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
  )

  RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
  RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData

  prosent <- function(x){sum(x, na.rm=T)/(length(x)-sum(is.na(x)))*100}

  tabNokkeltall <- rbind(
    'Antall operasjoner' = tapply(RegData$Alder, RegData$TidsEnhet, FUN=length),
    'Alder > 70 år (%)' = tapply(RegData$Alder>70, RegData$TidsEnhet, FUN=prosent),
      'Alder (gj.sn)' = tapply(RegData$Alder, RegData$TidsEnhet, FUN=mean, na.rm=T),
      'Kvinneandel (%)' = tapply(RegData$ErMann==0, RegData$TidsEnhet, FUN=prosent),
    'Liggedøgn, totalt' = tapply(RegData$Liggedogn, RegData$TidsEnhet, FUN=sum, na.rm=T),
    'Liggetid, postop., (gj.sn.)' = tapply(RegData$LiggetidPostOp, RegData$TidsEnhet, FUN=mean, na.rm=T),
    'Fornøyde 3 mnd. etter operasjon (%)' = tapply(RegData$Fornoyd3mnd, RegData$TidsEnhet,
                                               FUN=function(x){100*sum(x %in% 1:2)/sum(!is.na(x))}),
    'Reg.forsinkelse (gj.sn., dager)' = tapply(RegData$DiffUtFerdig, RegData$TidsEnhet, FUN=mean, na.rm=T)
    )

  if (utvid == 1) {
    tabUtvid <- rbind(
    'Antall avdelinger' = tapply(RegData$ShNavn, RegData$TidsEnhet, FUN=length), #length(unique((RyggData1aar$ShNavn))),
    tabNokkeltall,
    'Svart på oppfølging, 3 mnd.' = tapply(RyggData1aar$Ferdigstilt1b3mnd==1, RegData$TidsEnhet, FUN=prosent) #mean(RyggData1aar$Ferdigstilt1b3mnd==1, na.rm=T),
    )

     tabNokkeltall <- tabUtvid
  }

  return(tabNokkeltall)
}

#' Finner pasienter med potensielt dobbeltregistrerte skjema
#'
#' @param RegData dataramme fra nakkeregisteret, tidligst fra 01-01-2019
#' @param tidssavik - maks tidsavvik (dager) mellom to påfølgende registreringer som sjekkes
#'
#' @return mulig dobbeltregistrerte skjema
#' @export
tabPasMdblReg <- function(RegData, datoFra = '2019-03-01', tidsavvik=30){

  RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra)$RegData

  FlereReg <- RegData %>% dplyr::group_by(PasientID) %>%
    dplyr::summarise(N = length(PasientID), #n(),
                     KortTid = ifelse(N>1,
                                      ifelse(difftime(InnDato[order(InnDato)][2:N], InnDato[order(InnDato)][1:(N-1)], units = 'days') <= tidsavvik,
                                             1, 0), 0),
                     PasientID = PasientID[1]
    )

  PasMdbl <- FlereReg$PasientID[which(FlereReg$KortTid == 1)]
  TabDbl <- RegData[which(RegData$PasientID %in% PasMdbl),
                    c("PasientID", "InnDato", "ShNavn", "ReshId", "ForlopsID")] #, 'SkjemaGUID'
  TabDbl <- TabDbl[order(TabDbl$InnDato), ]
  N <- dim(TabDbl)[1]

  if (N>0) {
    indSmTid <- which(difftime(TabDbl$InnDato[2:N], TabDbl$InnDato[1:(N-1)], units = 'days') <= tidsavvik)
    TabDbl <- TabDbl[unique(sort(c(indSmTid, (indSmTid+1)))), ]
    TabDbl$InnDato <- format(TabDbl$InnDato, '%Y-%m-%d') #'%d.%m.%Y')
    tabUt <- TabDbl[order(TabDbl$PasientID, TabDbl$InnDato), ]
  } else {tabUt <- paste0('Ingen registreringer med mindre enn ', tidsavvik, 'minutter mellom registreringene for samme pasient.')}
}



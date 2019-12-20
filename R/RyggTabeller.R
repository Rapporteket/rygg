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
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0){
      #RegData må inneholde ..
  if (reshID!=0){RegData <- RegData[which(RegData$ReshId==reshID), ]}
      datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd, abbreviate = T), unit='month')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[intersect(which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')),
                               which(as.Date(RegData$InnDato, tz='uTC') > as.Date(datoFra, tz='UTC'))), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      if (reshID==0){
        tabAvdMnd1 <- addmargins((tabAvdMnd1))}
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
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
#' @param SkjemaOversikt Tabellen skjemaoversikt
#' @param datoFra angi start for tidsperioden
#' @param datoTil angi slutt for tidsperioden
#' @param skjemastatus 0: Kladd, 1:ferdigstilt
#' @export
tabAntSkjema <- function(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1){
  #tabAntSkjema(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1)
  #NB: Denne skal også kunne vise skjema i kladd!
  #Skjemastatus kan være -1, 0 og 1
  SkjemaOversikt$SkjemaRekkeflg <- factor(SkjemaOversikt$SkjemaRekkeflg, levels = c(5,10,15,20))
  skjemanavn <- c('Pasient preop.','Lege preop.','Oppfølging, 3mnd', 'Oppfølging, 12mnd')

  indDato <- which(as.Date(SkjemaOversikt$InnDato) >= datoFra & as.Date(SkjemaOversikt$InnDato) <= datoTil)
  indSkjemastatus <- which(SkjemaOversikt$SkjemaStatus==skjemastatus)
  SkjemaOversikt <- SkjemaOversikt[intersect(indDato, indSkjemastatus),]

  tab <-table(SkjemaOversikt[,c('ShNavn', 'SkjemaRekkeflg')])
  colnames(tab) <- skjemanavn
  tab <- xtable::xtable(tab)

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
#' @param RegData
#'
#' @param tidsenhet
#' @param datoTil
#' @param enhetsUtvalg
#' @param reshID
#'
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0) {
  datoFra <- switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                    Aar = paste0(year(as.Date(datoTil))-4, '-01-01')
  )
  RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
  RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
  #NB: sjekk riktige utvalg!!!
  indLigget <- which(RegData$liggetid>0)
  indRespt <- which(RegData$respiratortid>0)
  indSAPS <- which(RegData$SAPSII > 0)
  indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
  RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
  #RegData <- FinnReinnleggelser(RegData=RegData, PasientID = 'PasientID')
  #indReinn <- intersect(which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')), which(RegData$Overf==1))
  ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
  RegData$Ut1708 <- 0
  RegData$Ut1708[ind1708]<-1

  tabNokkeltall <- rbind(
    'Antall opphold' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
    'Antall pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                FUN=function(x) length(unique(x))),
    'Antall intensivdøgn' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
    'Liggetid (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
    'Liggetid (totalt)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=sum, na.rm=T),
    'Respirator-\nstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet,
                                       FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Respiratortid (median)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                      FUN=median, na.rm=T),
    'Respiratortid (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                      FUN=sum, na.rm=T),
    'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$TidsEnhet[indSAPS], FUN=median, na.rm=T),
    'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                   RegData$TidsEnhet[indNEMS], FUN=median, na.rm=T),
    'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                             RegData$TidsEnhet[indNEMS], FUN=sum, na.rm=T),
    'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$TidsEnhet,
                        FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$TidsEnhet,
                                          #tapply(RegData$Reinn[indReinn]==1, RegData$TidsEnhet[indReinn],
                                          FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$TidsEnhet,
                                         FUN=function(x) sum(x, na.rm=T)/length(x)*100)
  )


  return(tabNokkeltall)
}

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
      if (reshID==0){
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
  tab <- rbind(tab,
               'TOTALT, alle enheter:'=colSums(tab))
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
#' @inheritParams RyggUtvalgEnh
#'
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=2, reshID=0) {
  datoFra <- switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                    Kvartal = paste0(year(as.Date(datoTil))-4, '-01-01'),
                    Aar = paste0(year(as.Date(datoTil))-4, '-01-01')
  )
  RegData <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
  RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
  # indLigget <- which(RegData$liggetid>0)

prosent <- function(x){sum(x, na.rm=T)/length(x)*100}

  tabNokkeltall <- rbind(
    'Antall operasjoner' = tapply(RegData$Alder, RegData$TidsEnhet, FUN=length),
    'Alder > 70 år (%)' = tapply(RegData$Alder>70, RegData$TidsEnhet, FUN=prosent),
      'Alder (gj.sn)' = tapply(RegData$Alder, RegData$TidsEnhet, FUN=mean, na.rm=T),
      'Kvinneandel (%)' = tapply(RegData$ErMann==0, RegData$TidsEnhet, FUN=prosent),
    'Liggedøgn, totalt' = tapply(RegData$Liggedogn, RegData$TidsEnhet, FUN=sum, na.rm=T),
    'Liggetid, postop., (gj.sn.)' = tapply(RegData$LiggetidPostOp, RegData$TidsEnhet, FUN=mean, na.rm=T),
    'Reg.forsinkelse (gj.sn., dager)' = tapply(RegData$DiffUtFerdig, RegData$TidsEnhet, FUN=mean, na.rm=T)
    )

    # 'Liggetid (gj.sn)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),


  return(tabNokkeltall)
}

#' Finner pasienter med potensielt dobbeltregistrerte skjema
#'
#' @param RegData dataramme fra nakkeregisteret
#' @param tidssavik - maks tidsavvik (dager) mellom to påfølgende registreringer som sjekkes
#'
#' @return
#' @export
tabPasMdblReg <- function(RegData, tidsavvik=30){


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

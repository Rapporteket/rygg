

RyggData <- RyggRegDataSQLV2V3()


#Bygger opp kjørefil basert på dokumentet ResutaterAarsrappKapittel.Rnw fra nkr-pakken.

# Preprosessere data:
RegData <- RyggPreprosess(RegData=RyggData)
RegData$ShNavn <- as.character(RegData$ShNavn)

RegData$ODIendr <- RegData$OswTotPre-RegData$OswTot12mnd
RegData$ODIpst <- with(RegData, (OswTotPre-OswTot12mnd)/OswTotPre*100)

#Felles parametre:
ktr <- 2
ktrtxt <- '12 mnd.'

startAar <- 2011
rappAar <- 2019
datoFra1aar <- paste0(rappAar,'-01-01')
datoFra2aar <- paste0(rappAar-1,'-01-01')
datoFra3aar <- paste0(rappAar-2,'-01-01')
datoTil12mnd <- paste0(rappAar-1,'-12-31')
datoFra <- paste0(startAar,'-01-01')
datoTil <- paste0(rappAar,'-12-31')

#Gjør utvalg
#Tidsrom	Det vil bare være hensiktsmessig å velge hele år.
RegData <- RegData[which(RegData$InnDato >= as.POSIXlt(datoFra)),]
#RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
RegData1aar <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra1aar, datoTil=datoTil)$RegData
RegData12mnd <- RegData[which(RegData$OpAar < rappAar), ] #For å ikke få med de som har fått 12mnd-skjema i inneværende år.
RegDataPro <- RegData[which(RegData$HovedInngrep==1),]
RegDataPro12mnd <- RegDataPro[which(RegDataPro$OpAar<rappAar), ]
RegDataSS <- RyggUtvalgEnh(RegData, hovedkat = 8)$RegData

Ntot <- dim(RegData)[1]
Ntot1aar <- dim(RegData1aar)[1]

dato <- strptime(RegData$OpDato,format="%d.%m.%Y")
datomin <- min(dato)
datomax <- max(dato)

# Spinal stenose: BRUK UTVALG !!!!
attach(RegData)
indSS <-with(RegData, which((RfSentr == 1 | RfLateral == 1)
                            & is.na(RfSpondtypeIsmisk)
                            & (OpDeUlamin==1 | OpLaminektomi==1 | OpDeFasett==1)
                            & (HovedInngrep %in% c(2:5,7))))
#Degenerativ spondylolistese:
indDegenSpondy <- intersect(indSS, which(RegData$RfSpondtypeDegen==1))
indDegenSpondyFusj <- intersect(indDegenSpondy, which(RegData$HovedInngrep==5))
detach(RegData)

tabAvdN <- addmargins(table(RegData[c('ShNavn','OpAar')]))
antKol <- ncol(tabAvdN)
tabAvdN5 <- tabAvdN[,(antKol-5):antKol]
rownames(tabAvdN5)[dim(tabAvdN5)[1] ]<- 'TOTALT, alle avd.:'
colnames(tabAvdN5)[dim(tabAvdN5)[2] ]<- paste0(min(RegData$OpAar),'-',rappAar)

xtable(tabAvdN5, digits=0, align=c('l', rep('r', 6)),
       caption=paste0('Antall registreringer ved hver avdeling siste 5 år, samt totalt siden ', min(RegData$OpAar, na.rm=T),'.'),
       label = 'tab:AntReg')

tabKjPst <- sprintf('%.1f',table(RegData$Kjonn)/Ntot*100)
ant2007_2017 <- 38636
ant_TOT <- round(ant2007_2017 + tabAvdN['Sum',as.character(2018:rappAar)]    )
#HELLER HENT FRA DATA...

#Avdelinger som har registrerer i perioden \Sexpr{min(RegData$OpAar)} til \Sexpr{rappAar}\Sexpr{dim(tabAvdN)[1]-1} .
#Totalt er det registrert \Sexpr{Ntot} operasjoner.
#Andel menn og kvinner i totalpop: tabKjPst
#Siste rygginngrep registrert i datauttrekket: datomax
#Registrering 2007-tom rappAar: ant_TOT




#Gjennomsnittsalderen per år:
AlderAar <- tapply(RegData$Alder, RegData$OpAar, 'mean', na.rm=T)
AlderAar <- sprintf('%.1f', AlderAar)

#Over 70 år i rappAar:  Andel70 per år pg \% (sum(RegData1aar$Alder>=70)
Andel70 <- sprintf('%.0f',sum(RegData1aar$Alder>=70, na.rm=T)/sum(RegData1aar$Alder > -1, na.rm=T)*100)
Andel70startAar <- sprintf('%.1f',sum(RegData1aar$Alder>=70, na.rm=T)/sum(RegData1aar$Alder > -1, na.rm=T)*100)


Alder70Tid <- RyggFigAndelTid(RegData=RegData, datoFra = datoFra, valgtVar='alder70', outfile='FigAlder70.pdf')
Alder70Aar <- sprintf('%.1f',Alder70Tid$AggVerdier$Hoved)
#(slås sm. med over?)

#Andelen pasienter med fedme:
  FedmeAar <- table(RegData$BMI>30, RegData$OpAar)
  AndelFedmeAar <- FedmeAar['TRUE',]/table(RegData$OpAar)*100


#----Figurer
  RyggFigAndeler(RegData=RegData1aar, valgtVar='Alder', datoFra=datoFra1aar, datoTil=datoTil, outfile='FigAlderFord.pdf')

  RyggFigAndeler(RegData=RegData1aar, valgtVar='BMI', datoFra=datoFra, datoTil=datoTil,
                            outfile='FigBMI.pdf')
  RyggFigAndelerGrVar(RegData = RegData1aar, valgtVar='Morsmal', outfile = 'FigMorsmal.pdf')
  FremmedSpraakAar <-  RyggFigAndelTid(RegData=RegData, valgtVar='Morsmal', aar = startAar:rappAar,
                                       outfile='FigMorsmalAar.pdf')

  Utdanning <- RyggFigAndeler(RegData=RegData1aar, valgtVar='Utd', datoFra=datoFra1aar, datoTil=datoTil,
                              outfile='FigUtd.pdf')
  HoyUtdAvd <- RyggFigAndelerGrVar(RegData=RegData1aar, valgtVar='Utd', Ngrense = 10,
                                   outfile='FigHoyUtdAvd.pdf')
  UtdanningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='Utd', aar = startAar:rappAar,
                                  outfile='FigUtdAar.pdf')
  UforTid <- RyggFigAndelTid(RegData=RegData, valgtVar='UforetrygdPre', outfile='FigUforTid.pdf')

  UforetrygdPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='UforetrygdPre', datoFra=datoFra1aar,
                                       outfile='FigUforAvd.pdf')
  ErstatningTid <- RyggFigAndelTid(RegData=RegData, valgtVar='ErstatningPre', outfile='FigErstatTid.pdf')


#Andelen fremmedspråklige (inkl. samisk) per år:
  FremmedSpraak <- sprintf('%.1f', FremmedSpraakAar$AggVerdier$Hoved)

#Andelen ryggopererte med høyere utdanning (høyskole eller universitet):
  UtdanningAar <- sprintf('%.1f', UtdanningTid$AggVerdier$Hoved)


#TabArbstat - variabel endret. Se nærmere på resultater !!!
#   indSyk <- which(RegData1aar$ArbstatusPre %in% 6:10)
# GjsnSyk<- mean(RegData1aar$SykemeldVarighPre[indSyk], na.rm=T)
# SdSyk<- sd(RegData1aar$SykemeldVarighPre[indSyk], na.rm=T)



#I fullt arbeid når de blir ryggoperert:
ArbNum <- round(table(RegData1aar$ArbstatusPre)*100/sum(table(RegData1aar$ArbstatusPre)), 1)
ArbNum[1]

#Andel pasienter svart på spørsmål om arbeidsstatus: \%
  NsvarArb <- sum(table(RegData1aar$ArbstatusPre))
  round(NsvarArb/Ntot1aar*100, 1)

Arb <- paste0(ArbNum, '%')
names(Arb) <- c('I arbeid', 'Hjemmeværende', 'Student/skoleelev', 'Pensjonist', 'Arbeidsledig',
                'Sykemeldt', 'Aktiv sykemeldt', 'Delvis Sykemeldt', 'Attføring/rehabiliteirng', 'Uføretrygdet')
xtable(cbind('Andeler'=Arb),  align=c('l','r'),
       caption=paste0('Arbeidsstatus, pasienter operert i ', rappAar,'.'),
       label="tab:Arb")

#Mottok sykepenger (sykemeldte, uføretrygdede eller attføring):
sum(ArbNum[6:10])


#Har søkt eller planlegger å søke uføretrygd, rappAar:
UforAar <- sprintf('%.1f', UforTid$AggVerdier$Hoved)
#Har søkt eller planlegger å søke erstatning, rappAar:
ErstatningAar <- sprintf('%.1f', ErstatningTid$AggVerdier$Hoved)

#Tabell, pasienter som har vært til radiologiske undersøkelser.
RVvar <- c('RvCt', 'RvMr', 'RvRadigr', 'RvDiscogr', 'RvDpregblok', 'RvRtgLscol', 'RvFunksjo')
RVant <- colSums(RegData1aar[, RVvar], na.rm=T)
RVpst <- round(RVant*100/Ntot1aar,1)

RV <- cbind('Antall' = c(RVant, Ntot1aar),
            'Andeler' = c(paste0(RVpst, '%'),' ')
)
rownames(RV) <- c('CT', 'MR', 'Radikulografi', 'Diskografi', 'Diagnostisk blokade',
                  'Røntgen LS-columna', 'Med fleksjon/ekstensjon', 'Tot. ant.')
xtable(RV, caption=paste0('Radiologisk vurdering, ',rappAar),
       label="tab:RV", align=c('l','r','r'))



#Tabell, diagnoser basert på radiologiske funn
RFvar <- c("RfNormal", "RfSkive", "RfSentr", "RfLateral", "RfForamino", "RfDegen",
           "RfSpondtypeIsmisk", "RfSpondtypeDegen", "RfDegskol", "RfSynovpre", "RfPseudom")
RFant <- colSums(RegData1aar[, RFvar], na.rm=T)[-1]
RFpst <- round(RFant*100/Ntot1aar)
RF <- cbind('Antall' = c(RFant, Ntot1aar),
            'Andeler' = c(paste0(RFpst, '%'),' '))

#Teller opp de som er diagnostisert som normale:
RFNorm <- intersect(which(rowSums(RegData1aar[, RFvar[2:11]], na.rm=T) == 0),
                    which(RegData1aar$RfNormal == 1))
rownames(RF) <- c('Skiveprolaps', 'Sentral spinalstenose', 'Lateral spinalstenose',
                  'Foraminal stenose', 'Degenerativ rygg/skivedegenerasjon',
                  'Istmisk spondylolistese', 'Degenerativ spondylolistese',
                  'Degenerativ skoliose', 'Synovial syste', 'Pseudomeningocele', 'Tot.ant.')
xtable(RF, caption=paste0('Radiologiske diagnoser, ', rappAar),
       label="tab:RF", align=c('l','r','r'))

#Tabell, ASA
ASAant <- table(factor(RegData1aar$ASA, levels=1:5), useNA='a')
ASApst <- round(ASAant*100/Ntot1aar, 1)
ASA <- cbind('Antall' = ASAant,
             'Prosent' = paste0(ASApst, '%'))
rownames(ASA) <- c('I','II','III','IV', 'V', 'Ikke besvart')
xtable(ASA, caption=paste0('Fordeling av ASA-grad, operasjoner utført i ', rappAar),
       label="tab:ASA", align=c('c','r','r'))

#Tabell, røyking
RoykTot <- round(sum(RegData1aar$Roker, na.rm=T)/sum(table(RegData1aar$Roker))*100,0)
Royk <- round(prop.table(ftable(RegData1aar[,c('Kjonn','Roker')]),1)[,2]*100, 0)
names(Royk) <- c('Menn', 'Kvinner')

#Andelen pasienter med ASA grad I-II:
round(sum(table(RegData1aar$ASA)[1:2])/Ntot1aar*100, 1)

#Andel røykere som ryggopereres, per år:
RoykAar <- sprintf('%.1f', RoykTid$AggVerdier$Hoved)



#----- Figurer
  #1: Samme nivå, 2:Annet nivå, 3: Annet og sm. nivå, 4: Primæroperasjon
  #NB: I figuren er 4 kodet om til 0 !!!
AndelTidlOp <- RyggFigAndelStabelTid(RegData=RegData, outfile='TidlOp.pdf', valgtVar='TidlOp')
#? TidlOpFordHoved <- rowSums(AndelTidlOp$AndelerHoved)
#? TidlOpHoved <- round(TidlOpFordHoved[2:4]*100/sum(TidlOpFordHoved[2:4]), 1)

RoykTid <- RyggFigAndelTid(RegData=RegData, valgtVar='Roker', outfile='FigRokerTid.pdf')


#Andelen reoperasjoner, per år:
AndelReop <- round(colSums(AndelTidlOp$AndelerHoved[2:4,]))

#Av de pasientene operert i \Sexpr{rappAar} som hadde vært operert tidligere:
AndelerSisteAar <- AndelTidlOp$AndelerHoved[,dim(AndelTidlOp$AndelerHoved)[2]]
AndelerTidlOp <- sprintf('%.1f', AndelerSisteAar[2:4]/sum(AndelerSisteAar[2:4])*100)
AndelerTidlOp[1] #\% operert i samme nivå,
AndelerTidlOp[2] #operert i annet nivå
AndelerTidlOp[3] #operert i både samme og annet nivå.

#Prolapspasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
AndelTidlOp3Pro <- round(table(RegDataPro$TidlOprAntall>2,RegDataPro$OpAar)['TRUE',]/table(RegDataPro$OpAar)*100,1)
min(AndelTidlOp3Pro)
max(AndelTidlOp3Pro)
#lumbal spinal stenosepasienter operert mer enn 2 ganger tidligere (startår-rapp.år):
AndelTidlOp3SS <- round(table(RegDataSS$TidlOprAntall>2,RegDataSS$OpAar)['TRUE',]/table(RegDataSS$OpAar)*100,1)
min(AndelTidlOp3SS) max(AndelTidlOp3SS)


#Andelen operert for lumbalt prolaps ved hjelp av synsfremmende midler:
MikroAarPro <- prop.table(table(RegDataPro$OpMikro, RegDataPro$OpAar),2)*100
BruktMikroAarPro <- sprintf('%.0f', MikroAarPro['1',])

#Andelen operert for lumbal spinal stenose ved hjelp av synsfremmende midler:
MikroAarSS <- prop.table(table(RegDataSS$OpMikro, RegDataSS$OpAar),2)*100
BruktMikroAarSS <- sprintf('%.0f', MikroAarSS['1',])


FornoydPro <- sprintf('%.0f',prop.table(table(RegDataPro12mnd$Fornoyd12mnd, RegDataPro12mnd$OpAar),2)[1,]*100)
SaarInfSS <- prop.table(table(RegDataSS$KpInf3Mnd, RegDataSS$OpAar),2)*100
FornoydSS <- sprintf('%.0f',prop.table(table(RegDataSS$Fornoyd12mnd, RegDataSS$OpAar),2)[1,]*100)




#Hyppigste tilstandene pasienter ble operert for i rappAar} var
HovedInngrep <- RyggFigAndeler(RegData=RegData1aar, valgtVar='HovedInngrep', datoFra=datoFra1aar,
                               datoTil=datoTil, outfile='HovedInngrep.pdf')
#Andel operert for lumbalt prolaps: \%
round(HovedInngrep$Andeler[1,2])
#Andel operert for Spinal stenose
indSS1aar <- intersect(which(RegData$OpAar==rappAar), indSS)
AndelSSrappaar <- round(100*length(indSS1aar)/dim(RegData1aar)[1])

#Tabell, fordeling av hovedinngrepstype
  HovedInngrepTab <- cbind('Antall' = HovedInngrep$Antall[1,],
                           'Andeler' = paste0(round(as.numeric(HovedInngrep$Andeler[1,])),'%')
  )
rownames(HovedInngrepTab) <- HovedInngrep$GruppeTekst
xtable(HovedInngrepTab, align=c('l','r','r'), caption=paste0('Fordeling av hovedinngrep, ', rappAar), label="tab:AntHovedInngrep", digits=1)



RyggFigAndelerGrVar(RegData = RegData, valgtVar = 'degSponFusj',
                             outfile = 'FigdegSponFusj.pdf')



#Andelen operert med dagkirurgi for hhv prolaps og spinal stenose
ProDagTid <- table(RegDataPro[ ,c('Dagkirurgi', 'OpAar')], useNA = 'a')
ProDagPst <- prop.table(ProDagTid[1:2,],2)*100
ProDag11_naa <- c(round(ProDagPst['1','2011']), round(ProDagPst['1',as.character(rappAar)]))

SSDagTid <- table(RegDataSS[ ,c('Dagkirurgi', 'OpAar')], useNA = 'a')
SSDagPst <- prop.table(SSDagTid[1:2,],2)*100
SSDag11_naa <- c(round(SSDagPst['1','2011']), round(SSDagPst['1',as.character(rappAar)]))

#Andel operert for spinal stenose som også hadde Degenerativ spondylolistese, rappAar
AntDegenSpondSS <-  dim(RyggUtvalgEnh(RegDataSS, hovedkat = 9, aar = rappAar)$RegData)[1]
AntSS <- sum(RegDataSS$OpAar==rappAar)
AndelDegSponSS <- round(AntDegenSpondSS/AntSS*100,1)



#andelen som får tilleggsbehandling med fusjonskirurgi:
DegSponFusjSStid <- RyggFigAndelTid(RegData=RegData, valgtVar = 'degSponFusj', hovedkat = 8,
                                    outfile = 'FigdegSponFusjSStid.pdf')
DegSponFusjSSAar <- sprintf('%.1f', DegSponFusjSStid$AggVerdier$Hoved)



#Tabell, antibiotika
AntibiotikaData <- RyggFigAndelTid(RegData=RegData, outfile='Antibiotika.pdf',
                                     valgtVar = 'Antibiotika')
Antibiotika <- rbind('Andeler' = paste(round(AntibiotikaData$AggVerdier$Hoved, 1), '%',sep=''),
                     'Antall' = AntibiotikaData$Ngr$Hoved)
xtable(Antibiotika, caption=AntibiotikaData$Tittel, label="tab:AntibiotikaAndel",
       align=c('l',rep('r', rappAar- startAar +1)))



#---Figurer
RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdPro.pdf',
                 valgtVar='Liggedogn', hovedkat = 1, valgtMaal = 'Gjsn')
RyggFigGjsnGrVar(RegData=RegData1aar, outfile='LiggetidAvdSS.pdf',
                 valgtVar='Liggedogn', hovedkat = 8, valgtMaal = 'Gjsn')
RyggFigGjsnBox(RegData=RegData, outfile='OswEndrPro.pdf',
               aar=startAar:(rappAar-1), valgtVar='OswEndr', hovedkat=1, ktr=ktr)
RyggFigGjsnBox(RegData=RegData, outfile='OswEndrSS.pdf',
               aar=startAar:(rappAar-1), valgtVar='OswEndr', hovedkat=8, ktr=ktr)



#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, før operasjon:
indProPP <-  with(RegDataPro, which((OpAar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIprePro <- sprintf('%.1f', mean(RegDataPro$OswTotPre[indProPP]))
#gjennomsnittlig ODI score, lumbalt prolaps, rappAar, etter operasjon:
ODIpostPro <- sprintf('%.1f', mean(RegDataPro$OswTot12mnd[indProPP]))

#ODI-pre, lumbal spinal stenose:
indSSPP <-  with(RegDataSS, which((OpAar == (rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIpreSS <- sprintf('%.1f', mean(RegDataSS$OswTotPre[indSSPP]))
#ODI-post, lumbal spinal stenose:
ODIpostSS <- sprintf('%.1f', mean(RegDataSS$OswTot12mnd[indSSPP]))

#ODI-pre, fusjonkirurgi:
indFusjPP <-  with(RegData,
                   which(HovedInngrep==5 & (OpAar==(rappAar-1)) & !is.na(OswTotPre) & !is.na(OswTot12mnd)))
ODIpreFusj <- sprintf('%.1f', mean(RegData$OswTotPre[indFusjPP]))
#ODI-pre, fusjonkirurgi:
ODIpostFusj <- sprintf('%.1f', mean(RegData$OswTot12mnd[indFusjPP]))


#--Figurer
RyggFigGjsnGrVar(RegData=RegData, outfile='OswEndrAvdPro.pdf', Ngrense = 20,
                          aar=c((rappAar-2):(rappAar-1)),
                          valgtVar='OswEndr', hovedkat=1, tidlOp=4, opKat=1, ktr=ktr)
RyggFigGjsnGrVar(RegData=RegData, outfile='OswEndrAvdSS.pdf', Ngrense = 20,
                        aar=c((rappAar-2):(rappAar-1)),
                 valgtVar='OswEndr', hovedkat=8, tidlOp=4, ktr=ktr)


RyggFigAndelTid(RegData=RegData, outfile='Osw20TidPro.pdf',
                       aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr20', hovedkat=1, ktr=ktr)
RyggFigAndelTid(RegData=RegData, outfile='Osw30TidSS.pdf',
                       aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr30pst', hovedkat=8, ktr=ktr)

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidPro.pdf',
                      aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr', hovedkat=1, ktr=ktr)

RyggFigGjsnBox(RegData=RegData, outfile='OswEndrTidSS.pdf',
                      aar=c((rappAar-2):(rappAar-1)), valgtVar='OswEndr', hovedkat=8, ktr=ktr)

RyggFigAndelStabelTid(RegData=RegData, aar=startAar:(rappAar-1), outfile='FigNyttePro.pdf', valgtVar='Nytte',
                                    hovedkat=1, ktr=ktr)

RyggFigAndelStabelTid(RegData=RegData, aar=startAar:(rappAar-1), outfile='FigNytteSS.pdf', valgtVar='Nytte',
                                 hovedkat=8, ktr=ktr)
FornoydProTid <- RyggFigAndelStabelTid(RegData=RegData, outfile='FigFornoydPro.pdf',
                                    valgtVar='Fornoyd', hovedkat=1, aar=startAar:(rappAar-1), ktr=ktr)
FornoydSSTid <- RyggFigAndelStabelTid(RegData=RegData, outfile='FigFornoydSS.pdf',
                                   valgtVar='Fornoyd', hovedkat=8, aar=startAar:(rappAar-1), ktr=ktr)

RyggFigAndelerGrVar(RegData=RegData, valgtVar='Fornoyd', ktr=ktr,
                           aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                           hovedkat=1,  opKat=1, tidlOp=4,  outfile='FigFornoydAvdPro.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar='Fornoyd', ktr=ktr,
                           aar=c((rappAar-2):(rappAar-1)), Ngrense = 20,
                           hovedkat=8,  opKat=1, tidlOp=4,  outfile='FigFornoydAvdSS.pdf')
RyggFigGjsnBox(RegData=RegData, valgtVar='Liggedogn', datoFra=datoFra, datoTil=datoTil,
                      hovedkat = 1, outfile='LiggetidProlaps.pdf') #



#Andel fornøyde, lumbalt prolaps, ett år etter:
FornoydProTid
#Andel fornøyde, lumbal spinal stenose, ett år etter:
FornoydSSTid

#Liggetid, prolaps:
LiggetidPro <- tapply(RegDataPro$Liggedogn[indPro], RegDataPro$OpAar[indPro], mean, na.rm=T)
NedgLiggetidPro <- sprintf('%.1f', abs(LiggetidPro[as.character(rappAar)]-LiggetidPro['2010']))




  ODIProOpKat <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$OpKat),2)*100,1)
ODIProTidlOpAnt3 <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$TidlOprAntall>2),2)*100,1)

AndelOhjSS <- round(prop.table(table(RegData$OpKat[indSS]))*100,1)
ODISSTidlOpAnt3 <- round(prop.table(table((RegData$ODIpst)[indSS]>=30,
                                          RegData$TidlOprAntall[indSS]>2),2)*100,1)

opKat <- 1  #Bare elektive pasienter
tidlOp <- 4 #Bare primæroperasjoner


TidlOp3 <- RyggFigAndelTid(RegData=RegData, hovedkat = 1, valgtVar = 'tidlOp3', outfile = 'FigTidlOpAnt3.pdf')


#suksessraten, lumbalt prolaps, ikke tidl. operert:
ODIProTidlOp <- round(prop.table(table(RegDataPro$ODIendr>20, RegDataPro$TidlOpr==4),2)*100,1)
ODIProTidlOp['TRUE','TRUE']

Hos pasienter med lumbalt prolaps som ikke har vært operert i ryggen tidligere er
suksessraten \Sexpr{ODIProTidlOp['TRUE','TRUE']} \% mot \Sexpr{ODIProTidlOp['TRUE','FALSE']} \%.
Hos prolapspasienter operert som ø-hjelp er andelen med betydelig forbedring
(suksessrate)  \Sexpr{ODIProOpKat['TRUE','2']} \%, mot \Sexpr{ODIProOpKat['TRUE','1']} \% av de som blir
operert planlagt (elektivt). Dersom man har vært operert mer enn 2 ganger tidligere i
ryggen faller suksessraten fra  for lumbal spinal stenoseopererte betydelig (10\%).
Langt færre pasienter i spinal stenosegruppen opereres som øyeblikkelig hjelp; \Sexpr{AndelOhjSS[2]} \%.


#Tabell, symptomvarighet
UtsRHnum <- round(table(RegData1aar$SympVarighUtstr, useNA='a')*100/Ntot1aar, 1)
UtsRH <- paste(UtsRHnum, '%', sep='')
names(UtsRH) <- c('Ingen utstrålende smerter', '< 3 mnd',
                  '3 - 12 mnd', '1 - 2 år', '> 2 år', 'Ikke besvart')	#, 'Tot. ant.')
xtable(cbind('Andeler'=UtsRH), caption=paste0('Varighet av nåværende utstrålende smerter, pasienter operert i ',
                                              rappAar),
       label="tab:Utstr", align=c('l','r'), digits=1)



#--Figurer
  dum <- RyggFigAndelerGrVar(valgtVar='SympVarighUtstr', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=1,
                             preprosess = 0, outfile='VarighUtstrAvdPro.pdf')
  dum <- RyggFigAndelerGrVar(valgtVar='SympVarighUtstr', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=8,
                             preprosess = 0, outfile='VarighUtstrAvdSS.pdf')
  dum <- RyggFigAndelTid(valgtVar='SympVarighUtstr', RegData=RegData,
                         preprosess = 0, hovedkat=1,outfile='VarighUtstrTidPro.pdf')
  dum <- RyggFigAndelTid(valgtVar='SympVarighUtstr', RegData=RegData,
                         preprosess = 0, hovedkat=8,outfile='VarighUtstrTidSS.pdf')


  dum <- RyggFigAndelerGrVar(valgtVar='SymptVarighRyggHof', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=1,
                             preprosess = 0, outfile='VarighRyggHofAvdPro.pdf')
  dum <- RyggFigAndelerGrVar(valgtVar='SymptVarighRyggHof', RegData=RegData1aar, datoFra=datoFra1aar, hovedkat=8,
                             preprosess = 0, outfile='VarighRyggHofAvdSS.pdf')

  RyggFigGjsnBox(RegData=RegData, aar = rappAar-1, valgtVar<-'SmBeinEndrPre', ktr=ktr,
                 outfile='FigBeinsmEndrPre.pdf')

  BeinsmLavPre <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='BeinsmLavPre', aar=(rappAar-1):rappAar,
                                      Ngrense = 20,
                                      hovedkat=1,  opKat=1, tidlOp=4,  outfile='FigBeinsmLavPre.pdf')

  KpInf3MndPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', aar=(rappAar-1):rappAar,
                                        Ngrense = 20,
                                        hovedkat = 1, outfile='FigKpInf3MndPro.pdf')
  KpInf3MndSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='KpInf3Mnd', aar=(rappAar-1):rappAar,
                                     Ngrense = 20,
                                     hovedkat = 8, outfile='FigKpInf3MndSS.pdf')

  KpInf3MndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='KpInf3Mnd',
                                     hovedkat = 1, outfile='FigKpInf3MndTidPro.pdf')
  KpInf3MndTidPro <- RyggFigAndelTid(RegData=RegData,  valgtVar='KpInf3Mnd',
                                     hovedkat = 8, outfile='FigKpInf3MndTidSS.pdf')

    DuraPro <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura',
                                   aar=(rappAar-1):rappAar, Ngrense = 20,
                                   opKat = 1, tidlOp = 4, hovedkat = 1, outfile='FigDuraPro.pdf')
  DuraSS <- RyggFigAndelerGrVar(RegData=RegData, valgtVar='PeropKompDura',
                                aar=(rappAar-1):rappAar, Ngrense = 20,
                                opKat = 1, tidlOp = 4, hovedkat = 8, outfile='FigDuraSS.pdf')
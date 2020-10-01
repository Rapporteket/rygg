#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#' Når ei fordeling består av flere variable, settes flerevar=1. Vi omdefinerer variablene slik at
#' alle gyldige registreringer (dvs. alle registreringer som skal telles med) er 0 eller 1.
#' De som har oppfylt spørsmålet
#' er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
#' som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de som 0.
#' Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'


RyggVarTilrettelegg  <- function(RegData=NULL, valgtVar, ktr=0,
                                 datoTil=Sys.Date(), hovedkat=99, figurtype='andeler'){ #grVar='',


      "%i%" <- intersect

      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      subtxt <- ''
      antDes <- 1
      if (figurtype == 'andelGrVar') {xAkseTxt <- 'Andel operasjoner (%)'}
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaalGrenser <- NA
      tittel <- 'Variabelvalg (valgtVar) feil angitt' # ? I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
      variable <- 'Ingen'
      #deltittel <- ''
      if (!is.null(RegData)) {RegData$Variabel <- 0}
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises??? her
      KIekstrem <- NULL

      #----Filtrere på at oppfølgigsskjema finnes
      #ktr kan ha verdiene 0, 1 eller 2
      #valgtVar <- 'arbstatus3mnd'
      if (length(grep('3mnd', valgtVar)) == 1) {
         ktr <- 1
         valgtVar <- sub('3mnd', '', valgtVar)}
      if (length(grep('12mnd', valgtVar) == 1)) {
         ktr <- 2
         valgtVar <- sub('12mnd', '', valgtVar)}
      varPrePost <- c('fornoydhet', 'nytte', 'EQ5DEndr','EQ5DEndr',
                      'OswEndr', 'SmBeinEndr', 'SmRyggEndr',
                      'OswEndrPre', 'SmBeinEndrPre', 'SmRyggEndrPre')
      if ((valgtVar %in% varPrePost) & (ktr==0)) {ktr <- 1}
      # Ferdig1a - pasientskjema
      # Ferdigstilt1b3mnd
      # Ferdigstilt1b12mnd
      # Ferdig2a - legeskjema
      #FILTRERING PÅ DENNE BLIR DET SAMME SOM Å FILTRERE PÅ VERSJON 3...!
      # if (valgtVar %in% varPrePost){
      #       if (ktr == 1) {RegData <- RegData[which(RegData$Ferdigstilt1b3mnd==1), ]}
      #       if (ktr == 2) {RegData <- RegData[which(RegData$Ferdigstilt1b12mnd==1), ]}
      # }
      ktrtxt <- c(' før operasjon', ' (3 mnd etter)', ' (12 mnd. etter)')[ktr+1]
      trekkfraDager <- c(0,90,365)[ktr+1]


      #-------------------------------------
      if (valgtVar=='deknRygg17') {        #andelerGrVar
            tittel <- 'Dekningsgrad, NKR Degenerativ Rygg, 2017'
            xAkseTxt <- 'dekningsgrad, NKR'
            KImaal <- 0.8
            data('deknRygg17', package = 'nkr') #paste0(valgtVar,'.Rdata')
            RegData <- deknRygg17 #paste0(valgtVar)
      }
      if (valgtVar=='deknNakke17') {        #andelerGrVar
            tittel <- 'Dekningsgrad, NKR Degenerativ Nakke, 2017'
            xAkseTxt <- 'dekningsgrad, NKR'
            KImaal <- 0.8
            data(deknNakke17, package = 'nkr')
            RegData <- deknNakke17 #paste0(valgtVar)
      }
      if (valgtVar=='deknRygg19') {        #andelerGrVar
         tittel <- 'Dekningsgrad, NKR Degenerativ Rygg, 2019'
         xAkseTxt <- 'dekningsgrad, NKR'
         KImaal <- 0.8
         data('deknRygg19', package = 'rygg') #paste0(valgtVar,'.Rdata')
         RegData <- deknRygg19 #paste0(valgtVar)
      }
      if (valgtVar=='deknNakke19') {        #andelerGrVar
         tittel <- 'Dekningsgrad, NKR Degenerativ Nakke, 2019'
         xAkseTxt <- 'dekningsgrad, NKR'
         KImaal <- 0.8
         data(deknNakke19, package = 'nakke')
         RegData <- deknNakke19 #paste0(valgtVar)
      }


      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse' #c('XX', 'test av tittel over to linjer') - funker!
            subtxt <- 'Aldersgrupper (år)'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (figurtype == 'andeler') {	#Fordelingsfigur
                  gr <- c(0,seq(20,90,10),150)
                  #gr <- c(seq(0, 100, 10),150)
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
                  grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')	#c(levels(RegData$VariabelGr)[-length(gr)], '90+')	#c(names(AndelLand)[-length(gr)], '90+')
                  #grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }

      if (valgtVar=='alder70') {	#AndelTid, AndelerGrVar
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder>=70)] <- 1
            varTxt <- 'over 70 år'
            tittel <- 'Pasienter over 70 år'
      }

      if (valgtVar == 'antibiotika') { #fordeling, AndelGrVar, AndelTid
            grtxt <- c('Nei', 'Ja', 'Ikke utfylt')
            #indKodeb <- which(KodebokRygg[,'Navn i Rapporteket']=='AntibiotikaV3'
            #grtxt <- KodebokRygg$Listetekst[indKodeb]
            RegData$VariabelGr <- factor(RegData$AntibiotikaV3, levels = c(0,1,9))
            #levels(RegData$VariabelGr) <- KodebokRygg$Listeverdier[indKodeb]
            tittel <- 'Er det gitt antibiotikaprofylakse?'
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
               RegData <- RegData[which(RegData$AntibiotikaV3 %in% 0:1), ]
                  RegData$Variabel <- RegData$AntibiotikaV3
                  tittel <- 'Fått antibiotika'
                  varTxt <- 'som har fått antibiotika'
            }
      }
      if (valgtVar=='antibiotikaMedikament'){ #fordeling
         tittel <- 'Antibiotikatyper'
         grtxt <- c("Cefalotin","Cefuroxim","Cefalexin","(Di)Kloksacillin","Klindamycin",
                    "Penicillin","Erytromycin","Ampicillin","Tetracyclin","Trimetoprim+Sulfonamid",
                    "Vancomycin","Fusidinsyre","Ciprofloxacin","Cefazolin","Annet")
         RegData$VariabelGr <- factor(RegData$AntibiotikaMedikament, levels = c(1:14,20))
         retn <- 'H'
      }

      if (valgtVar == 'arbstatus') { #Fordeling, AndelGrVar, AndelTid
            # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
            #  Gml: grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
            #		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
            datoTil <- min(datoTil, as.character(Sys.Date()-trekkfraDager))
            RegData$Arbstatus <- switch(as.character(ktr),
                                        '0' = RegData$ArbstatusPreV3,
                                        '1'= RegData$Arbstatus3mndV3,
                                        '2'= RegData$Arbstatus12mndV3)
            ind <- switch(as.character(ktr),
                          '0' = 1:dim(RegData)[1],
                          '1' = which(RegData$Ferdigstilt1b3mnd==1),
                          '2' = which(RegData$Ferdigstilt1b12mnd == 1))
            RegData <- RegData[ind, ]
            retn <- 'H'
            grtxt <- c("Fulltidsjobb","Deltidsjobb","Student/skoleelev",
                       "Alderspensjonist", "Arbeidsledig","Sykemeldt","Delvis sykemeldt",
                       "Arbeidsavklaringspenger", "Uførepensjonert","Ikke utfylt")
            RegData$VariabelGr <- factor(RegData$Arbstatus, levels = c(1:9,99))
            tittel <- paste0('Arbeidsstatus, ', ktrtxt)
            if (figurtype %in% c('andelTid', 'andelGrVar')) {
                  RegData <- RegData[which(RegData$Arbstatus %in% 1:9), ]
                  RegData$Variabel[which(RegData$Arbstatus %in% 6:9)] <- 1
                  tittel <- paste0('Mottar sykepenger' ,ktrtxt)
                  varTxt <- 'som mottar sykepenger'}
            sortAvtagende <- FALSE
      }

      if (valgtVar == 'ASA') { #fordeling, AndelGrVar
            grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'Ikke utfylt')
            subtxt <- 'Sykdomsgrad'
            RegData <- RegData[which(RegData$ASA %in% c(1:4,9)), ]  	#Antar ikke opererer døde...
            RegData$VariabelGr <- factor(RegData$ASA, levels = c(1:4,9))
            tittel <- 'ASA-grad (komorbiditet)'
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData <- RegData[which(RegData$ASA %in% 1:4), ]
                  RegData$Variabel[which(RegData$ASA > 2)] <- 1
                  tittel <- 'ASA-grad > II'
            }
            sortAvtagende <- FALSE
      }

      if (valgtVar == 'BMI') { #Fordeling, AndelGrVar
            #BMI > 30
            RegData <- RegData[which(RegData$BMI >10), ]
            gr <- c(0, 18.5, 25, 30, 35, 40, 1000)
            RegData$VariabelGr <- cut(RegData$BMI, breaks=gr, include.lowest=TRUE, right=FALSE)
            tittel <- 'Pasientenes BMI (Body Mass Index)'
            grtxt <- c('<18,5', levels(RegData$VariabelGr)[2:(length(gr)-2)],'40+')
            grtxt2 <- c('Undervekt', 'Normalvekt', 'Overvekt', 'Fedme', 'Fedme kl II', 'Fedme kl III')
            xAkseTxt <- '"Body Mass Index"'
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData$Variabel[which(RegData$BMI > 30)] <- 1
                  tittel <- 'Pasienter med fedme: (BMI>30)'
                  varTxt <- 'BMI>30'
                  sortAvtagende <- FALSE
            }}

      if (valgtVar == 'degSponFusj') { #AndelGrVar, AndelTid
            #hovedkat=10 #Degen. spondylolistese
            RegData <- RyggUtvalgEnh(RegData, hovedkat=10)$RegData
            RegData$Variabel[which(RegData$HovedInngrep ==5)] <- 1
            varTxt <- 'tilfeller'
            tittel <- 'Degen. spondylolistese operert med fusjonskirurgi'
            sortAvtagende <- F
            xAkseTxt <- 'Andel med fusjonskirurgi (%)'
      }
      if (valgtVar == 'degSponSSSten') { #AndelGrVar
            #(Først og fremst fusjonskirurgi)
            RegData$Variabel[which((RegData$RfSentr==1) & (RegData$RfSpondtypeDegen == 1))] <- 1
            tittel <- 'Degenerativ spondylolistese og sentral spinal stenose'
            varTxt <- 'som har dette'
            sortAvtagende <- FALSE
      }
      if (valgtVar == 'EQ5DPre') {#ford gjsnPre (gjsnBox)
            RegData <- RegData[which(RegData$EQ5DV3Pre > -0.6),]
            gr <- c(-0.6, seq(-0.2, 0.9, 0.1), 1)
            #gr <- c(0,seq(20,90,10),150)
            RegData$VariabelGr <- cut(round(RegData$EQ5DV3Pre,3), breaks=gr, include.lowest=TRUE, right=FALSE)
            #grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')
            grtxt <- levels(RegData$VariabelGr)
            #grtxt <- c(levels(RegData$VariabelGr)[-length(gr)], '90+')
            tittel <- 'EQ5D før operasjon'
            if (figurtype %in% c('gjsnBox', 'gjsnGrVar')){
               tittel <- 'EQ5D før operasjonen'}
            RegData$Variabel <- RegData$EQ5DV3Pre
            KIekstrem <- c(-0.6, 1)
      }

      if (valgtVar == 'EQ5DEndr') {#gjsnPre (gjsnBox)
            tittel <- paste0('forbedring av EQ5D', ktrtxt)
            RegData$Variabel <- switch(as.character(ktr),
                                       '1'= (RegData$EQ5DV33mnd - RegData$EQ5DV3Pre),
                                       '2'= (RegData$EQ5D12V3mnd - RegData$EQ5DV3Pre))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            Xlab <- 'EQ5D før operasjon'
            gr <- c(round(seq(-0.6,0.8,0.2),1),1.6)	#round(seq(-0.6,1.6,0.3),1)}
            RegData$Gr <- cut(RegData$EQ5DV3Pre, gr, right=F)
            GrNavn <- levels(RegData$Gr)
            AntGr <- length(GrNavn)
            GrNavn[AntGr] <- '0.8+'
            KIekstrem <- c(-1.6, 1.6)
      }

      if (valgtVar == 'EQangstPre') {#fordeling
         tittel <- 'Problemer med angst/depresjon'
         grtxt <- c('Ingen', 'Litt','Middels', 'Svært','Ekstremt', 'Ikke utfylt')
         RegData$VariabelGr <- factor(RegData$EqangstV3Pre, levels = c(1:5,9))
         subtxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
      }
      if (valgtVar == 'EQgangePre') { #fordeling
         tittel <- 'Problemer med gangfunksjon før operasjon'
         grtxt <- c('Ingen', 'Litt','Middels', 'Store', 'Ute av stand til å gå', 'Ikke utfylt')
         RegData$VariabelGr <- factor(RegData$EqgangeV3Pre, levels = c(1:5,9))
         subtxt <- 'problemer med gange'
      }
      if (valgtVar == 'erstatningPre') { #fordeling, AndelGrVar, #AndelTid
         #Pasientskjema. Andel med ErstatningPre
         #V2: Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
         #V3: [0,1,2,3,9]	["Nei","Ja","Planlegger","Innvilget","Ikke utfylt"]

         grtxt <- c('Nei', 'Ja', 'Planlegger', 'Innvilget', 'Ikke utfylt')
         #RegData$VariabelGr <- 9
         #indDum <- which(RegData$ErstatningPre %in% 1:4)
         #RegData$VariabelGr[indDum] <- RegData$ErstatningPre[indDum]
         RegData$VariabelGr <- factor(RegData$ErstatningPre, levels = c(0:3,9))
         tittel <- 'Har pasienten søkt erstatning?'
         if (figurtype %in% c('andelGrVar', 'andelTid')) {
            RegData <- RegData[which(RegData$ErstatningPre %in% 0:3), ]
            RegData$Variabel[which(RegData$ErstatningPre %in% c(1,2))] <- 1
            tittel <- 'Pasienten har søkt/planlegger å søke erstatning'
            varTxt <- 'som har søkt erstatning'
            sortAvtagende <- FALSE}
      }

      if (valgtVar =='fornoydhet') { #fordeling, AndelGrVar
         #3/12mndSkjema. Andel med helt Fornøyd (1)
         #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
         tittel <- paste('Fornøydhet', ktrtxt)
         datoTil <- min(datoTil, as.Date(Sys.Date()-trekkfraDager))
         RegData$Fornoyd <- switch(as.character(ktr),
                                   '1'= RegData$Fornoyd3mnd,
                                   '2'= RegData$Fornoyd12mnd)
         grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
         RegData$VariabelGr <- 9
         indDum <- which(RegData$Fornoyd %in% 1:5)
         RegData$VariabelGr[indDum] <- RegData$Fornoyd[indDum]
         RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
         if (figurtype %in% c('andelGrVar', 'andelTid')) {
            RegData <- RegData[which(RegData$Fornoyd %in% 1:5), ]
            RegData$Variabel[which(RegData$Fornoyd %in% 1:2)] <- 1 #
            #RegData$Variabel[which(RegData$Fornoyd ==1)] <- 1 #%in% 1:2
            tittel <- paste0('Fornøyde pasienter', ktrtxt)
         }
      }
      if (valgtVar=='hovedInngrep'){ #fordeling
         tittel <- 'Hovedinngrep'
         # grtxt <- c('Udefinerbart', 'Prolapskirurgi', 'Foramenotomi', 'Laminektomi',
         #            'Interspin. implantat', 'Fusjonskirurgi', 'Skiveprotese', 'Rev. av implantat')
         grtxt <- c('Udefinerbart', 'Prolapskirurgi', 'Dekompresjon', 'Laminektomi', 'Eksp. intersp impl.',
                    'Fusjonskirurgi', 'Deformitet', 'Rev. av implantat', 'Skiveprotese')
         RegData$VariabelGr <- factor(RegData$HovedInngrep, levels = 0:8)
         retn <- 'H'
      }
      if (valgtVar=='inngrep'){ #fordeling
         tittel <- 'Inngrepstyper'
         grtxt <- c('Andre inngrep', 'Prolaps mikro', 'Prolaps åpen', 'Midtlinjebev. dekomp.',
                    'Laminektomi', 'Eksp. intersp impl.', 'PLF', 'PLIF', 'TLIF', 'ALIF', 'XLIF',
                    'Udefinert fusjon', 'Osteotomi/deform.', 'Revisjon', 'Skiveprotese')
         RegData$VariabelGr <- factor(RegData$InngrepV2V3, levels = 0:14)
         retn <- 'H'
      }


      if (valgtVar=='komorbiditet') {
         tittel <- 'Komorbiditet'
         retn <- 'H'
         flerevar <- 1
         RegData <- RegData[which(RegData$OpDato >= '2019-01-01'), ]
         # variableV2 <- c('SykdAndreRelevanteSykdBechtrew', 'SykdAnnenendokrin', 'SykdAnnenreumatisk',
         #               'SykdCerebrovaskular', 'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHoftekneartose',
         #               'SykdHypertensjon', 'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk',
         #               'SykdKroniskSmerterMuskelSkjelettsyst', 'SykdOsteoporose', 'SykDprebetesMellitus',
         #               'SykdReumatoidartritt', 'SykdVaskularClaudicatio', 'Sykd')
         variable <- c('SykdAndreRelevanteSykdBechtrew', 'SykdAnnenendokrin', 'SykdAnnenreumatisk',
                       'SykdCerebrovaskular', 'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHoftekneartose',
                       'SykdHypertensjon', 'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk',
                       'SykdOsteoporose', 'SykdOsteoporoseBrudd', 'SykDprebetesMellitus',                   #'SykdKroniskSmerterMuskelSkjelettsyst',
                       'SykdPolynevropati', 'SykdProstatisme',
                       'SykdReumatoidartritt', 'SykdVaskularClaudicatio', 'SykdGeneralisertSmSyndr',
                       'Sykd')

         grtxt <- c('Bechterew', 'Endokrin', 'Reumatisk',
                    'Cerebrovaskulær', 'Depresjon/Angst', 'Hjerte-kar', 'Hoftekneartose',
                    'Hypertensjon', 'Kreft', 'Lungesykd.', 'Nevrologisk sykd.',
                    'Osteoporose', 'Osteoporosebrudd', 'Diabetes Mell.',                         #'Muskel-/skjelettsm.',
                    'Polynevropati', 'Prostatisme',
                    'Reumatoid artritt', 'Vask. Claudicatio', 'Gen.smertesyndrom',
                    'Tot. Komorb.')

         cexgr <- 0.9	#Kan endres for enkeltvariable
      }

      if (valgtVar=='komplPer') { #fordeling
         tittel <- 'Peroperative komplikasjoner'
         retn <- 'H'
         flerevar <- 1
         #NB <- '(Komplikasjoner rapporteres kun f.o.m. 2010)'
         variable <- c('PeropKompDura', 'PeropKompFeilnivSide', 'PeropKompNerve', 'PeropKompTransfuBlodning',
                       'PeropKompKardio','PeropKompFeilplassImp','PeropKompResp','PeropKompAnafy')
         grtxt <- c('Durarift', 'Operert feil nivå/side', 'Nerveskade', 'Transfusjonskrevende blødning',
                    'Kardiovaskulær komplikasjon', 'Feilplassert implantat', 'Respiratorisk komplikasjon', 'Anafylaksi')
      }

      if (valgtVar=='komplPost') {
         tittel <- 'Pasientrapporterte komplikasjoner'
         datoTil <- min(datoTil, as.character(Sys.Date()-90))
         retn <- 'H'
         flerevar <- 1
         #Andel kun av de som har svart på 3 mnd ktr:
         RegData <- RegData[which(RegData$Ferdigstilt1b3mnd==1), ]
         # variableV2 <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpMiktProb3Mnd','KpUVI3Mnd',
         #               'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd', 'Kp3Mnd')
         variable <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpUVI3Mnd', #'KpMiktProb3Mnd',
                       'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd', 'Kp3Mnd')
         grtxt <- c('Overfladisk sårinfeksjon', 'Dyp sårinfeksjon',
                    'Urinveisinfeksjon', 'Pneumoni', #'Problem, vannlatning/avføring',
                    'Transf./opr. pga. blødning', 'DVT','Lungeemboli', 'Postop. kompl. totalt') #'Tot. komplikasjoner'
      }

      if (valgtVar == 'kp3Mnd') { #AndelGrVar
         #Komplikasjoner 0:nei, 1:ja
         RegData <- RegData[which(RegData$Ferdigstilt1b3mnd ==1), ]
         variable <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpUVI3Mnd', #'KpMiktProb3Mnd',
                       'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd')
         RegData$Kp3Mnd <- NULL
         RegData$Kp3Mnd[rowSums(RegData[ ,variable], na.rm = T) > 0] <- 1
         RegData$Variabel <- RegData$Kp3Mnd
         tittel <- 'Pasientrapporterte komplikasjoner (%)'
         sortAvtagende <- FALSE
      }
      if (valgtVar == 'kpInf3Mnd') { #AndelGrVar, AndelTid
         #Komplikasjoner 0:nei, 1:ja
         RegData <- RegData[which(RegData$Ferdigstilt1b3mnd ==1), ]
         RegData$Variabel <- NULL
         RegData$Variabel[rowSums(RegData[ ,c('KpInfOverfla3Mnd', 'KpInfDyp3Mnd')], na.rm = T) > 0] <- 1
         RarTxt <- 'tilfeller'
         tittel <- 'Sårinfeksjon, pasientrapportert'
         sortAvtagende <- FALSE
         xAkseTxt <- 'Andel sårinfeksjoner (%)'
         #KImaalRetn <- 'lav'
         if (hovedkat == 1) {KImaalGrenser <- c(0,2)}
         if (hovedkat == 8) {KImaalGrenser <- c(0,3)}
      }

      if (valgtVar=='liggedogn') {#fordeling, gjsnGrVar, andeler, gjsnTid
            #liggedogn
            #For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
            tittel <- 'Liggetid ved operasjon'
            dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
            RegData$Liggedogn[dagind]<-0
            RegData <- RegData[which(RegData$Liggedogn>=0),]
            RegData$Variabel <- RegData$Liggedogn #gjsnGrVar
            gr <- c(0:7,100)
            RegData$VariabelGr <- cut(RegData$Liggedogn, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c(0:6, '7+')
            xAkseTxt <- 'Antall liggedøgn' #(subtxt
            subtxt <- 'døgn'
            if (figurtype=='gjsnGrVar') {tittel <- 'liggetid'}
            sortAvtagende <- 'F'
            TittelVar <- 'Liggetid ved operasjon'
            ytxt1 <- 'liggetid'
            KIekstrem <- c(0, 20)
      }
      if (valgtVar=='liggetidPostOp') {#fordeling, gjsnGrVar, andeler, gjsnTid
         #liggedogn
         #For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
         tittel <- 'Liggetid etter operasjon'
         RegData <- RegData[which(RegData$OpDato >= '2019-01-01'), ]
         dagind <- which( (is.na(RegData$LiggetidPostOp) | is.nan(RegData$LiggetidPostOp))  & RegData$Dagkirurgi==1)
         RegData$LiggetidPostOp[dagind]<-0
         RegData <- RegData[which(RegData$LiggetidPostOp>=0),]
         RegData$Variabel <- RegData$LiggetidPostOp #gjsnGrVar
         gr <- c(0:7,100)
         RegData$VariabelGr <- cut(RegData$LiggetidPostOp, breaks=gr, include.lowest=TRUE, right=FALSE)
         grtxt <- c(0:6, '7+')
         xAkseTxt <- 'Antall liggedøgn' #(subtxt
         subtxt <- 'døgn'
         if (figurtype=='gjsnGrVar') {tittel <- 'liggetid'}
         sortAvtagende <- 'F'
         TittelVar <- 'Liggetid ved operasjon'
         ytxt1 <- 'liggetid'
         KIekstrem <- c(0, 20)
      }
      if (valgtVar == 'misfornoyd') { #AndelGrVar	#%in% c('Misfor3mnd','Misfor12mnd')) { #AndelGrVar
            #3/12mndSkjema. Andel med Misfornøyd/litt misfornøyd (1,2)
            #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
            RegData$Misfornoyd <- switch(as.character(ktr),
                                         '1'= RegData$Fornoyd3mnd,
                                         '2'= RegData$Fornoyd12mnd)
            RegData <- RegData[which(RegData$Misfornoyd %in% 1:5), ]
            RegData$Variabel[which(RegData$Misfornoyd %in% 4:5)] <- 1
            tittel <- paste0('Misfornøyde pasienter' ,ktrtxt)
            sortAvtagende <- FALSE
      }
      if (valgtVar == 'morsmal') { #fordeling AndelGrVar, AndelTid
            grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ikke utfylt')
            tittel <- 'Morsmål'
            RegData$VariabelGr <- factor(RegData$Morsmal, levels = c(1:3,9))
            if (figurtype %in% c('andelTid', 'andelGrVar')) {
                  RegData <- RegData[which(RegData$Morsmal %in% 1:3), ]
                  RegData$Variabel[which(RegData$Morsmal %in% 2:3)] <- 1
                  tittel <- 'Fremmedspråklige (ikke norsk som morsmål)'
                  varTxt <- 'fremmedspråklige'
                  sortAvtagende <- F}
      }

      if (valgtVar == 'nytte') { #fordeling, AndelGrVar
            #Andel med helt bra/mye bedre (1:2)
            #Kode 1:7: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
            #				'Verre enn noen gang', 'Ukjent')
            tittel <- c('Hvilken nytte har du hatt av operasjonen?',ktrtxt)
            RegData$Nytte <- switch(as.character(ktr),
                                    '1'=RegData$Nytte3mnd,
                                    '2'=RegData$Nytte12mnd)
            datoTil <- min(datoTil, as.character(Sys.Date()-trekkfraDager))
            retn <- 'H'
            RegData$VariabelGr <- 9
            indDum <- which(RegData$Nytte %in% 1:7)
            RegData$VariabelGr[indDum] <- RegData$Nytte[indDum]
            RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:7,9))
            grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
                       'Verre enn noen gang', 'Ukjent')
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
                  RegData$Variabel[RegData$Nytte %in% 1:2] <- 1
                  tittel <- paste0('Helt bra eller mye bedre' , ktrtxt)
            }}

      if (valgtVar=='opInd') { #fordeling
            tittel <- 'Operasjonsindikasjon'
            retn <- 'H'
            flerevar <- 1
            variable <- c('OpIndCauda', 'OpIndParese') #, 'OpIndSme')
            grtxt <- c('Cauda equlina', 'Parese')# , 'Smerter')
      }
      if (valgtVar == 'opIndPareseGrad') {#fordeling
            tittel <- 'Operasjonsindikasjon, paresegrad'
            grtxt <- c(0:5, 'Ukjent')
            RegData <- RegData[which(RegData$OpIndParese ==1),]
            indDum <- which(RegData$OpIndPareseGrad %in% 0:5)
            RegData$VariabelGr <- 9
            RegData$VariabelGr[indDum] <- RegData$OpIndPareseGrad[indDum]
            RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:5,9))
      }

      # if (valgtVar == 'opIndSmeType') {#fordeling
      #       tittel <- 'Operasjonsindikasjon, smertetype'
      #       grtxt <- c('Rygg/hofte', 'Bein', 'Begge deler', 'Ukjent')
      #       RegData <- RegData[which(RegData$OpIndSme ==1),]
      #       indDum <- which(RegData$OpIndSmeType %in% 1:3)
      #       RegData$VariabelGr <- 9
      #       RegData$VariabelGr[indDum] <- RegData$OpIndSmeType[indDum]
      #       RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
      # }
      if (valgtVar == 'opKat') { #fordeling
            #retn <- 'H'
            tittel <- 'Operasjonskategori'
            grtxt <- c('Elektiv', 'Akutt', '1/2-Akutt', 'Ikke utfylt')
            RegData$VariabelGr <- factor(RegData$OpKat, levels = c(1:3,9))
      }

      if (valgtVar == 'OswEndr') {
         #Forbedring=lavere Oswestry
         tittel <- paste0('forbedring av ODI', ktrtxt)
         RegData$Variabel <- switch(as.character(ktr),
                                    '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                    '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
         RegData <- RegData[which(!is.na(RegData$Variabel)),]
         KIekstrem <- c(-100, 100)
      }
      if (valgtVar == 'OswEndrPre') { #Sammenligner endring og prescore
         #Forbedring=lavere Oswestry
         tittel <- paste0('forbedring av ODI', ktrtxt)
         RegData$Variabel <- switch(as.character(ktr),
                                    '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                    '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
         RegData <- RegData[which(!is.na(RegData$Variabel)),]
         KIekstrem <- c(-100, 100)
         Xlab <- 'Oswestry før operasjon'
         gr <- c(seq(0,90,10), 101)
         RegData$Gr <- cut(RegData$OswTotPre, gr, right=F)
         GrNavn <- levels(RegData$Gr)
         GrNavn[length(GrNavn)] <- '[90,100]'
      }


      if (valgtVar == 'OswEndrLav') { #AndelGrVar
            #Mislykkede operasjoner
            RegData$OswEndr <- switch(as.character(ktr),
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(RegData$OswEndr >= -100), ]
            RegData$Variabel[which(RegData$OswEndr <13)] <- 1
            tittel <- paste0('Forbedring av Oswestry-skår < 13 poeng', ktrtxt)
            sortAvtagende <- F
      }
      if (valgtVar == 'OswEndr20') { #AndelGrVar, andelTid
            #Mislykkede operasjoner
            RegData$OswEndr <- switch(as.character(ktr),
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(RegData$OswEndr >= -100), ]
            RegData$Variabel[which(RegData$OswEndr >=20)] <- 1
            varTxt <- 'med >20 poeng forbedring'
            tittel <- paste0('Forbedring av Oswestry-skår >= 20 poeng', ktrtxt)

      }

      if (valgtVar == 'OswEndr30pst') { #AndelGrVar, andelTid
            #Andel med klinisk signifikant forbedring i Oswestry-skår.
            #Forbedring = nedgang
            RegData$OswPst <- switch(as.character(ktr),
                                     '1' = (RegData$OswTotPre - RegData$OswTot3mnd)/RegData$OswTotPre*100,
                                     '2' = (RegData$OswTotPre - RegData$OswTot12mnd)/RegData$OswTotPre*100)
            RegData <- RegData[which(RegData$OswPst>=-1000), ]
            RegData$Variabel[which(RegData$OswPst >=30)] <- 1
            varTxt <- 'med \u2265 30 % forbedring'
            tittel <- paste0('Minst 30% forbedring av Oswestry-skår', ktrtxt)
      }
      if (valgtVar == 'Osw22') { #AndelGrVar
            #Andel med Oswestry-skår under 23 etter op.
            RegData$OswPost <- switch(as.character(ktr),
                                      '1' = RegData$OswTot3mnd,
                                      '2' = RegData$OswTot12mnd)
            RegData <- RegData[which(RegData$OswPost>=0), ]
            RegData$Variabel[which(RegData$OswPost <= 22)] <- 1
            tittel <- paste0('Oswestry-skår < 23 poeng', ktrtxt)
      }
      if (valgtVar == 'Osw48') { #AndelGrVar
            #Andel med Oswestry-skår fortsatt over 48.
            RegData$OswPost <- switch(as.character(ktr),
                                      '1' = RegData$OswTot3mnd,
                                      '2' = RegData$OswTot12mnd)
            RegData <- RegData[which(RegData$OswPost>=0), ]
            RegData$Variabel[which(RegData$OswPost >48)] <- 1
            tittel <- paste0('Oswestry-skår > 48 poeng', ktrtxt)
            sortAvtagende <- F
      }

      if (valgtVar == 'OswTotPre') { #fordeling, gjsnBox, gjsnGrVar
         RegData$Variabel <- RegData$OswTotPre
         RegData <- RegData[which(!is.na(RegData$Variabel)),]
         tittel <- 'oswestryskår før operasjonen'
         gr <- seq(0, 100, 10)
         RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
         grtxt <- levels(RegData$VariabelGr)
         xAkseTxt <- 'skår'
         KIekstrem <- c(0, 100)
         sortAvtagende <- FALSE
      }

       if (valgtVar=='peropKomp') { #AndelGrVar
            #Komplikasjoner ved operasjon
            #Kode 1:Ja,  tomme:Nei
            RegData$Variabel[which(RegData$PeropKomp == 1)] <- 1
            tittel <- 'Komplikasjoner ved operasjon'
            varTxt <- 'med komplikasjoner'
            sortAvtagende <- FALSE
      }
      if (valgtVar=='peropKompDura') { #AndelGrVar
            #Durarift ved operasjon
            #Kode 1:Ja,  tomme:Nei
            RegData$Variabel[which(RegData$PeropKompDura == 1)] <- 1
            tittel <- 'Komplikasjon ved operasjon: Durarift'
            sortAvtagende <- FALSE
            xAkseTxt <- 'Andel durarift (%)'
            varTxt <- 'med durarift'
            #KImaalRetn <- 'lav'
            if (hovedkat == 1) {KImaalGrenser <- c(0,2,20)}
            if (hovedkat %in% c(5,9)) {KImaalGrenser <- c(0,3,20)}
      }
      if (valgtVar=='radUnders') { #fordeling
            tittel <- 'Radiologisk undersøkelse'
            retn <- 'H'
            flerevar <- 1
            variable <- c('RvCt', 'RvMr',  'RvRtgLscol', 'RvFunksjo') #'RvRadigr-it', 'RvDiscogr-ut', 'RvDpregblok',
            grtxt <- c('CT', 'MR', 'Rtg.LS-columna', 'Fleks./Ekst.') #'Radikulografi-ut', 'Diskografi-ut', 'Diag.blokade'-finnes,
            #variable <- c('RvCt', 'RvMr', 'RvRadigr', 'RvDiscogr', 'RvDpregblok', 'RvRtgLscol', 'RvFunksjo')
            #grtxt <- c('CT', 'MR', 'Radikulografi', 'Diskografi', 'Diag.blokade', 'Rtg.LS-columna', 'Fleks./Ekst.')
      }
       if (valgtVar == 'regForsinkelse') {  #Andeler, GjsnGrVar
         #Verdier: 0-3402
         RegData <- RegData[which(RegData$DiffUtFerdig > -1), ]
         tittel <- switch(figurtype,
                          andeler='Tid fra utskriving til ferdigstilt registrering',
                          andelGrVar = 'Mer enn 30 dager fra utskriving til ferdig registrering') #
         subtxt <- 'døgn'
         gr <- c(0,1,7,14,30,90,365,5000) #gr <- c(seq(0, 90, 10), 1000)
         RegData$VariabelGr <- cut(RegData$DiffUtFerdig, breaks = gr, include.lowest = TRUE, right = TRUE)
         grtxt <- c('1', '(1-7]', '(7-14]', '(14-30]', '(30-90]', '(90-365]', '>365')
         cexgr <- 0.9
         xAkseTxt <- 'dager'
         sortAvtagende <- FALSE

         if (figurtype == 'andeler') {	#Fordelingsfigur
            gr <- c(seq(0,98,7), 2000)
            RegData$VariabelGr <- cut(RegData$DiffUtFerdig, breaks=gr, include.lowest=TRUE, right=FALSE)
            #plot(RegData$VariabelGr)
            grtxt <- c(1:14, '>3 mnd.')
            subtxt <- 'innen gitte uker etter utskriving'
         }

         if (figurtype %in% c('andelTid', 'andelGrVar')) {
            RegData$Variabel[which(RegData$DiffUtFerdig >90)] <- 1
            tittel <- 'Registrert for sent for 3 mnd. oppfølging'
            varTxt <- 'for sent registrert'
            sortAvtagende <- F}
      }

      if (valgtVar=='roker') { #fordeling, AndelGrVar, #AndelTid
            #PasientSkjema. Andel med Roker=1
            #Kode 0,1,tom: Nei, Ja Ukjent
            #V3: [0,1,2,9] ["Nei","Ja","Har røkt tidligere","Ikke utfylt"]
            tittel <- 'Røyker du?'
            if (figurtype == 'andeler'){
            grtxt <- c("Nei","Ja","Har røkt tidligere","Ikke utfylt")
            RegData <- RegData[which(RegData$RokerV3 %in% c(0:2,9)), ]
            RegData$VariabelGr <- factor(RegData$RokerV3, levels = c(0:2,9))
            }
            if (figurtype %in% c('andelGrVar', 'andelTid')){
                  # RegData <- RegData[which(RegData$RokerV3 %in% 0:2), ]
                  # RegData$Variabel[RegData$RokerV3==1] <- 1
                  RegData <- RegData[which(RegData$RokerV2 %in% 0:1), ]
                  RegData$Variabel <- RegData$RokerV2
                  tittel <- 'Røykere'
                  varTxt <- 'røykere'
                  sortAvtagende <- FALSE}
      }


      if (valgtVar == 'saardren') { #AndelGrVar
            #LegeSkjema. Andel med Saardren=1
            #Kode 0,1,tom: Nei, Ja Ukjent
            grtxt <- c('Nei', 'Ja', 'Ikke utfylt')
            RegData$VariabelGr <- factor(RegData$Saardren, levels = c(0:1,9))
            tittel <- 'Sårdren'
            sortAvtagende <- F
            if (figurtype %in% c('andelGrVar', 'andelTid')){
                  RegData <- RegData[which(RegData$Saardren %in% 0:1), ]
                  RegData$Variabel <- RegData$Saardren
                  tittel <- 'Andel som får sårdren (%)'
            }
      }
      if (valgtVar == 'sivilStatus') {
         tittel <- 'Sivilstatus'
         #V2: grtxt <- c('Gift', 'Samboer', 'Enslig', 'Ukjent')
         grtxt <- c('Gift/Samboer', 'Enslig', 'Ikke utfylt')
         RegData$VariabelGr <- factor(RegData$SivilStatusV3, levels = c(1:2,9))
      }
       if (valgtVar == 'smStiPreHypp') { #fordeling
         tittel <- 'Hyppighet av smertestillende før operasjonen'
         grtxt <- c('Sjeldnere', 'Månedlig', 'Ukentlig', 'Daglig', 'Flere ganger om dagen', 'Ikke utfylt') #'Oftere',
         #RegData <- RegData[which(RegData$SmHyppPre %in% c(1:5, 9)), ]
         RegData$VariabelGr <- factor(RegData$SmHyppPre, levels = c(1:5,9))
         retn <- 'H'
      }

      if (valgtVar == 'smBeinEndr') {
         tittel <- paste0('bedring av beinsmerter ',ktrtxt)
         RegData <- RegData[which((RegData$SmBePre %in% 0:10) &
                                     (RegData$SmBe3mnd %in% 0:10) & (RegData$SmBe3mnd %in% 0:10)), ]
         RegData$Variabel <- switch(as.character(ktr),
                                    '1'= (RegData$SmBePre - RegData$SmBe3mnd),
                                    '2'= (RegData$SmBePre - RegData$SmBe12mnd))
         #RegData <- RegData[which(!is.na(RegData$Variabel)),]
         KIekstrem <- c(-10,10)
      }

      if (valgtVar == 'smBePre') { #fordeling, gjsn
         if (figurtype=='andeler'){
            RegData$VariabelGr <- RegData$SmBePre
            levels(RegData$VariabelGr) <- c(0:10,99)
            grtxt <- c(as.character(0:10), 'Ikke utfylt')
            tittel <- 'Beinsmerter før operasjon'
         }
         if (figurtype %in% c('gjsnBox', 'gjsnGrVar')){
            RegData <- RegData[RegData$SmBePre %in% 0:10, ]
            RegData$Variabel <- RegData$SmBePre
            tittel <- 'beinsmerter før operasjonen'
         }
         xAkseTxt <- 'skår'
         KIekstrem <- c(0, 10)
      }
      if (valgtVar == 'smBePreLav') { #AndelGrVar, AndelTid
         #Lav beinsmerte og ingen parese. (Først og fremst prolaps)
         RegData <- RegData[which(RegData$SmBePre %in% 0:10),]
         RegData$Variabel[which((RegData$OpIndParese==0) & (RegData$SmBePre < 3.5))] <- 1
         sortAvtagende <- F
         tittel <- "Lite beinsmerter og ingen parese" #expression("Lite beinsmerter og ingen parese") #paste0('Beinsmerte ', expression(""<="3"), ' og ingen parese')
         #intToUtf8(2264)
         #KImaalRetn <- 'lav'
         KImaalGrenser <- c(0,3,20)
         varTxt <- 'med manglende indikasjon'
      }

      if (valgtVar == 'smRyggEndr') {#gjsn
         tittel <- paste0('forbedring av  ryggsmerter', ktrtxt)
         RegData <- RegData[which((RegData$SmRyPre %in% 0:10) &
                                     (RegData$SmRy3mnd %in% 0:10) & (RegData$SmRy12mnd %in% 0:10)), ]
         RegData$Variabel <- switch(as.character(ktr),
                                    '1'= RegData$SmRyPre - RegData$SmRy3mnd,
                                    '2'= RegData$SmRyPre - RegData$SmRy12mnd)
         KIekstrem <- c(-10,10)
      }

      if (valgtVar == 'smRyggPre') { #fordeling, gjsn
         if (figurtype=='andeler'){
            RegData$VariabelGr <- RegData$SmRyPre
            levels(RegData$VariabelGr) <- c(0:10,99)
            grtxt <- c(as.character(0:10), 'Ikke utfylt')
            tittel <- 'Ryggsmerter før operasjon'
         }
         if (figurtype %in% c('gjsnBox', 'gjsnGrVar')){
            RegData <- RegData[RegData$SmRyPre %in% 0:10, ]
            RegData$Variabel <- RegData$SmRyPre
            tittel <- 'ryggsmerter før operasjonen'
         }
      }

      if (valgtVar == 'smStiPre') { #fordeling, andelGrVar/Tid
         tittel <- 'Bruk av smertestillende før operasjonen'
         grtxt <- c('Nei', 'Ja', 'Ikke utfylt')
         RegData$VariabelGr <- factor(RegData$SmStiPre, levels = c(0:1,9))
         if (figurtype %in% c('andelGrVar', 'andelTid')) {
            RegData <- RegData[which(RegData$SmStiPre %in% 0:1), ]
            RegData$Variabel <- RegData$SmStiPre
            varTxt <- 'som brukte smertestillende'
            sortAvtagende <- F}
      }
      if (valgtVar %in% c('symptVarighRyggHof','sympVarighUtstr')) { #fordeling, andelGrVar/Tid
            grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ikke utfylt')
            RegData$VariabelGr <- factor(switch(valgtVar,
                             symptVarighRyggHof = RegData$SymptVarighRyggHof,
                             sympVarighUtstr = RegData$SympVarighUtstr),
                             levels = c(1:5,9)
            )
            tittel <- switch(valgtVar,
                             symptVarighRyggHof = 'Varighet av rygg-/hoftesmerter',
                             sympVarighUtstr = 'Varighet av utstrålende smerter'
            )
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData <- RegData[which(RegData$VariabelGr %in% 1:5), ]
                  RegData$Variabel[which(RegData$VariabelGr %in% 4:5)] <- 1
                  varTxt <- 'med varighet minst 1 år'
                  tittel <- switch(valgtVar,
                                   symptVarighRyggHof ='Varighet av rygg-/hoftesmerter minst ett år',
                                   sympVarighUtstr = 'Varighet av utstrålende smerter minst ett år')
                  sortAvtagende <- F}
            if (valgtVar == 'sympVarighUtstr') {KImaalGrenser <- c(0,20,100)}
      }


      if (valgtVar == 'tidlOpr') {
            tittel <- 'Tidligere ryggoperert?'
            retn <- 'H'
            grtxt <- c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon') #, 'Ukjent')
            #Versjon 2:  RegData$VariabelGr <- 9
            # indDum <- which(RegData$TidlOpr %in% 1:4)
            # RegData$VariabelGr[indDum] <- RegData$TidlOpr[indDum]
            #Definert i preprosess: RegData$TidlOp <- 9
            # RegData$TidlOp[RegData$TidlIkkeOp==1] <- 4
            # RegData$TidlOp[RegData$TidlOpsammeNiv==1] <- 1
            # RegData$TidlOp[RegData$TidlOpAnnetNiv==1] <- 2
            # RegData$TidlOp[RegData$TidlOpsammeNiv==1 & RegData$TidlOpAnnetNiv==1] <- 3
            RegData$VariabelGr <- factor(RegData$TidlOpr, levels = c(1:4))
            #test <- rowSums(RegData[,var])
            #RegData[test>1,var]
      }
      if (valgtVar=='tidlOprAntall') {
            tittel <- 'Antall tidligere operasjoner'
            gr <- c(-1:5, 1000)
            RegData$Variabel <- -1
            RegData$Variabel[RegData$TidlOpr==4] <- 0
            indDum <- which(RegData$TidlOprAntall>-1)
            RegData$Variabel[indDum] <- RegData$TidlOprAntall[indDum]
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('Ukjent', 0:4,paste0('5-', max(RegData$TidlOprAntall, na.rm=T)))
      }

      if (valgtVar == 'tidlOp3'){ #AndelTid, AndelGrVar
            RegData$Variabel[RegData$TidlOprAntall>2] <- 1
            varTxt <- 'med >2 tidl. operasjoner'
            tittel <- 'Flere enn to tidligere operasjoner'
            sortAvtagende <- F
      }


      if (valgtVar == 'trombProfyl') { #AndelGrVar, AndelTid
         #Legeskjema
         tittel <- 'Tromboseprofylakse gitt ifm. operasjon'
         #if (figurtype %in% c('andelGrVar', 'andelTid')){
            RegData <- RegData[which(RegData$PostopTrombProfyl %in% 0:1), ]
            RegData$Variabel <- RegData$PostopTrombProfyl
            varTxt <- 'fått tromboseprofylakse'
            sortAvtagende <- F #}
      }


      if (valgtVar == 'uforetrygdPre') { #fordeling, AndelGrVar, AndelTid
            #PasientSkjema. Andel med UforetrygdPre ja og planlegger
            #Kode V2 1:4,tom: 'Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
            #V3: [0,1,2,3,9]	["Nei","Ja","Planlegger å søke","Er allerede innvilget","Ikke utfylt"]
            tittel <- 'Har pasienten søkt uføretrygd?'
            #retn <- 'H'
            grtxt <- c('Nei', 'Ja', 'Planlegger søknad', 'Innvilget', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$UforetrygdPre, levels = c(0:3,9))
            if (figurtype %in% c('andelGrVar', 'andelTid')){
                  RegData <- RegData[which(RegData$UforetrygdPre %in% 0:3), ]
                  RegData$Variabel[which(RegData$UforetrygdPre %in% c(1,2))] <- 1
                  varTxt <- 'søkt/planlagt å søke'
                  tittel <- 'Har søkt/planlegger å søke uføretrygd før op.'
                  sortAvtagende <- F}
      }


      if (valgtVar == 'utd') { #AndelGrVar, AndelTid
            #PasientSkjema. Andel med Utdanning 4 el 5
            #Kode V2 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
            #Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
            #V3: [1,2,3,4,5,9]	["Grunnskole 7-10 år, framhaldsskole eller folkehøyskole",
            grtxt <- c('Grunnskole, 7-10år','Real-, yrkes- el vg skole', #'Real-, yrkes-/yrkesfaglig vg skole',
                       'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+',
                       'Ikke utfylt')
            RegData$VariabelGr <- factor(RegData$Utd, levels = c(1:5,9))
            retn <- 'H'
            tittel <- 'Høyeste fullførte utdanning'
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData$Variabel[which(RegData$Utd %in% 4:5)] <- 1
                  varTxt <- 'med høyere utdanning'
                  tittel <- 'Andel høyskole-/universitetsutdannede'
            }}


      if (valgtVar == 'ventetidSpesOp') { #Fordeling, AndelGrVar, AndelTid
         grtxt <- c("< 3 mnd.","3-6 mnd","6-12 mnd.","> 12 mnd.","Ikke utfylt")
         RegData$VariabelGr <- factor(RegData$VentetidSpesialistTilOpr, levels = c(1:4,9))
         retn <- 'H'
         tittel <- 'Ventetid fra operasjon bestemt til utført'
         if (figurtype %in% c('andelGrVar', 'andelTid')) {
            RegData <- RegData[which(RegData$VentetidSpesialistTilOpr %in% 1:4),]
            RegData$Variabel[which(RegData$VentetidSpesialistTilOpr == 1)] <- 1
            varTxt <- 'ventet <3mnd'
            tittel <- 'Ventetid < 3 mnd. fra operasjon bestemt til utført'
            KImaalGrenser <- c(0,50,80,100)
         }}

      if (valgtVar == 'verre') { #AndelGrVar		#%in% c('Verre3mnd','Verre12mnd')) {
            #3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
            #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
            #				'Verre enn noen gang', 'Ukjent')
            RegData$Nytte <- switch(as.character(ktr),
                                    '1'=RegData$Nytte3mnd,
                                    '2'=RegData$Nytte12mnd)
            RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
            RegData$Variabel[RegData$Nytte %in% 6:7] <- 1
            tittel <- paste0('Mye verre/verre enn noen gang' , ktrtxt)
            sortAvtagende <- F
      }



      RegData <- RegData[which(!is.na(RegData$Variabel)),]

      UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt,
                     KImaalGrenser=KImaalGrenser, KIekstrem=KIekstrem, #KImaal=KImaalRetn,
                     retn=retn, subtxt=subtxt, tittel=tittel, antDes=antDes,
                     flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}

# promData <- read.table('C:/Registerdata/rygg/proms2022-12-09.csv',
#            sep=';', header=T, encoding = 'UTF-8')

# promData <- readr::read_csv2('C:/Registerdata/Rygg/proms2022-12-09.csv')
#
# promData1 <- dplyr::rename(promData,
#               EpromRegType = REGISTRATION_TYPE,
#               EpromUtlopDato = EXPIRY_DATE,
#               EpromPurreDato = REMINDER_DATE,
#               EpromSendt = TSSENDT,
#               EpromMottatt = TSRECEIVED,
#               EpromVarslingsKanal = NOTIFICATION_CHANNEL,
#               EpromUtsendRegel = DISTRIBUTION_RULE,
#               EpromStatus = STATUS,
#               EpromFeilKode = FORM_ORDER_STATUS_ERROR_CODE,
#               EpromOppdatert = TSUPDATED)
#
# names(promData1)


#E-prombesvarelser
# EpromRegType er enten:
#   PATIENTFOLLOWUP: 3-mnd ePROM
#   PATIENTFOLLOWUP12: 12-mnd ePROM
#   PATIENTFOLLOWUP_3_PiPP: 3-mnd PiPP
#   PATIENTFOLLOWUP_3_PiPP_REMINDER: 3-mnd PiPP purring
#   PATIENTFOLLOWUP_12_PiPP: 12-mnd PiPP
#   PATIENTFOLLOWUP_12_PiPP_REMINDER: 12-mnd PiPP purring
# EpromUtlopDato er utløpsdatoen
# EpromPurreDato er purredatoen
# EpromSendt er tidspunkt for utsendelse
# EpromMottatt er tidspunkt for mottak
# EpromVarslingsKanal er definert av Hemit her: https://eprom.hemit.org/Integrasjonsguide, og er pr tiden:
#   0 = None
# 1 = Helsenorge
# 2 = DigitalMailbox
# 3 = Unsecure
# 4 = PhysicalMailbox
# EpromUtsendRegel er definert på samme plass som den forrige:
#   0 = Basic
# 1 = AllowUnsecure
# 2 = NoDistribution
# 3 = BasicOrPaper
# 4 = AllowUnsecureOrPaper
# 5 = PaperOnly
# 6 = HelsenorgeOnly
# 7 = DigitalMailboxOnly
# 8 = UnsecureOnly

# #EpromStatus er definert av oss, og den som er viktigst med tanke på svarprosent.
# #Det er altså verdi 3 her som betyr at pasienten har besvart.
#OBS at den skiller seg litt fra tilsvarende variabel i Hemit-definisjonen. Denne er for deg og oss definert slik:
#   0 = Created
# 1 = Ordered
# 2 = Expired
# 3 = Completed
# 4 = Failed

# EpromFeilKode er definert av Hemit som de to litt lenger opp. En vanlig kombo er EpromStatus=4 og EpromFeilKode=1. Dette er digitalt ikke-nåbare pasienter:
#   0 = None
# 1 = PatientUnreachable
# 2 = SikkerDigitalPostError
# EpromOppdatert er tidspunkt for siste oppdatering av denne recorden i proms-tabellen.



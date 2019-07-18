#' Beregn p-verdi, justert for multippeltesting, ved sammenligning av to og to andeler
#'
#' Denne funksjonen benytter prop.test for å beregne p-verdi for at to andeler er like
#' og justerer p-verdiene ved p.adjust
#'
#' @param n - Antall tellende hendelser, skalar eller vektor
#' @param N - Antall observasjoner, skalar eller vektor av samme lengde som n
#' @param konfnivaa - Konfidensnivået til test (standard 0.95)
#' @param justMetode - justeringsmetode, standard: 'fdr'. Mulige valg:
#'                     "holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none"
#'
#' @return Justerte p-verdier for test av endring i andeler
#' @export
PverdiAndelsDiff <- function(n, N, konfnivaa=0.95, justMetode='fdr')
{

   #Litt innledende utforsking:
       # n <- matrix(rep(5,6),ncol = 2, nrow=3)
       # N <- matrix(c(20,14,35,90,72,81), ncol = 2, nrow=3)
       # n/N
      #smokers  <- c( 83, 90, 129, 70 )
      #patients <- c( 86, 93, 136, 82 )
      #prop.test(smokers, patients)
      #n <- t(Nvar[ ,-indGrUt2])
      #N <- t(Ngr[ ,-indGrUt2])
      options(warn=-1)
      pVerdi <- NA
      for (k in 1:dim(N)[1]){
            if (min(N[k,]>0)) {
                  pVerdi[k] <- prop.test(n[k,],N[k,])$p.value
            } else {
                  pVerdi[k] <- NA
            }
      }
      pVerdierJust <- p.adjust(p=pVerdi, method = justMetode) #
      #p.adjust.methods
      # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",  "fdr", "none")

      #a <- prop.test(c(34,84), c(50,93))      #c(32,15),c(597,559))
      #p.adjust(p=c(0.04, a$p.value, 0.2), method = 'fdr')
#
# #Binomial test med N-tilnærming
# p <- (n[1] * n[1]/N[1] + n[2] * n[2]/N[2])/ (n[1] + n[2]) # np>5 and n(1−p)>5, for p1 og p2
# z <- (n[1]/N[1] - n[2]/N[2]) / sqrt(p * (1-p) * (1/N[1]+ 1/N[2]))
# 2*pnorm(z)
# #(z <- (p1 - p2) / sqrt(p * (1-p) * (1/n1 + 1/n2)))
# #Gyldig hvis:
# n[1] * n[1]/N[1]>5 & n[2]*n[2]/N[2]>5 & n[1]*(1- n[1]/N[1])>5 & n[2]*(1- n[2]/N[2])>5

#fisher.test(x=cbind(n,N), simulate.p.value = TRUE, B = 10000) #$p.value
#Betinger at rad og kolonne marginaler er konstant...(?) Ellers får vi mer konservative p-verdier



return(invisible(pVerdierJust))
}


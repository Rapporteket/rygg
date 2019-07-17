# rygg

<!-- badges: start -->
<!-- badges: end -->

The goal of rygg is to ...

## Installasjon
Pakken kan lastes direkte i din R-studio sesjon på følgende måte:


``` r
install.packages("devtools")
devtools::install_github("Rapporteket/rapbase")

```
Hvis du jobber gjennom proxy, kan kommandoen over feile. I så fall er det være nødvendig 
å kjøre følgende før installasjonen:

``` r
library(httr)
  set_config(
    use_proxy(url="18.91.12.23", port=8080, username="user",password="passwd")
  )
# ELLER:
Sys.setenv(http_proxy="www-proxy.helsenord.no:8080")
Sys.setenv(https_proxy="www-proxy.helsenord.no:8080")
```
NB: Husk å bytte ut de oppgitte parametrene med de som er aktuelle for systemet de installeres på.

Når pakken installeres på Rapporteket, pass på at opprydning utføres:

``` r
  devtools::install_github("Rapporteket/rapbase", args=c("--clean"))
``` 

Dette legger også til lokal konfigurasjon etter at pakken er installert.


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rygg)
## basic example code
```


# ------ Initialisation --------------------------------------------------------

# ------Load packages ----------------------------------------------------------
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(RPostgreSQL)
library(RPostgres)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ade4)
library(shinyjs)
library(ssh)
library(httr)
library(jsonlite)
library(future)
library(promises)
library(ipc)
library(shinyalert)
library(shinycssloaders)
library(data.table)

#-----Lists---------------------------------------------------------------------
listSugar <- list("arabinose","cellobiose","fructose","fucose","galactose","glucose","inositol",
                  "isomaltose","maltose","mannose","mannose_xylose","melbiose","melezitose","palatinose",
                  "raffinose","rhamnose","ribose","sucrose","trehalose","xylose")
listHormones <- list("ABA","IAA","JA","SA","CMLX")
listGlucosinolates<- list("x3mtp","x5mtp","x6msh","x7msh","x7mth","x8mso","x8mto","butyl",
                          "epigallocatechin","epiprogoitrin","glucoalysiin","glucobrassicin","glucoerucin","gluconapin",
                          "gluconasturtiin","glucoraphanin","glucoraphenin","glucosinalbin","hexyl","isobutyl",
                          "neoglucobrassicinpeak2","neoglucobrassicinpeak1","progoitrin","sinigrin")
listSecondaryMetabolites<- list("apigeninRutinoside","caffeicAcid","chlorogenicAcid","citrat",
                                "cyanidinRhamnoside","cyanidinSophorosidGlucoside","dihydroCaffeoylGlucuronide","fumarat",
                                "kaempherolGlucosylRhamnosylglucoside","kaempherolRutinoside","kaempherolXylosylRhamnoside","malat",
                                "mCoumaricAcid","pelargonidinCumaroylDiglucoside","pelargonidinSambubioside","prenylNaringenin",
                                "quercetinGlucoside","succinat")
listFunctionalTraits <- list("LDMC","SLA","LNC","Thickness","RWC","LCC","Delta13C","Delta15N","Plant_lifespan","Plant_growth_rate",
                             "Csr_c","Csr_s","Csr_r")

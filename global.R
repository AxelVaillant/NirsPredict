options(shiny.port = 8080) # Necessary for auth0 callback URLs

# ------ Initialisation --------------------------------------------------------

auth0_info <- auth0::auth0_info()

# ------Load packages ----------------------------------------------------------

library(shiny)
library(RPostgreSQL)
library(RPostgres)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ade4)
library(auth0)
library(sendmailR)
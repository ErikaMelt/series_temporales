################################ LOAD LIBRARIES ################################

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(plotly)
library(ggplot2)
library(forecast)
library(DT)

#Voy a leer los datos justo aquí porque resulta más práctico y sólo se leería una vez
#Eso sí obliga a que el dataset de denomine LOGIT.csv

df <- fread(paste0("data/SERIE.csv"))

################################# LOAD MODULES #################################

my_modules <- list.files("tabs", pattern = "tab_module.R", full.names = TRUE,
                         recursive = TRUE)

for(my_module in my_modules) source(my_module)


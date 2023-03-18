################################ LOAD LIBRARIES ################################

library(shiny)
library(shinydashboard)

library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(earth)



################################# LOAD MODULES #################################

my_modules <- list.files("tabs", pattern = "tab_module.R", full.names = TRUE,
                         recursive = TRUE)

for(my_module in my_modules) source(my_module)


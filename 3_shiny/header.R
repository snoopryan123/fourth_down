
library(shiny)
library(shinycssloaders)
library(shinycustomloader)
library(shinydashboard)
library(gt)

############################
### LOAD DATA AND MODELS ###
############################

#FIXME
### 4th down decision making directory
shiny_wd = getwd()
source("../0_clean_lm.R")
setwd("../4_Decision_Making")
### LOAD 4TH DOWN PLOTTING FUNCTIONS
source("decision_making_bootstrap_main.R")
source("decision_making_loadModels.R")
source("decision_making_functions.R")
### LOAD MLR AND XGB MODEL FUNCTIONS
setwd("../3_model_selection/mlr_110")
source("models.R")
setwd("../xgb_110")
source("models.R")
### set wd back
setwd(shiny_wd)

#############

map_qualitativeButton_to_stdSlider <- function(btn) {
  case_when(
    btn == "exceptional" ~ 1,
    btn == "above average" ~ 1/3,
    btn == "average" ~ 0,
    btn == "below average" ~ -1/3,
    btn == "horrible" ~ -1,
    TRUE ~ 0
  )
}

std_var_min = -1
std_var_max = 1
std_var_step = 0.1
# loading_color = "#0dc5c1"
loading_color = "#c5910d"
# loading_color = "#004953"







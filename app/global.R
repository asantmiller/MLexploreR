###     Global environment for the application     ###
### evaluatoR machine learning analytics platform  ###
### Author: Aaron Sant-Miller, Booz Allen Hamilton ###

# Package loading - list all needed packages here -------------------------
package_list <- c("tidyverse", "tools", "janitor", "lubridate", "caret", "shiny",
                  "shinydashboard", "shinythemes", "reshape2", "DT", "plotly", "RColorBrewer",
                  "DBI", "viridis", "stringr", "igraph", "topicmodels", "visNetwork",
                  "relaxo", "elasticnet", "arm", "mboost", "mgcv", "h2o", "earth", 
                  "nnet", "pls", "randomForest", "geosphere", "monomvn", "arm")


# Source function base and modules to be handled in application  ----------
# Function base
source("/Users/aaronsantmiller/Desktop/dev/evaluatoR/app/src/function_base.R")

# Modules
source("/Users/aaronsantmiller/Desktop/dev/evaluatoR/app/R/data_import.R")
source("/Users/aaronsantmiller/Desktop/dev/evaluatoR/app/R/feature_analysis.R")
source("/Users/aaronsantmiller/Desktop/dev/evaluatoR/app/R/model_performance.R")


# Establish protected name space in the global environment ----------------
DATA_IMPORT<- "data_import"
FEATURE_ANALYSIS <- "feature_analysis"
MODEL_PERFORMANCE <- "model_performance"


# Define global parameters for application stability ----------------------
set.seed(10)
install_packages(package_vector = package_list)

# Work around
import_df <- load_big_tibble(
  path_to_file = "/Users/aaronsantmiller/Desktop/dev/evaluatoR/data/covtype.csv")



###     Global environment for the application     ###
### MLexploreR machine learning analytics platform ###
### Author: Aaron Sant-Miller, Booz Allen Hamilton ###

# Package loading - list all needed packages here -------------------------
package_list <- c("tidyverse", "tools", "janitor", "lubridate", "caret", "shiny",
                  "shinydashboard", "shinythemes", "reshape2", "DT", "plotly", "RColorBrewer",
                  "DBI", "viridis", "stringr", "igraph", "topicmodels", "visNetwork",
                  "relaxo", "elasticnet", "arm", "mboost", "mgcv", "h2o", "earth", 
                  "nnet", "pls", "randomForest", "geosphere", "monomvn", "arm",
                  "shinycssloaders", "vcd", "RCurl", "readxl")


# Source function base and modules to be handled in application  ----------
# Function base
source("/Users/aaronsantmiller/Desktop/dev/MLexploreR/app/src/function_base.R")

# Modules
source("/Users/aaronsantmiller/Desktop/dev/MLexploreR/app/R/data_import.R")
source("/Users/aaronsantmiller/Desktop/dev/MLexploreR/app/R/feature_analysis.R")
source("/Users/aaronsantmiller/Desktop/dev/MLexploreR/app/R/model_performance.R")


# Establish protected name space in the global environment ----------------
DATA_IMPORT<- "data_import"
FEATURE_ANALYSIS <- "feature_analysis"
MODEL_PERFORMANCE <- "model_performance"


# Define global parameters for application stability ----------------------
set.seed(10)
install_packages(package_vector = package_list)

high_colors <- round(seq(255, 40, length.out = 13), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
low_colors <- round(seq(40, 255, length.out = 12), 0) %>% {paste0("rgb(", .,",", ., ",", "255)")}
hot_cold_colors <- c(low_colors, high_colors)


# Preload datasets --------------------------------------------------------
# Load red wine
red <- getURL("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv") %>%
  textConnection() %>%
  read.csv(file = ., header = TRUE, sep = ";") %>%
  mutate(quality = as.factor(quality)) %>%
  as_tibble()

# Load white wine
white <- getURL("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv") %>%
  textConnection() %>%
  read.csv(file = ., header = TRUE, sep = ";") %>%
  mutate(quality = as.factor(quality)) %>%
  as_tibble()

import_df <- rbind.data.frame(red, white) %>%
  as_tibble() %>%
  janitor::clean_names()

table_choices <- names(import_df)


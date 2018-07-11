### Machine Learning / Data Processing Function Base ###
###  Author: Aaron Sant-Miller, Booz Allen Hamilton  ###

# TODO: Add comments and parameter callouts

# Helper functions --------------------------------------------------------
install_packages <- function(package_vector) {
  # This function helps install packages in a loop to ensure the user
  # isn't missing critical dependencies
  
  new_packages <- package_vector[!(package_vector %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
  
  sapply(package_vector, require, character.only = TRUE)
}

# Data loading and preprocessing functions --------------------------------
load_big_tibble <- function(path_to_file) {
  # This function reads in the full data frame, simplifies the names, and normalizes feature types
  # NOTE: No preprocessing integrated at this point
  
  df <- read_csv(path_to_file) %>%
    janitor::clean_names()
  
  # Detect strings
  characters <- sapply(df, typeof) %>%
    grep(pattern = "character", x = .)
  
  # Rare numerics - workaround for encoding on Kaggle
  uniques <- sapply(df, function(feature) {
    unique(feature) %>%
      length()
  })
  kaggle_categoricals <- names(df)[which(uniques < 10)]
  
  # Turn strings into factors
  df %>%
    mutate_at(.vars = characters, .funs = as.factor) %>%
    mutate_at(.vars = kaggle_categoricals, .funs = as.factor)
}

filter_missing_outcomes <- function(df, outcome_feature) {
  # This function takes the full data frame and filters out rows with a missing outcome value
  # NOTE: User should check frequencies of NA before using this function blindly
  
  df %>%
    filter_(!is.na(outcome_feature))
}



# Generate plots for the user interface -----------------------------------
generate_feature_viz <- function(df, selected_feature) {
  if (is.numeric(x = df[[selected_feature]])) {
    gg <- ggplot(data = df) +
      geom_density(aes_string(x = selected_feature), 
                   color = "navyblue",
                   fill = "lightblue") +
      ggtitle("Feature Visualization for Inspection")
    
    ggplotly(gg)
    
  } else if (is.factor(df[[selected_feature]])) {
    gg <- ggplot(data = df) +
      geom_bar(aes_string(x = selected_feature), 
               color = "navyblue", 
               fill = "lightblue") +
      ggtitle("Feature Vizualization for Inspection")
    
    ggplotly(gg)
    
  } else if (is.Date(df[[selected_feature]])) {
    gg <- ggplot(data = df) +
      geom_freqpoly(aes_string(x = selected_feature), 
               color = "navyblue") +
      ggtitle("Feature Vizualization for Inspection")
    
    ggplotly(gg)
    
  } else {
    stop("Please select either a catagorical feature or a numeric feature")
  }
}

# Generate feature analysis tables
generate_aesthetic_summary <- function(vector) {
  vector %>%
    summary() %>%
    as.matrix() %>%
    t() %>%
    as.data.frame %>%
    mutate_all(.funs = round, digits = 2) %>%
    DT::datatable(.,
                  rownames = FALSE,
                  escape = FALSE,
                  class = 'compact cell-border stripe hover',
                  extensions = 'Buttons',
                  options = list(dom = 'BT<"clear">ltirp',
                                 paging = FALSE,
                                 searching = FALSE,
                                 buttons = list('copy'))) 
}

generate_feature_table <- function(df, features_to_display) {
  df %>%
    select_(., .dots = features_to_display) %>%
    DT::datatable(.,
                  rownames = FALSE,
                  escape = FALSE,
                  filter = 'top',
                  class = 'compact cell-border stripe hover',
                  extensions = 'Buttons',
                  options = list(dom = 'BT<"clear">ltirp',
                                 buttons = list('copy', 
                                                list(extend = 'csv', filename = "feature_table"), 
                                                list(extend = 'excel', filename = "feature_table")),
                                 pageLength = 20)) 
}

generate_feature_analysis <- function(df, selected_features, analysis_choice) {
  if (analysis_choice == "Zero-Variance Detection") {
    df[, selected_features] %>%
      nearZeroVar(., saveMetrics = TRUE) %>%
      as.data.frame %>%
      build_simple_dt_output()
    
  } else if (analysis_choice == "Correlation Analysis") {
    tryCatch({
      df[, selected_features] %>%
        cor() %>% 
        as.data.frame(x = ., row.names = TRUE) %>%
        build_simple_dt_output(dataframe = ., rn = TRUE) %>%
        formatStyle(table = ., columns = selected_features,
                    backgroundColor = styleInterval(seq(from = -1, to = 1, length.out = 24), 
                                                    hot_cold_colors))
    }, error = function(cond) {
      stop("Correlation analysis restricted to numeric features. Ensure all selected features are numeric!")
    })
    
  } else if (analysis_choice == "Association Matrix") {
    tryCatch({
      empty_matrix <- matrix(ncol = length(selected_features),
                             nrow = length(selected_features),
                             dimnames = list(selected_features, 
                                             selected_features))
      calculate_cramer(empty_matrix, df[, selected_features]) %>%
        as.data.frame(x = ., row.names = TRUE) %>%
        build_simple_dt_output(dataframe = ., rn = TRUE) %>%
        formatStyle(table = ., columns = selected_features,
                    backgroundColor = styleInterval(seq(from = 0, to = 1, length.out = 12),
                                                    high_colors))
    }, error = function(cond) {
      stop("Association matrices are restricted to categorical features. Ensure all selected features are factors!")
    })
  } else {
    NULL
  }
  
}

calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

build_simple_dt_output <- function(dataframe, rn = FALSE) {
  vars_to_round <- sapply(dataframe, is.numeric) %>%
    which() 
  
  dataframe %>%
    mutate_at(.vars = vars_to_round, .funs = round, digits = 2) %>%
    DT::datatable(.,
                  rownames = rn,
                  escape = FALSE,
                  class = 'compact cell-border stripe hover',
                  extensions = 'Buttons',
                  options = list(dom = 'BT<"clear">ltirp',
                                 paging = FALSE,
                                 searching = FALSE,
                                 buttons = list('copy')))
}

# Build training and testing data for evaluation --------------------------
create_holdout <- function(df, split_approach = "balanced", target_variable) {
  # This function takes the full data frame and shards off a hold out set for rigorous test
  # The user can pass either "random", "balanced", or "time" as a parameter to the function to either randomly
  # select the holdout set or keep 2011-2015 as train-test and 2016-2017 as holdout
  
  if (split_approach == "balanced") {
    if (typeof(df[[target_variable]]) == "double") {
      train_index <- createDataPartition(y = df[[target_variable]], 
                                         p = 0.75, 
                                         times = 1,
                                         list = FALSE,
                                         groups = 100)
    } else {
      train_index <- createDataPartition(y = df[[target_variable]], 
                                         p = 0.75, 
                                         times = 1,
                                         list = FALSE)
    }
    
    list("build" = df[train_index, ], 
         "holdout" = df[-train_index, ])
  } else if (split_approach == "random") {
    train_index <- sample(x = 1:nrow(df), 
                          size = round(0.75 * nrow(df)), 
                          replace = FALSE)
    
    list("build" = df[train_index, ], 
         "holdout" = df[-train_index, ])
  } else {
    stop("Please define split_approach as either random or balanced!")
  }
}


# Model training pipeline - wrapper for caret -----------------------------
test_modeling_approaches <- function(df, outcome, predictors, training_method,
                                     k, model_vector, parallel = FALSE) {
  # This function takes the train test data, a defined model vector to test, and training characteristics, and
  # adefined features. It trains and tests the various models and returns a named list of performance metrics 
  # NOTE: This function should be used to evaluate various models, NOT train a production model
  
  # TODO: Parallelize over detected cores and CPUs
  
  ml_formula <- paste(predictors, collapse = " + ") %>%
    paste(outcome, " ~ ", .) %>%
    as.formula()
  
  training_control <- trainControl(method = training_method,
                                   number = k)
  
  performance_list <- lapply(model_vector, function(model) {
    evaluate_ml_model(df = df, 
                      control_map = training_control, 
                      model_formula = ml_formula, 
                      model_selection = model)
  })
  
  names(performance_list) <- model_vector
  
  return(performance_list)
  
}

evaluate_ml_model <- function(df, control_map, model_formula, model_selection) {
  # This function takes the train test data, a selected model, a defined training scheme, and
  # a defined model formula. It trains the selected model per directive, and returns the performance 
  # of that model. 
  # NOTE: This function should be used to evaluate a specific model, NOT train a production model
  
  cat(paste0("Training ", model_selection, " model...\n"))
  train(model_formula, 
        data = df, 
        method = model_selection,
        trControl = control_map)
}



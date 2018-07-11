### Model Performance Module: Server, Elements, and UI for MLexploreR  ###
###          Author: Aaron Sant-Miller, Booz Allen Hamilton            ###

# User Interface ----------------------------------------------------------
model_performance_ui <- function(id) {
  ns <- NS(id)
  ui_elements <- model_performance_elements(id)
  
  div(
    h3("Configure Training Regimen and Evaluate Model"),
    tags$i("Define the intended framework and mechanism for model training and select model to train. 
            The performance measures may take time to compute if your data is large."),
    br(), 
    tags$b("NOTE: No outputs from this tool should be used in implementation. This is purely exploratory analysis."),
    br(),
    br(),
    fluidRow(
      sidebarPanel(
        title = "Define machine learning approach:",
        width = 4,
        fluidRow(
          ui_elements["ml_text"],
          box(
            width = 12,
            style = 'padding: 10px;',
            ui_elements[["predictors"]],
            ui_elements[["outcome"]],
            ui_elements[["model_selection"]],
            ui_elements[["train_method"]],
            ui_elements[["error_metric"]],
            ui_elements[["preprocessing"]],
            ui_elements[["parallel"]],
            div(
              width = 12,
              style = 'padding-right: 17px;',
              align = "right",
              ui_elements[["train_model"]]
            )
          )
        ),
        style = 'padding: 10px;'
      ),
      mainPanel(
        width = 8,
        fluidRow(
          box(
            width = 12,
            style = 'padding: 10px;',
            ui_elements[["ml_output"]]
          )
        )
      )
    )
  )
}

# Server Code -------------------------------------------------------------
model_performance_server <- function(input, output, session) {
 output[["ml_performance"]] <- renderText({
   if (input[["run_training"]]) {
     df <- import_df[1:5000, ]
     
     ml_formula <- paste(input$predictors, collapse = " + ") %>%
       paste(input$outcome, " ~ ", .) %>%
       as.formula()
     
     preprocessed <- df[, input$predictors] %>%
       preProcess(.,
                  method = input$preproc) %>%
       predict(., newdata = df[, input$predictors])
     
     training_control <- trainControl(method = input$approach,
                                      allowParallel = input$parallel)
     
     num_cores <- ifelse(test = input$parallel, 
                         yes = detectCores() - 2,
                         no = 1)
     
     train(ml_formula, 
           data = cbind.data.frame(preprocessed, df[, input$outcome]),
           method = input$model, 
           trControl = training_control, 
           verbose = FALSE, 
           metric = input$metric,
           workers = num_cores)
   } else {
     print("Configure approach...")
   }
   
 })
}


# Defined Elements --------------------------------------------------------
model_performance_elements <- function(id) {
  ns <- NS(id)
  ui_elements <- tagList()
  
  # Configuration tab
  ui_elements[["ml_text"]] <- box(
    width = 12,
    style = 'padding: 10px',
    tags$b("Header 1"),
    tags$br(),
    tags$p("Main text here talking about why this is important"),
    tags$br(),
    tags$b("Method one"),
    tags$em("Explain"),
    tags$br(),
    tags$b("Method two"),
    tags$br("Explain"),
    tags$br(),
    tags$br(),
    tags$hr(),
    tags$b("LIMITATIONS:"),
    tags$br(),
    tags$p("Include caveats here")
  )
  ui_elements[["train_method"]] <- selectizeInput(inputId = ns("approach"),
                                                  label = "Select method for training:",
                                                  multiple = FALSE,
                                                  choices = list("select a method to implement" = NULL,
                                                                 "Cross-validation" = "cv", 
                                                                 "Repeated Cross-validation" = "repeatedcv",
                                                                 "Bootstrapping" = "boot",
                                                                 "Leave-one-out-cross-validation" = "loocv"),
                                                     selected = "select a method to implement")
  ui_elements[["model_selection"]] <- selectizeInput(inputId = ns("model"),
                                                     label = "Select model for evaluation:",
                                                     multiple = FALSE,
                                                     choices = list("GLM" = "glm", 
                                                                    "Random Forest" = "rf", 
                                                                    "Multivairate Adaptive Spline" = "earth", 
                                                                    "Generalized Additive Model" = "gam", 
                                                                    "Naive Bayes" = "nb", 
                                                                    "Neural Network" = "nnet",
                                                                    "Ridge Regression" = "ridge",
                                                                    "Suport Vector Machine" = "svmRadial",
                                                                    "AdaBoost" = "ada",
                                                                    "Lasso" = "lasso",
                                                                    "Bayesian GLM" = "bayeesglm",
                                                                    "Boosted GLM" = "glmboost", 
                                                                    "Extreme Gradient Boosting" = "xgbTreeß"),
                                                     selected = NULL)
  ui_elements[["error_metric"]] <- selectizeInput(inputId = ns("metric"),
                                                  label = "Select error metric for evaluation:",
                                                  multiple = FALSE,
                                                  choices = c("RMSE", "MAE", "Accuracy", "Kappa"),
                                                  selected = NULL)
  ui_elements[["parallel"]] <- selectizeInput(inputId = ns("parallel"),
                                               label = "Allow parallel processing:",
                                                multiple = FALSE,
                                                choices = list("select choice on distribution" = NULL,
                                                                "Yes" = TRUE, 
                                                                "No" = FALSE),
                                                  selected = "select choice on distribution")
  ui_elements[["preprocessing"]] <- selectizeInput(inputId = ns("preproc"),
                                                   label = "Select preprocessing methods:",
                                                   multiple = TRUE,
                                                   choices = c("center", "scale", "YeoJohnson", 
                                                               "nzv", "BoxCox", "pca"),
                                                   selected = NULL)
  
  ui_elements[["predictors"]] <- selectizeInput(inputId = ns("predictors"),
                                                label = "Select predictors for model:",
                                                multiple = TRUE,
                                                choices = names(import_df),
                                                selected = NULL)
  ui_elements[["outcome"]] <- selectizeInput(inputId = ns("outcome"),
                                                label = "Select target variable for model:",
                                                multiple = FALSE,
                                                choices = names(import_df),
                                                selected = NULL)
  
  ui_elements[["train_model"]] <- actionButton(inputId = ns("run_training"),
                                               label = "Train Model",
                                               icon("trophy"),
                                               style = "color: #fff; background-color: #337ab7;
                                                        border-color: #2e6da4")
  
  ui_elements[["ml_output"]] <- textOutput(ns("ml_performance")) %>%
    withSpinner()
  
  ui_elements
  
}

### Data Import Module: Server, Elements, and UI for MLexploreR  ###
###       Author: Aaron Sant-Miller, Booz Allen Hamilton         ###

# TODO: Need to functionalize the handoff between the UI file upload and the server
      # Note, this will need to be visible throughout the application - may need to move to main ui and server
# TODO: Add the submit button and loading spinners
# TODO: Add something visual below the main feature visualization
# TODO: Find a way to more generalizably handle cateogrical features
# TODO: Find a way to better visualize dates
# TODO: Add comments and parameter callouts

# User Interface ----------------------------------------------------------
data_import_ui <- function(id) {
  ns <- NS(id)
  ui_elements <- data_import_elements(id)
  div(
    h3("Import Data and Explore Features"),
    tags$i("Currently preloaded into the tool is a data set on wine characteristics and their scored quality."),
    br(),
    tags$b("NOTE: This can be changed in the global.R of the application or 
           changed by allowing the user to upload data"),
    br(), 
    sidebarPanel(
      title = "Explore data:",
      width = 4,
      fluidRow(
        column(
          width = 6,
          style = 'padding: 20px;',
          ui_elements[["row_box"]]
        ),
        column(
          width = 6,
          style = 'padding: 20px;',
          ui_elements[["column_box"]]
        )
      ),
      fluidRow(
        div(
          style = 'padding: 20px',
          ui_elements[["feature_selection"]]
        )
      ),
      fluidRow(
        column(
          width = 6,
          style = 'padding: 20px;',
          ui_elements[["feature_type"]]
        ),
        column(
          width = 6,
          style = 'padding: 20px;',
          ui_elements[["missing_values"]]
        )
      ),
      style = 'padding: 10px;'
    ), 
    # Right hand column for data visualizations of uploaded data and summary measures
    mainPanel(
      width = 8,
      fluidRow(
        box(
          width = 12,
          style = 'padding: 10px;',
          ui_elements[["viz"]]
        )
      ),
      fluidRow(
        box(
          width = 12,
          style = 'padding: 10px',
          ui_elements[["feature_summary"]]
        )
      )
    )
  )
}


# Server Code -------------------------------------------------------------
data_import_server <- function(input, output, session) {
  output[["row_box"]] <- renderValueBox({
    valueBox(
      subtitle = "observations in dataset", 
      color = "navy",
      value = nrow(x = import_df),
      icon = icon("arrow-down")
    )
  })
  
  output[["column_box"]] <- renderValueBox({
    valueBox(
      subtitle = "features in dataset",
      color = "light-blue",
      value = ncol(x = import_df),
      icon = icon("arrow-right")
    )
  })
  
  output[["feat_sum"]] <- DT::renderDataTable({
    import_df[[input$selected_feature]] %>%
      generate_aesthetic_summary(vector = .)
  })
  
  output[["viz"]] <- renderPlotly(
    generate_feature_viz(df = import_df, selected_feature = input$selected_feature)
  )
  
  output[["feature_type"]] <- renderValueBox({
    valueBox(
      subtitle = "feature type",
      color = "navy",
      value = class(import_df[[input$selected_feature]]),
      icon = icon("shopping-bag")
    )
  })
  
  output[["missing_values"]] <- renderValueBox({
    n <- which(is.na(input$selected_feature)) %>%
      length()
    
    valueBox(
      subtitle = "missing values in feature", 
      color = "light-blue",
      value = n,
      icon = icon("low-vision")
    )
  })
}


# Defined Elements --------------------------------------------------------
data_import_elements <- function(id) {
  ns <- NS(id)
  ui_elements <- tagList()
  
  ui_elements[["data_import"]] <- selectizeInput(inputId = ns("chosen_data"),
                                                 label = "Pick a dataset:",
                                                 choices = list(
                                                   "Building Efficiency - Regression" = "building_efficiency",
                                                   "Car Acceptability - Classification" = "car_acceptable"),
                                                 selected = NULL,
                                                 multiple = FALSE)
  
  ui_elements[["column_box"]] <- valueBoxOutput(outputId = ns("column_box"),
                                                width = "100%")
  
  ui_elements[["row_box"]] <- infoBoxOutput(outputId = ns("row_box"), 
                                            width = "100%")
  
  ui_elements[["feature_selection"]] <- selectizeInput(inputId = ns("selected_feature"),
                                                       label = "Select a feature:", 
                                                       choices = names(import_df),
                                                       selected = NULL,
                                                       multiple = FALSE)
  
  ui_elements[["feature_type"]] <- valueBoxOutput(outputId = ns("feature_type"),
                                                width = "100%")
  
  ui_elements[["missing_values"]] <- infoBoxOutput(outputId = ns("missing_values"), 
                                            width = "100%")
  
  ui_elements[["feature_summary"]] <- DT::dataTableOutput(outputId = ns("feat_sum")) %>%
    withSpinner()
  
  ui_elements[["viz"]] <- plotlyOutput(outputId = ns("viz")) %>%
    withSpinner()
  
  ui_elements
  
}

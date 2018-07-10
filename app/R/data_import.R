### Data Import Module: Server, Elements, and UI for evalautoR  ###
###       Author: Aaron Sant-Miller, Booz Allen Hamilton        ###

# User Interface ----------------------------------------------------------
data_import_ui <- function(id) {
  ns <- NS(id)
  ui_elements <- data_import_elements(id)
  div(
    h3("Import Data and Explore Features"),
    tags$i("Choose a local CSV file to upload and explore features of interest. 
           Please limit your selection to a structured dataset with numerical and categorical features only."),
    br(), 
    tags$b("NOTE: Larger files may take some time to upload. Thank you for your patience."),
    br(),
    br(), 
    sidebarPanel(
      title = "Import data:",
      width = 4,
      fluidRow(
        # File selection element
        box(
          width = 12,
          style = 'padding: 10px',
          ui_elements[["data_import"]]
        )
      ),
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
        box(
          width = 12,
          style = 'padding: 10px',
          ui_elements[["feature_summary"]]
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
      )
    )
  )
}


# Server Code -------------------------------------------------------------
data_import_server <- function(input, output, session) {
  # import_df <- reactive({
  #   infile <- input$imported_data
  # 
  #   print(str(infile))
  # 
  #   if (is.null(infile)) {
  #     return(NULL)
  #   } else {
  #     df <- read_csv(infile$datapath)
  #     return(df)
  #   }
  # })
  
  # import_df <- read_csv("data/globalterrorismdb_0617dist.csv")
  
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
  
  # output[["feature_names"]] <- reactive({
  #   names(import_df)
  # })
  
  output[["feat_sum"]] <- renderPrint({
    import_df[[input$selected_feature]] %>%
      summary()
  })
  
  output[["viz"]] <- renderPlotly(
    generate_feature_viz(df = import_df, selected_feature = input$selected_feature)
  )
}


# Defined Elements --------------------------------------------------------
data_import_elements <- function(id) {
  ns <- NS(id)
  ui_elements <- tagList()
  
  ui_elements[["data_import"]] <- fileInput(inputId = ns("imported_data"),
                                            label = "Choose a CSV file to import:",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv"))
  
  # ui_elements[["import_button"]] <- actionButton(inputId = "import_data",
  #                                                label = "Import File",
  #                                                icon("paper-plane"), 
  #                                                style = "color: #fff; background-color: #337ab7; 
  #                                                         border-color: #2e6da4")
  
  ui_elements[["column_box"]] <- valueBoxOutput(outputId = ns("column_box"),
                                                width = "100%")
  
  ui_elements[["row_box"]] <- infoBoxOutput(outputId = ns("row_box"), 
                                            width = "100%")
  
  ui_elements[["feature_selection"]] <- selectizeInput(inputId = ns("selected_feature"),
                                                       label = "Select a feature:", 
                                                       choices = names(import_df),
                                                       selected = NULL,
                                                       multiple = FALSE)
  
  ui_elements[["feature_summary"]] <- verbatimTextOutput(outputId = ns("feat_sum"))
  
  ui_elements[["viz"]] <- plotlyOutput(outputId = ns("viz"))
  
  ui_elements
  
}

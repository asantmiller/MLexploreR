### Feature Analysis Module: Server, Elements, and UI for MLexploreR  ###
###         Author: Aaron Sant-Miller, Booz Allen Hamilton            ###

# TODO: Add more analysis in - comparative distribution testing, etc.
# TODO: Add text about various feature analysis testing
# TODO: Find efficient feature selection mechanism and use that for the third tab, caret functions are slow


# User Interface ----------------------------------------------------------
feature_analysis_ui <- function(id) {
  ns <- NS(id)
  ui_elements <- feature_analysis_elements(id)
  div(
    h3("Import Data and Explore Features"),
    tags$i("Explore the features more deeply of your imported dataset. 
           The tests in this page are not comprehensive, but can help gain preliminary insight into the data."),
    br(), 
    tags$b("NOTE: It is recommended that deeper ad hoc feature analysis is undertaken after exploration here."),
    br(),
    br(), 
    fluidRow(
      box(
        width = 12,
        style = 'padding: 10px;',
        ui_elements[["selected_features"]] 
      )
    ),
    fluidRow(
      tabBox(
        width = 12,
        tabPanel(
          title = "Explore Features: Table",
          style = 'padding: 5px;',
          fluidRow(
            style = 'padding: 10px;',
            ui_elements[["main_feature_table"]]
          )
        ),
        tabPanel(
          title = "Analyze Features: Correlations and Associations",
          style = 'padding: 5px;',
          fluidRow(
            sidebarPanel(
              title = "Run Feature Analysis:",
              width = 4,
              fluidRow(
                ui_elements["summary_text"],
                box(
                  width = 12,
                  style = 'padding: 10px;',
                  ui_elements[["analysis_to_run"]],
                  div(
                    width = 12,
                    style = 'padding-right: 17px;',
                    align = "right",
                    ui_elements[["run_comparison"]]
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
                  ui_elements[["analysis_output"]]
                )
              )
            )
          )
        )
        # tabPanel(
        #   title = "Select Features: Recursive Feature Elimination",
        #   style = 'padding: 5px;',
        #   fluidRow(
        #     sidebarPanel(
        #       title = "Define RFE Approach:",
        #       width = 4,
        #       fluidRow(
        #         ui_elements["rfe_text"],
        #         box(
        #           width = 12,
        #           style = 'padding: 10px;',
        #           ui_elements[["select_approach_1"]],
        #           ui_elements[["select_approach_2"]],
        #           ui_elements[["select_approach_3"]],
        #           div(
        #             width = 12,
        #             style = 'padding-right: 17px;',
        #             align = "right",
        #             ui_elements[["run_rfe"]]
        #           )
        #         )
        #       ),
        #       style = 'padding: 10px;'
        #     ), 
        #     mainPanel(
        #       width = 8,
        #       fluidRow(
        #         box(
        #           width = 12,
        #           style = 'padding: 10px;',
        #           ui_elements[["rfe_output"]]
        #         )
        #       )
        #     )
        #   )
        # )
      )
    )
  )
}


# Server Code -------------------------------------------------------------
feature_analysis_server <- function(input, output, session) {
  output[["main_feature_table"]] <- DT::renderDataTable({
    generate_feature_table(df = import_df, features_to_display = input[["features"]])
  })
  
  # output[["feature_analysis"]] <- eventReactive({ input[["run_comparison"]] }, {
  #   req(isolate(input[["run_comparison"]]))
  #   
  #   generate_feature_analysis(df = import_df, 
  #                             selected_features = input[["features"]], 
  #                             analysis_choice = input[["analysis_selection"]])
  # })
  
  output[["feature_analysis"]] <- DT::renderDataTable({
    input[["run_comparison"]]  
    
    generate_feature_analysis(df = import_df,
                              selected_features = isolate(input[["features"]]),
                              analysis_choice = isolate(input[["analysis_selection"]]))
    
  })
}


# Defined Elements --------------------------------------------------------
feature_analysis_elements <- function(id) {
  ns <- NS(id)
  ui_elements <- tagList()
  
  # Primary feature delimiter
  ui_elements[["selected_features"]] <- selectizeInput(inputId = ns("features"),
                                                           label = "Select features to display and analyze:",
                                                           multiple = TRUE,
                                                           choices = table_choices,
                                                           selected = table_choices[1:5],
                                                           width = "950px")
  ui_elements[["main_feature_table"]] <- DT::dataTableOutput(outputId = ns("main_feature_table")) %>%
    withSpinner()
  
  # Feature association and correlation tab
  ui_elements[["summary_text"]] <- box(
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
  ui_elements[["analysis_to_run"]] <- selectizeInput(inputId = ns("analysis_selection"),
                                                     label = "Select analysis to run on above features:",
                                                     multiple = FALSE,
                                                     choices = c("select an analysis to run",
                                                                 "Zero-Variance Detection", 
                                                                 "Correlation Analysis",
                                                                 "Association Matrix"),
                                                     selected = "select an analysis to run")
  ui_elements[["run_comparison"]] <- actionButton(inputId = ns("run_comparison"),
                                                 label = "Run Feature Analysis",
                                                 icon("paper_plane"),
                                                 style = "color: #fff; background-color: #337ab7;
                                                          border-color: #2e6da4")
  ui_elements[["analysis_output"]] <- DT::dataTableOutput(outputId = ns("feature_analysis"),
                                                          width = "100%") %>%
    withSpinner()
  
  ui_elements
}

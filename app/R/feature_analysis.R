### Feature Analysis Module: Server, Elements, and UI for evalautoR  ###
###         Author: Aaron Sant-Miller, Booz Allen Hamilton           ###

# User Interface ----------------------------------------------------------
feature_analysis_ui <- function(id) {
  ns <- NS(id)
  ui_elements <- feature_analysis_elements(id)
  
  tabBox(
    width = 12,
    tabPanel(
      title = "Tabular View",
      style = 'padding: 5px;',
      fluidRow(
        docKeyword_ui(DOC_SCOUT))
    )
  ),
  tabPanel(
    title = "Tab 2",
    style = 'padding: 5px;',
    fluidRow(
      docTopic_ui(DOC_SCOUT))
  )
  )
)
}

# docSimilar_ui <- function(id) {
#   ns <- NS(id)
#   ui_elements <- docScout_elements(id)
#   div(
#     style='padding: 0px;',
#     width =12,
#     column(
#       width = 4,
#       box(
#         style ='padding: 0px;',
#         title = "Pointer Document",
#         width = 12,
#         collapsible = T,
#         solidHeader = T,
#         column(width = 12,
#                ui_elements$pointerbox,
#                style = 'padding: 10px;'
#         )
#       )
#     ),
#     column(
#       width = 8,
#       fluidRow(ui_elements$pointer_matched)
#     )
#   )
# }


# Server Code -------------------------------------------------------------
feature_analysis_server <- function(input, output, session) {

  
}


# Defined Elements --------------------------------------------------------
feature_analysis_elements <- function(id) {
  ns <- NS(id)
  ui_elements <- tagList()
  
  
  ui_elements
  
}

### Core evaluatoR User Interface for Application Hosting  ###
###     Author: Aaron Sant-Miller, Booz Allen Hamilton     ###

# Define and construct the primary sidebar for the application ------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Define data import component
    menuItem(
      text = "Data Import",
      tabName = "data_tab",
      icon = icon("refresh")
    ),
    
    # Define feature analysis component
    menuItem(
      text = "Feature Analysis",
      tabName = "feature_tab",
      icon = icon("area-chart")
    ),
    
    # Define model performance component
    menuItem(
      text = "Model Performance",
      tabName = "model_tab",
      icon = icon("bar-chart")
    )
  )
)

# Define and construct the "guts" of the evalutoR dashboard  --------------
# Data Import Tab
data_tab <- tabItem(
  tabName = "data_tab",
  data_import_ui(DATA_IMPORT)
)


# Feature Analysis Tab
feature_tab <- tabItem(
  tabName = "feature_tab",
  feature_analysis_ui(FEATURE_ANALYSIS)
)
 
# Model performance evaluation tab
model_tab <- tabItem(
  tabName = "model_tab",
  model_performance_ui(MODEL_PERFORMANCE)
)

# Put the pieces together into a final dashboard body that holds the "guts" --------------------------
body <- dashboardBody(
  tabItems(
    data_tab,
    feature_tab,
    model_tab
  ),
  
  tags$head(tags$style(HTML('
                            .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                            background-color: #DD4B39;
                            }
                            '))),
  tags$style(HTML("

                  
                  .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:Black
                  }
                  
                  .box.box-solid.box-primary{
                  border-bottom-color:White;
                  border-left-color:Black;
                  border-right-color:Black;
                  border-top-color:Black;
                  }
                  
                  ")),
  tags$style(HTML(".irs-bar {background: blue}"))
)

# Put all the component dashboard pieces together -------------------------
dashboardPage(skin = "blue",
              dashboardHeader(title = "MLexploreR"),
              sidebar,
              body
)



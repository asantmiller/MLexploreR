### Core evaluatoR Server for Application Hosting  ###
### Author: Aaron Sant-Miller, Booz Allen Hamilton ###

shinyServer(function(input, output, session) {
  # Data Import
  callModule(
    data_import_server,
    DATA_IMPORT
  )
  
  # Document Scout
  callModule(
    feature_analysis_server,
    FEATURE_ANALYSIS
  )
 
  # Network Scout
  callModule(
    model_performance_server,
    MODEL_PERFORMANCE
  )
  
})
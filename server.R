# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

# use_python("/Users/sajalkaurminhas/Documents/PhD/Group Project/Risk_classifier",required=T)
# source_python("Source_EA.py")
# x <- func()
# #py_config()

# Run in the shell to find your python directory:
# import os
# import sys
# os.path.dirname(sys.executable)

# ui<-fluidPage(toString(x))
shinyServer(function(input, output, session) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Variable section method",
               "No. of Decision Trees"),
      Value = as.character(c(input$method,
                             input$trees)),
      stringsAsFactors = FALSE)
    
  })
  
 method<-reactive({
   input$method
 })
 
 trees<-reactive({
   input$trees
 })
 
global <- reactiveValues(response = FALSE)
 
Main<-function(){
   if(global$response==T){
    classifier(default_data,  method(),trees())
   }
   else return(NULL)
}

# Show the values in an HTML table ----
output$values <- renderTable({
  input$button
  isolate(sliderValues())
})

observeEvent(input$button, {
  # Show a modal when the button is pressed
  shinyalert("calculating.....please wait", type = "info",showConfirmButton = TRUE,
             showCancelButton = TRUE,
             confirmButtonText = "OK",
             cancelButtonText = "Cancel",callbackR = function(x) {
               global$response <- x
             }
  )
})

})


#shinyApp(ui=ui,server=server)
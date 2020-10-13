# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

# Loading the dataset
load("GermanCredit.Rdata")
GermanCredit<-GermanCredit[,c(10,1:9,11:62)]
default_data <- GermanCredit

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
      Name = c("Variable selection method",
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
 
# Show the values in an HTML table ----
output$values <- renderTable({
  input$button
  isolate(sliderValues())
})

# Show the values in an HTML table ----
output$dataset <- renderDataTable({
  default_data
}, options = list(pageLength=10), escape = FALSE)


Main<-function(){
  if(global$response==T){
    classifier(default_data,  method(),trees())
  }
  else return(NULL)
}

#classifier_outputs <- Main()

#f_evolve <- function(){
# use_python("C:/Users/fredx/Anaconda3",required=T) 
# for (Ctree in C$Trees) {
# rules <- tidyRules(CTree)
# }
#}

# Show the values in an HTML table ----
output$Reduced_data <- renderDataTable({
  Main()$Reduced_data
  #classifier_outputs$Reduced_data
}, options = list(pageLength=10), escape = FALSE)



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
source("Main.R")

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
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$sample_file
    
    if (is.null(inFile))
      return(default_data)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  output$dataset <- renderDataTable({
    default_data
  }, options = list(lengthMenu = c(5, 10, 20, 50,100,500,1000), pageLength = 5), escape = FALSE)
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("Variable selection method",
               "No. of Decision Trees","View the specific Decision Tree","File format/type"),
      Value = as.character(c(input$method,
                             input$trees, input$viewtree, input$filetype)),
      stringsAsFactors = FALSE)
    
  })
  
 method<-reactive({
   input$method
 })
 
 trees<-reactive({
   input$trees
 })
 
 viewtree<-reactive({
   input$viewtree
 })
 
global <- reactiveValues(response = FALSE)
 
# Show the values in an HTML table ----
output$values <- renderTable({
  input$button
  isolate(sliderValues())
})

# Show the values in an HTML table ----


output$col <- renderTable({
  input$button
  colnames(default_data)
}, caption=paste("Variables in the dataset"),
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))


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

Main<-function(){
  if(global$response==T){
    Classifier(default_data,  method(),trees())
  }
  else return(NULL)
}

# Show the values in an HTML table ----
output$Reduced_data <- renderDataTable({
  
  input$button
  isolate(if(global$response==T){
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Function Running', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      Main()$Reduced_data
    })
  } else  return(NULL)
  )
}, options = list(lengthMenu = c(5, 10, 20, 50,100,500,1000), pageLength = 5), 
escape = FALSE)

output$colred <- renderTable({
  input$button
  colnames(Main()$Reduced_data)
}, caption=paste("Reduced variables in the dataset"),
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))

output$plot<-renderPlot({
  input$button1
  
  isolate(if(global$response==T){
    
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      rpart.plot(Main()$Trees[[viewtree()]],roundint=FALSE)
    })
  }
  else  return(NULL)
  )
})


observeEvent(input$button1, {
  # Show a modal when the button is pressed
  shinyalert("Showing plot.....please wait", type = "info",showConfirmButton = TRUE,
             showCancelButton = TRUE,
             confirmButtonText = "OK",
             cancelButtonText = "Cancel",callbackR = function(x) {
               global$response <- x
             }
  )
})

output$res<-renderTable({
  Main()$Accuracy[[viewtree()]]
  Main()$AUROC[[viewtree()]]
  Main()$Gini_Index[[viewtree()]]
})

output$down<-downloadHandler(
  #Specify filename
  filename = function(){
    paste("DecisionTree",input$filetype,sep=".")
  },
  content = function(file){
    #open the device <-png(),pdf()
    # create/write the plot
    #close the device
    if(input$filetype=="png")
      png(file)
    else
      pdf(file)
    Main()$Reduced_data
    dev.off()
  }
)


output$down1<-downloadHandler(
  #Specify filename
  filename = function(){
    paste("DecisionTree",input$filetype,sep=".")
  },
  content = function(file){
    #open the device <-png(),pdf()
    # create/write the plot
    #close the device
    if(input$filetype=="png")
      png(file)
    else
      pdf(file)
    prp(Main()$Trees[[viewtree()]],roundint=FALSE)
    dev.off()
  }
)

})


#shinyApp(ui=ui,server=server)
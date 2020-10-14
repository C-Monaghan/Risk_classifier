source("Main.R")

# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

# Loading the dataset
load("GermanCredit.Rdata")
default_data<-GermanCredit





use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
source_python("Source_EA.py")

initiate_population <- function(Forest){
  bad_trees_count=0
  for (Ctree in C$Trees) {
    if (tree.size(Ctree) > 1){
      bad_trees_count = bad_trees_count+1
    }
    else{
      rules <- tidyRules(Ctree)
      #print(rules)
      PDT$'insert_r_tree_to_population'(rules)
    }
  }
  print(paste0("Bad trees: ", bad_trees_count))
  for (i in 1:bad_trees_count){
    random_tree = PDT$'generate_random_tree'()
    PDT$'insert_tree_to_population'(random_tree)
  }
}

initiate_ea <- function(forest,dataset, tournament_size = 3, crossover_rate = 0.5, mutation_rate = 0.4) {
  PDT <- DecisionTree_EA(tournament_size = tournament_size,
                         crossover_rate = crossover_rate,
                         mutation_rate = mutation_rate,
                         elitism_rate = 0.1,
                         hall_of_fame_size = 3)
  PDT$'adapt_to_data'(labels = dataset$Class, data=dataset) #CHANGE: hardcoded to Class
  names <- PDT$'get_attribute_names'()
  n_atts = length(names)
  values <- PDT$'get_crucial_values'()
  print(values)
  len <- sapply(values,length)
  len <- n_atts - len
  crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
  #print("values")
  #print(values)
  print("names")
  print(names)
  print(length(names))
  
  #crucial_values <- data.frame(values)
  #colnames(crucial_values) <- names
  print(crucial_values)
  #initiate_population(forest)
  return (crucial_values)
}

evolve <- function(generations){
  winner <- PDT$evolve(generations)
  return(winner)
}

testf <- function(){
  na <- test_get_names()
  return(na)
}





shinyServer(function(input, output, session){
  
  output$dataset <- renderDataTable({
    
    inFile <- input$sample_file
    
    if (is.null(inFile))
      return(default_data)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  }, options = list(lengthMenu = c(2,5, 10, 20, 50,100,500,1000), pageLength = 2), escape = FALSE)
  
  ## Original Data
  original_data <- reactive({
    input$button
    inFile <- input$sample_file
    if (is.null(inFile))
      return(default_data)
    
    else {
      ori_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      ori_data
      }
       })
  
  
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
  colnames(original_data())
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

Main<-reactive({
  input$button
Classifier(original_data(), method(),trees())
})

#classifier_outputs <- Main()

#f_evolve <- function(){
# use_python("C:/Users/fredx/Anaconda3",required=T) 
# for (Ctree in C$Trees) {
# rules <- tidyRules(CTree)
# }
#}

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
}, options = list(lengthMenu = c(2,5, 10, 20, 50,100,500,1000), pageLength = 2), 
escape = FALSE)


output$colred <- renderTable({
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

observeEvent(input$evolve, {
  initiate_ea(forest = Main()$Trees, dataset = Main()$Test_data)
  #print("hola")
  #print(length(Main()$Trees))
  #print(testf())
  #rpart.plot(Main()$Trees[[1]])
})

output$res<-renderTable({
  input$button1
  Main()$Accuracy[[viewtree()]]
  # Main()$AUROC[[viewtree()]]
  # Main()$Gini_Index[[viewtree()]]
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
    rpart.plot(Main()$Trees[[viewtree()]],roundint=FALSE)
    dev.off()
  }
)

})


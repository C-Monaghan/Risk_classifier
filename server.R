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

PDT <- DecisionTree_EA(tournament_size = 5,
                       crossover_rate = 0.6,
                       mutation_rate = 0.3,
                       elitism_rate = 0.1,
                       hall_of_fame_size = 3)

 progress_values <- reactiveValues()
 progress_values$gens <- c(0)
 progress_values$best_values <- c(0.5)
 progress_values$mean_values <- c(0.5)

initiate_ea <- function(forest,dataset) {
  #source_python("Source_EA.py") #temp

  PDT$'adapt_to_data'(labels = dataset$Class, data=dataset) #CHANGE: hardcoded to Class
  print(PDT$'generation')
  print(paste0(PDT$'generation'))

  bad_trees_count=0
  for (Ctree in forest) {
    if (nrow(Ctree$frame) < 2){
      #print(tree.size(Ctree))
      bad_trees_count = bad_trees_count+1
    }
    else{
      rules <- tidyRules(Ctree)
      PDT$'insert_r_tree_to_population'(rules)
    }
  }
  print(paste0("Bad trees: ", bad_trees_count))
  for (i in 1:bad_trees_count){
    random_tree = PDT$'generate_random_tree'()
    PDT$'insert_tree_to_population'(random_tree)
  }

  names <- PDT$'get_attribute_names'()
  values <- PDT$'get_crucial_values'()
  len <- sapply(values,length)
  m_l <- max(len)
  len <- m_l - len
  crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
  colnames(crucial_values) <- names
  print(crucial_values)

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
  isolate(if(global$response==T){
Classifier(original_data(),  method(),trees())
  } else  return(NULL)
  )
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
  input$button
  isolate(if(global$response==T){
 colnames(Main()$Reduced_data)
  } else  return(NULL)
  )
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

observeEvent(input$seed, {
  crucial_values <- initiate_ea(forest = Main()$Trees, dataset = Main()$Reduced_data)
  output$crucial_values <- renderDataTable(crucial_values)})


Values <- reactive({

  data.frame(
    Name = c("Accuracy of the Tree",
             "Gini Index","AUROC"),
    Value = as.character(c(Main()$Accuracy[[viewtree()]], Main()$Gini_Index[[viewtree()]],
                           Main()$AUROC[[viewtree()]])),
    stringsAsFactors = FALSE)

})

update_progress <- function(g, current_best_value, current_mean_value) {
  progress_values$best_values = c(progress_values$best_values, current_best_value)
  progress_values$mean_values = c(progress_values$mean_values, current_mean_value)
  progress_values$gens = c(progress_values$gens, tail(progress_values$gens, n=1) + g)
}

observeEvent(input$evolve, {
  print(input$generations)
  for (g in 1:input$generations){
    PDT$'evolve'()
    current_best_value <- PDT$'get_best_value_for_objective'()
    current_mean_value <- PDT$'get_population_mean_for_objective'()
    update_progress(g, current_best_value, current_mean_value)
    #progress_values$best_values = c(progress_values$best_values, current_best_value)
    #progress_values$mean_values = c(progress_values$mean_values, current_mean_value)
    #progress_values$gens = c(progress_values$gens, tail(progress_values$gens, n=1) + g)
  }

})

observeEvent(input$b_update_progress, {
    update_progress(c(),c(),c())
  })

output$evolution_progress <- renderPlot({
  #plot(x = progress_values$gens, y=progress_values$best_values, type = "o", ylim = c(0,1))
  plot(x=progress_values$gens, y=progress_values$best_values, type="o", lty=1, ylim=c(0,1),
       axes=F, bty="n", xaxs="i", yaxs="i", main="Accuracy progress",
       xlab="Generation", ylab="Accuracy")
  
  # plot dashed line
  lines(x=progress_values$gens, y=progress_values$mean_values, lty=2)
  
  # add axes
  axis(side=1, at=progress_values$gens)
  axis(side=2, at=seq(0.5,1,0.05), las=1)
  
  # add vertical red line
  abline(h=progress_values$best_values[[2]], col="red")
  
  # add legend
  par(xpd=TRUE)
  legend(x=1.5, y=2, legend=c("Best tree", "Population mean"), lty=1:2, box.lty=0, ncol=2)
  })
#output$evolution_progress <- renderPlot(plot(x = unlist(gens), y=unlist(best_values), type = "o", ylim = c(0,1)))

output$res<-renderTable({
  input$button1
  isolate(if(global$response==T){
    Values()
  }
  else  return(NULL)
  )
})

#~~~~ Download the reduced dataset
output$down <- downloadHandler(
  filename = function() {
    paste("ReducedData.csv", sep = "")
  },
  content = function(file) {
    write.csv(Main()$Reduced_data, file, row.names = FALSE)
  }
)

#~~~~ Download the decision tree plot
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

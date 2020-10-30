source("Main.R")

# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

# Loading the dataset
load("GermanCredit.Rdata")
data<-GermanCredit


shinyServer(function(input, output, session){
  
  options(shiny.maxRequestSize=100*1024^2)
  
  ## Original Data
  original_data <- reactive({
    input$button
    inFile <- input$sample_file
    if (is.null(inFile))
      return(data)

    else {
      ori_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      ori_data
      }
       })

  output$dataset <- renderDataTable({
    
    original_data()
  }%>% datatable(selection=list(target="cell"),
                 options = list(scrollX = TRUE,
                                paginate = T,
                                lengthMenu = c(2,5, 10, 20, 50,100,500,1000),
                                pageLength = 10,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'color': '#fff'});",
                                  "}")
                 )) %>% DT::formatStyle(columns = names(original_data()), color="blue"))
  
 

  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({

    data.frame(
      Name = c("Variable selection method",
               "No. of Decision Trees","File format/type","Option"),
      Value = as.character(c(input$method,
                             input$trees, input$filetype,input$option)),
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
  enable("button1")
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
    
    withProgress({Main()$Reduced_data},
                 message = 'Function Running', value = 0.8  )
      
  
  } else  return(NULL)
  )
}%>% datatable(selection=list(target="cell"),
               options = list(scrollX = TRUE,
                              paginate = T,
                              lengthMenu = c(2,5, 10, 20, 50,100,500,1000),
                              pageLength = 10,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'color': '#fff'});",
                                "}")
               )) %>% DT::formatStyle(columns = names(Main()$Reduced_data), color="blue"))


output$colred <- renderTable({
  input$button
  isolate(if(global$response==T){
 colnames(Main()$Reduced_data)
  } else  return(NULL)
  )
}, caption=paste("Reduced variables in the dataset"),
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))

option<-reactive({
  input$button1
  isolate(if(global$response==T){
  input$option
  }
  else  return(NULL)
  )
})

output$plot<-renderPlot({
  input$button1
  
  if(option()=="ind_max_acc"){
  isolate(if(global$response==T){

    withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_acc]],roundint=FALSE)},
                 message = 'Making plot', value = 0.5 )
  }
  else  return(NULL)
  )
  }else if (option()=="ind_min_gini") {
    isolate(if(global$response==T){
      
      withProgress({rpart.plot(Main()$Trees[[Main()$ind_min_gini]],roundint=FALSE)},
                   message = 'Making plot', value = 0.5 )
        
      
    }
    else  return(NULL)
    )
  }else {
    isolate(if(global$response==T){
  
      
      withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_AUROC]],roundint=FALSE)},
                   message = 'Making plot', value = 0.5  )
    }
    else  return(NULL)
    )
  }
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


Values <- reactive({

  if(option()=="ind_max_acc"){

  data.frame(
    Name = c("Accuracy of the Tree",
             "Gini Index","AUROC"),
    Value = as.character(c(Main()$Accuracy[[Main()$ind_max_acc]], Main()$Gini_Index[[Main()$ind_max_acc]],
                           Main()$AUROC[[Main()$ind_max_acc]])),
    stringsAsFactors = FALSE)

  }
  else  if(option()=="ind_min_gini"){
  data.frame(
    Name = c("Accuracy of the Tree",
             "Gini Index","AUROC"),
    Value = as.character(c(Main()$Accuracy[[Main()$ind_min_gini]], Main()$Gini_Index[[Main()$ind_min_gini]],
                           Main()$AUROC[[Main()$ind_min_gini]])),
    stringsAsFactors = FALSE)
  }
  else {
    data.frame(
      Name = c("Accuracy of the Tree",
               "Gini Index","AUROC"),
      Value = as.character(c(Main()$Accuracy[[Main()$ind_max_AUROC]], Main()$Gini_Index[[Main()$ind_max_AUROC]],
                             Main()$AUROC[[Main()$ind_max_AUROC]])),
      stringsAsFactors = FALSE)
  }

})


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


######################################################################################
##########  EVOLUTIONARY ALGORITHM ###################################################
######################################################################################


# use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
use_python("/Users/sajalkaurminhas/anaconda3/bin/python",required=T)
source_python("Source_EA.py")
disable("evolve")

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
  PDT$'add_objective'(objective_name = "accuracy") #new
  
  names <- PDT$'get_attribute_names'()
  values <- PDT$'get_crucial_values'()
  len <- sapply(values,length)
  m_l <- max(len)
  len <- m_l - len
  crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
  colnames(crucial_values) <- names
  #print(crucial_values)
  
  return (crucial_values)
}

update_progress <- function(current_best_value, current_mean_value) {
  progress_values$best_values <<- c(progress_values$best_values, current_best_value)
  progress_values$mean_values <<- c(progress_values$mean_values, current_mean_value)
  progress_values$gens <<- c(progress_values$gens, tail(progress_values$gens, n=1) + 1)
  #print(paste0("Updated values after gen ",progress_values$gens,", tail:",tail(progress_values$gens, n=1)))
}

observeEvent(input$evolve, {
  print(paste0("Running ",input$generations," generations"))
  withProgress(message = "Evolving",
               value = 0,
               {
                 for (g in 1:input$generations){
                   PDT$'evolve'()
                   current_best_value <- PDT$'get_best_value_for_objective'()
                   current_mean_value <- PDT$'get_population_mean_for_objective'()
                   update_progress(current_best_value, current_mean_value)
                   incProgress(1/input$generations)
                 }
               })
})

observeEvent(input$seed, {
  crucial_values <- initiate_ea(forest = Main()$Trees, dataset = Main()$Test_data)
  #output$crucial_values <- renderDataTable(crucial_values)})
  #output$crucial_values = renderDT(crucial_values, options = list())
  output$crucial_values = renderDT(crucial_values %>% datatable(selection=list(target="cell"),
                                                                options = list(scrollX = TRUE,
                                                                               paginate = T,
                                                                               lengthMenu = c(5, 10, 15),
                                                                               pageLength = 15,
                                                                               initComplete = JS(
                                                                                 "function(settings, json) {",
                                                                                 "$(this.api().table().header()).css({'color': '#fff'});",
                                                                                 "}")
  )) %>% DT::formatStyle(columns = names(crucial_values), color="blue"))
  enable("evolve")
})

output$evolution_progress <- renderPlot({
  #plot(x = progress_values$gens, y=progress_values$best_values, type = "o", ylim = c(0,1))
  plot(x=progress_values$gens, y=progress_values$best_values, type="o", lty=1, ylim=c(0.5,1),
       axes=F, bty="n", xaxs="i", yaxs="i", main="Accuracy progress",
       xlab="Generation", ylab="Accuracy")
  
  # plot dashed line
  lines(x=progress_values$gens, y=progress_values$mean_values, lty=2)
  
  # add axes
  axis(side=1, at=progress_values$gens)
  axis(side=2, at=seq(0.5,1,0.05), las=1)
  grid()
  
  # add vertical red line
  abline(h=progress_values$best_values[[2]], col="red")
  
  # add legend
  par(xpd=TRUE)
  legend(x=1.5, y=2, legend=c("Best tree", "Population mean"), lty=1:2, box.lty=0, ncol=2)
})



})

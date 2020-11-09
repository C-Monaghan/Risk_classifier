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
   
    inFile <- input$sample_file
    if(input$data_choice == FALSE){
      return(data)
    }
      else if(is.null(inFile)) { 
        return(NULL)
      }
    else {
      ori_data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      ori_data
    }
       })

  output$dataset <- renderDataTable({  
      if(is.null(original_data())){return()}
      original_data()
  },options = list(pageLength=10, lengthMenu = c(2,5 ,10, 20, 50,100,500,1000),scrollX = TRUE, paginate = T))

  

  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({

    data.frame(
      Name = c("Variable selection method",
               "No. of Decision Trees"),
      Value = as.character(c(input$method,
                             input$trees)),
      stringsAsFactors = FALSE)

  })
  # Dynamically adjust Usage guide Main layout width
  observeEvent(input$tabs, {
    if(input$tabs == 'steps') {
      removeClass("main_layout", "col-sm-9")
      addClass("main_layout", "col-sm-12")
    }
    else {
      removeClass("main_layout", "col-sm-12")
      addClass("main_layout", "col-sm-9")
    }
  })
  
  # Data button from Quick Start
  observeEvent(input$data,
               isolate({
                 updateTabsetPanel(session, "tabs",
                                   selected = "data")
               })
  )
  
  # Method specification button from Quick Start
  observeEvent(input$method_spec,
               isolate({
                 updateTabsetPanel(session, "tabs",
                                   selected = "specification")
               })
  )
  # Parameter button from View dataset tab
  observeEvent(input$parameter_button,
               isolate({
                 updateTabsetPanel(session, "tabs",
                                   selected = "specification")
               })
  )
  
  # Return Parameter button from View dataset tab
  observeEvent(input$return_parameter_button,
               isolate({
                 updateTabsetPanel(session, "tabs",
                                   selected = "specification")
               })
  )

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

Main<-reactive({
  
    Classifier(original_data(),  method(),trees())
  
})


# Submit Calculate Parameter Button
observeEvent(input$button, {
            isolate({
               # Show a modal when the button is pressed
               shinyalert("calculating.....please wait", type = "info",showConfirmButton = TRUE,
                          showCancelButton = TRUE,
                          confirmButtonText = "OK",
                          cancelButtonText = "Cancel",callbackR = function(x) {
                            global$response <- x
                          }
               )
               output$Reduced_data <- renderDataTable({ 
                 if(is.null(original_data())){return()}
                 
                 Main()$Reduced_data
               },options = list(pageLength=10, lengthMenu = c(2,5 ,10, 20, 50,100,500,1000),scrollX = TRUE, paginate = T))
               
               withProgress({Main()$Reduced_data},message = 'Function Running', value = 0.8  )
               
               # Switches to Reduced Dataset after variable election tab if User is in Method specification and when model has finished running.
               if(input$tabs == 'specification') {
                 updateTabsetPanel(session, "tabs",
                                   selected = "download")
               }
               
            }) 
            
})


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

    withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_acc]],roundint=FALSE,extra=104, box.palette="GnBu",
                             branch.lty=3, shadow.col="gray", nn=TRUE)},
                 message = 'Making plot', value = 0.5 )
  }
  else  return(NULL)
  )
  }else if (option()=="ind_min_gini") {
    isolate(if(global$response==T){
      
      withProgress({rpart.plot(Main()$Trees[[Main()$ind_min_gini]],roundint=FALSE,extra=104, box.palette="GnBu",
                               branch.lty=3, shadow.col="gray", nn=TRUE)},
                   message = 'Making plot', value = 0.5 )
        
      
    }
    else  return(NULL)
    )
  }else {
    isolate(if(global$response==T){
  
      
      withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_AUROC]],roundint=FALSE,extra=104, box.palette="GnBu",
                               branch.lty=3, shadow.col="gray", nn=TRUE)},
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
    if(option()=="ind_max_acc"){
    #open the device <-png(),pdf()
    # create/write the plot
    #close the device
    if(input$filetype=="png")
      png(file)
    else
      pdf(file)
    rpart.plot(Main()$Trees[[Main()$ind_max_acc]],roundint=FALSE,extra=104, box.palette="GnBu",
               branch.lty=3, shadow.col="gray", nn=TRUE)
    dev.off()
    }
    else  if(option()=="ind_min_gini"){
      if(input$filetype=="png")
        png(file)
      else
        pdf(file)
      rpart.plot(Main()$Trees[[Main()$ind_min_gini]],roundint=FALSE,extra=104, box.palette="GnBu",
                 branch.lty=3, shadow.col="gray", nn=TRUE)
      dev.off()
    }
    else {
      if(input$filetype=="png")
        pdf(file)
      else
        png(file)
      rpart.plot(Main()$Trees[[Main()$ind_max_AUROC]],roundint=FALSE,extra=104, box.palette="GnBu",
                 branch.lty=3, shadow.col="gray", nn=TRUE)
      dev.off()
    }
  }
)





######################################################################################
##########  EVOLUTIONARY ALGORITHM ###################################################
######################################################################################







#use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
#use_python("/Users/sajalkaurminhas/anaconda3/bin/python",required=T)


source_python("Source_EA.py")
disable("evolve")
disable("restart_evolution")
seeded_evolution <- FALSE
nodes_objective_index <- NULL
accuracy_objective_index <- NULL

PDT <- DecisionTree_EA(tournament_size = 5,
                       crossover_rate = 0.6,
                       mutation_rate = 0.3,
                       elitism_rate = 0.1,
                       hall_of_fame_size = 3)

progress_values <- reactiveValues()
#progress_values$gens <- c()
progress_values$best_values <- c()
progress_values$mean_values <- c()
progress_values$best_tree_nodes <- c()
progress_values$mean_nodes <- c()
progress_values$df <- data.frame()

initiate_ea <- function(forest,dataset) {
  #source_python("Source_EA.py") #temp
  
  PDT$'adapt_to_data'(labels = dataset$Class, data=dataset) #CHANGE: hardcoded to Class
  
  bad_trees_count=0
  for (Ctree in forest) {
    if (nrow(Ctree$frame) < 5){
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
  PDT$'add_objective'(objective_name = "accuracy")
  PDT$'add_objective'(objective_name = "nodes", to_max = FALSE) #new
  nodes_objective_index <<- 1
  accuracy_objective_index <<- 0
  
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

update_progress <- function(current_best_value, current_mean_value, current_best_tree_nodes, current_mean_nodes) {
  progress_values$best_values <<- c(progress_values$best_values, current_best_value)
  progress_values$mean_values <<- c(progress_values$mean_values, current_mean_value)
  #progress_values$gens <<- c(progress_values$gens, tail(progress_values$gens, n=1) + 1)
  progress_values$best_tree_nodes <<- c(progress_values$best_tree_nodes, current_best_tree_nodes)
  progress_values$mean_nodes <<- c(progress_values$mean_nodes, current_mean_nodes)
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
                   #current_best_tree_nodes <- PDT$'get_best_value_for_objective'(objective_index = nodes_objective_index)
                   current_best_tree_nodes <- PDT$'get_best_individual'(objective_index=0)$'objective_values'[[nodes_objective_index+1]]
                   current_mean_nodes <- PDT$'get_population_mean_for_objective'(objective_index = nodes_objective_index)
                   update_progress(current_best_value, current_mean_value, current_best_tree_nodes, current_mean_nodes)
                   incProgress(1/input$generations)
                 }
               })
  enable("restart_evolution")
})


observeEvent(input$seed, {
  crucial_values <- initiate_ea(forest = Main()$Trees, dataset = Main()$Train_data)
  #output$crucial_values <- renderDataTable(crucial_values)})
  #output$crucial_values = renderDT(crucial_values, options = list())
  output$crucial_values = renderDataTable({ crucial_values}%>% datatable(selection=list(target="cell"),options = list(pageLength=10, lengthMenu = c(5, 10, 15),scrollX = TRUE, paginate = T)))
    # renderDT(crucial_values %>% datatable(selection=list(target="cell"),
    #                                                             options = list(scrollX = TRUE,
    #                                                                            #scrolly = TRUE,
    #                                                                            paginate = T,
    #                                                                            lengthMenu = c(5, 10, 15),
    #                                                                            pageLength = 15,
    #                                                                            initComplete = JS(
    #                                                                              "function(settings, json) {",
    #                                                                              "$(this.api().table().header()).css({'color': '#fff'});",
    #                                                                              "}")
  #)) %>% DT::formatStyle(columns = names(crucial_values), color="blue"))
  enable("evolve")
  seeded_evolution <<- TRUE
})

output$evolution_progress <- renderPlot({
  # df<- cbind(data.frame(progress_values$best_values),
  #            data.frame(progress_values$mean_values),
  #            data.frame(progress_values$best_tree_nodes),
  #            data.frame(progress_values$mean_nodes))
  # Generation <- as.numeric(row.names(df))
  # plot(x=Generation, y=progress_values$best_values, type="o", lty=1, ylim=c(0.5,1),
  #      axes=F, bty="n", xaxs="i", yaxs="i", main="Accuracy progress",
  #      xlab="Generation", ylab="Accuracy")
  # 
  # # plot dashed line
  # lines(x=Generation, y=progress_values$mean_values, lty=2)
  # 
  # # add axes
  # axis(side=1, at=Generation)
  # axis(side=2, at=seq(0.5,1,0.05), las=1)
  # grid()
  # 
  # # add vertical red line
  # abline(h=progress_values$best_values[[1]], col="red")
  # 
  # # add legend
  # par(xpd=TRUE)
  # legend(x=1.5, y=2, legend=c("Best tree", "Population mean"), lty=1:2, box.lty=0, ncol=2)
  
    df<- cbind(data.frame(progress_values$best_values),
               data.frame(progress_values$mean_values),
               data.frame(progress_values$best_tree_nodes),
               data.frame(progress_values$mean_nodes))
    Generation <- as.numeric(row.names(df))
    ggplot(data = df, aes(x=Generation))+
      geom_line(aes(y=progress_values$best_values), color = "darkblue", size=1) +
      geom_point(aes(y=progress_values$best_values), color = "darkblue", size=3) +
      geom_line(aes(y=progress_values$mean_values), color = "blue", linetype="twodash") +
      xlab("Generation") +
      ylab("Accuracy")
})


output$evolution_progress_nodes <- renderPlot({
  df<- cbind(data.frame(progress_values$best_values),
            data.frame(progress_values$mean_values),
            data.frame(progress_values$best_tree_nodes),
            data.frame(progress_values$mean_nodes))
  Generation <- as.numeric(row.names(df))
  ggplot(data = df, aes(x=Generation))+
    geom_line(aes(y=progress_values$best_tree_nodes), color = "darkblue", size = 1) +
    geom_point(aes(y=progress_values$best_tree_nodes), color = "darkblue", size=3) +
    geom_line(aes(y=progress_values$mean_nodes), color = "blue", linetype="twodash") +
    xlab("Generation") +
    ylab("Nodes")
})

observeEvent(input$update_tree, {
  output$network <- renderVisNetwork({
    PDT$'evaluate_population'()
    best_tree <- PDT$'get_best_individual'(objective_index=0)$'genotype'
    best_tree_nodes <- best_tree$'get_subtree_nodes'()
    #length(best_tree_nodes)
    connections <- best_tree$'get_connections'()
    connections
    c_from <- connections[[1]]
    c_to <- connections[[2]]
    c_color <- connections[[3]]
    new_edges <- data.frame(from = c_from, to=c_to, color=c_color)
    new_df <- data.frame(id=c(),label=c(), shape=c())
    i=0
    for (node in best_tree_nodes){
      label = (toString(node))
      color="lightblue"
      font_color = "black"
      if (node$'is_terminal'() == TRUE){
        shape="square"
      }
      else{
        if (node$'is_root'()){
          shape="triangle"
          color="blue"
        }
        else{
          shape="triangle"
        }
      }
      
      new_df <- rbind(new_df, data.frame(id=i,
                                         label=label, 
                                         shape=shape,
                                         color=color,
                                         font.color = font_color))
      i=i+1
    }
    #new_df
    visNetwork(new_df, new_edges, height = "500px", width = "100%") %>% 
      visEdges(arrows = "from") %>% 
      visHierarchicalLayout()
  })
})

})

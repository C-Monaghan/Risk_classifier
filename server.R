source("Main.R")

# Library for running shinyapp
library(shiny)

# Library for using python scripts in shiny
library(reticulate)

# Loading the dataset
load("GermanCredit.Rdata")
default_data<-GermanCredit


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
        #Sys.sleep(0.1)
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
      rpart.plot(Main()$Trees[[Main()$ind_max_acc]],roundint=FALSE)
    })
  }
  else  return(NULL)
  )
  }else if (option()=="ind_min_gini") {
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
        rpart.plot(Main()$Trees[[Main()$ind_min_gini]],roundint=FALSE)
      })
    }
    else  return(NULL)
    )
  }else {
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
        rpart.plot(Main()$Trees[[Main()$ind_max_AUROC]],roundint=FALSE)
      })
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






use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
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
                   current_best_tree_nodes <- PDT$'get_best_individual'()$'objective_values'[[nodes_objective_index+1]]
                   current_mean_nodes <- PDT$'get_population_mean_for_objective'(objective_index = nodes_objective_index)
                   update_progress(current_best_value, current_mean_value, current_best_tree_nodes, current_mean_nodes)
                   incProgress(1/input$generations)
                 }
               })
  enable("restart_evolution")
})

observeEvent(input$seed, {
  crucial_values <- initiate_ea(forest = Main()$Trees, dataset = Main()$Test_data)
  #output$crucial_values <- renderDataTable(crucial_values)})
  #output$crucial_values = renderDT(crucial_values, options = list())
  output$crucial_values = renderDT(crucial_values %>% datatable(selection=list(target="cell"),
                                                                options = list(scrollX = TRUE,
                                                                               #scrolly = TRUE,
                                                                               paginate = T,
                                                                               lengthMenu = c(5, 10, 15),
                                                                               pageLength = 15,
                                                                               initComplete = JS(
                                                                                 "function(settings, json) {",
                                                                                 "$(this.api().table().header()).css({'color': '#fff'});",
                                                                                 "}")
  )) %>% DT::formatStyle(columns = names(crucial_values), color="blue"))
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
    best_tree <- PDT$'get_best_individual'()$'genotype'
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
      font_color = "white"
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

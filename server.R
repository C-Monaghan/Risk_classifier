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
  
  
  global <- reactiveValues(response=FALSE)
  global_plot <- reactiveValues(value=TRUE)
  
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
  
  
  
  # Submit Calculate Parameter Button
  observeEvent(input$button, {
    # print(input$button)
    # Show a modal when the button is pressed
    shinyalert("calculating.....please wait", type = "success",showConfirmButton = TRUE,
               showCancelButton = TRUE,
               confirmButtonText = "OK",
               cancelButtonText = "Cancel",
               animation = TRUE,
               callbackR = function(x) {
                 global$response <- x
               })
  })
  
  Main<-reactive({
    
    Classifier(original_data(),  method(),trees())
    
  })  
  
  # Submit Calculate Parameter Button
  observeEvent(global$response, {
    if(isolate(global$response)==T){
      
      output$Reduced_data <- renderDataTable({ 
        if(is.null(original_data())){return()}
        
        Main()$Reduced_data
      },options = list(pageLength=10, lengthMenu = c(2,5 ,10, 20, 50,100,500,1000),scrollX = TRUE, paginate = T))
      
      output$colred <- renderTable({
        
        
        colnames(Main()$Reduced_data)
        
      }, caption=paste("Reduced variables in the dataset"),
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      
      withProgress({Main()$Reduced_data},message = 'Function Running', value = 0.8  )
      
      # Switches to Reduced Dataset after variable selection tab if User is in Method specification and when model has finished running.
      if(input$tabs == 'specification') {
        updateTabsetPanel(session, "tabs",
                          selected = "download")
      }
      global$response= FALSE
    }
    
    
    else { return(NULL) 
    }
    
    
    
  })
  
  observeEvent(input$button1, {
    # Show a modal when the button is pressed
    shinyalert("Showing plot.....please wait", type = "info",showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               callbackR = function(x) {
                 global_plot$value <- x
               }
    )
  })
  
  option<-reactive({
  input$button1
    isolate(if(global_plot$value==T){
      input$option
    }
    else  return(NULL)
    )
  })
  

      output$plot<-renderPlot({
        
        
        if(option()=="ind_max_acc"){
          
            withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_acc]],roundint=FALSE,extra=104, box.palette="GnBu",
                                     branch.lty=3, shadow.col="gray", nn=TRUE)},
                         message = 'Making plot', value = 0.5 )
          
        }else if (option()=="ind_min_gini") {
         
            
            withProgress({rpart.plot(Main()$Trees[[Main()$ind_min_gini]],roundint=FALSE,extra=104, box.palette="GnBu",
                                     branch.lty=3, shadow.col="gray", nn=TRUE)},
                         message = 'Making plot', value = 0.5 )
            
        }else {
          
            
            
            withProgress({rpart.plot(Main()$Trees[[Main()$ind_max_AUROC]],roundint=FALSE,extra=104, box.palette="GnBu",
                                     branch.lty=3, shadow.col="gray", nn=TRUE)},
                         message = 'Making plot', value = 0.5  )
          
        }
      }) 
     
   
  
  
  Values <- reactive({
    input$button1
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
    isolate(if(global_plot$value==T){
      Values()
  }
)




######################################################################################
######################################################################################
##########  EVOLUTIONARY ALGORITHM ###################################################
######################################################################################
######################################################################################

source_python("Source_EA.py")

#Parameters
available_objectives <- c("accuracy", "nodes", "sensitivity", "sensibility") #ordering must be kept
to_max <- c(TRUE, FALSE, TRUE, TRUE)
initially_included <- c(FALSE, FALSE, FALSE, FALSE)
PDT <- DecisionTree_EA(tournament_size = 5,
                       crossover_rate = 0.4,
                       mutation_rate = 0.6,
                       elitism_rate = 0.0,
                       hall_of_fame_size = 3)


#Initial setup





#use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
#use_python("/Users/sajalkaurminhas/anaconda3/bin/python",required=T)

#source_python("Source_EA.py")

disable("evolve")
disable("restart_evolution")
disable("accuracy_checkbox")
seeded_evolution <- FALSE
nodes_objective_index <- NULL
accuracy_objective_index <- NULL
max_objectives <- length(available_objectives)

#Reactives
reactive_variables <- reactiveValues()
reactive_variables$objectives <- data.frame("Index"=0:(max_objectives-1),
                                            "Objective_name"=available_objectives,
                                            "To_max" = to_max,
                                            "Included" = initially_included,
                                            "Best_values" = vector(mode="double", length=max_objectives),
                                            "Mean_values" = vector(mode="double", length=max_objectives),
                                            "Std_devs" = vector(mode="double", length=max_objectives))
reactive_variables$progress <- data.frame("Gen"=integer(),
                                           "Objective_name"=character(),
                                           "Type"=character(),
                                           "Value"=double(),
                                           "Low"=double(),
                                           "High"=double(),
                                           stringsAsFactors = FALSE)
reactive_variables$pareto <- data.frame("Gen"=integer(),
                                        "Individual_index"=integer(),
                                        "Generation_of_creaton"=integer(),
                                        "Rank"=integer(),
                                        "Nodes"=integer(),
                                        "accuracy"=double(),
                                        "nodes"=double(),
                                        "sensitivity"=double(),
                                        "sensibility"=double()) #there should be a way to add the column names from the available_objectives variable

########################
# Functions ############

update_reactive_variables <- function(){
  current_generation <- PDT$'current_generation'
  
  #Update the individuals
  individual_counter <- 0
  for (individual in PDT$'population'){
    #individual=PDT$'population'[[1]]
    objective_values <- c()
    for (objective_value in individual$'objective_values'){
      objective_values <- c(objective_values, objective_value)
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
  ######################################################################################
  ##########  EVOLUTIONARY ALGORITHM ###################################################
  ######################################################################################
  ######################################################################################
  use_python("/Users/sajalkaurminhas/anaconda3/bin/python",required=T)
  source_python("Source_EA.py")
  
  #Parameters
  available_objectives <- c("accuracy", "nodes", "sensitivity", "sensibility") #ordering must be kept
  to_max <- c(TRUE, FALSE, TRUE, TRUE)
  initially_included <- c(FALSE, FALSE, FALSE, FALSE)
  PDT <- DecisionTree_EA(tournament_size = 5,
                         crossover_rate = 0.4,
                         mutation_rate = 0.6,
                         elitism_rate = 0.0,
                         hall_of_fame_size = 3)
  
  
  #Initial setup
  #use_python("C:/Users/fredx/Anaconda3",required=T) #Using python means that R sessions needs to be restarted every time or it will conflict
  
  

  
  disable("evolve")
  disable("restart_evolution")
  disable("accuracy_checkbox")
  seeded_evolution <- FALSE
  nodes_objective_index <- NULL
  accuracy_objective_index <- NULL
  max_objectives <- length(available_objectives)
  
  #Reactives
  reactive_variables <- reactiveValues()
  reactive_variables$objectives <- data.frame("Index"=0:(max_objectives-1),
                                              "Objective_name"=available_objectives,
                                              "To_max" = to_max,
                                              "Included" = initially_included,
                                              "Best_values" = vector(mode="double", length=max_objectives),
                                              "Mean_values" = vector(mode="double", length=max_objectives),
                                              "Std_devs" = vector(mode="double", length=max_objectives))
  reactive_variables$progress <- data.frame("Gen"=integer(),
                                            "Objective_name"=character(),
                                            "Type"=character(),
                                            "Value"=double(),
                                            "Low"=double(),
                                            "High"=double(),
                                            stringsAsFactors = FALSE)
  reactive_variables$pareto <- data.frame("Gen"=integer(),
                                          "Individual_index"=integer(),
                                          "Generation_of_creaton"=integer(),
                                          "Rank"=integer(),
                                          "Nodes"=integer(),
                                          "accuracy"=double(),
                                          "nodes"=double(),
                                          "sensitivity"=double(),
                                          "sensibility"=double()) #there should be a way to add the column names from the available_objectives variable
  #colnames(reactive_variables$pareto) <- c("Gen", "Individual_index", "Generation_of_creaton", "Rank", "Nodes", available_objectives)
  
  progress_values <- reactiveValues()
  progress_values$best_values <- c()
  progress_values$mean_values <- c()
  progress_values$best_tree_nodes <- c()
  progress_values$mean_nodes <- c()
  progress_values$df <- data.frame("")
  
  visibility_flags <- reactiveValues()
  visibility_flags$test <- TRUE
  
  
  
  ########################
  # Functions ############
  
  update_reactive_variables <- function(){
    current_generation <- PDT$'current_generation'
    
    #Update the individuals
    individual_counter <- 0
    for (individual in PDT$'population'){
      #individual=PDT$'population'[[1]]
      objective_values <- c()
      for (objective_value in individual$'objective_values'){
        objective_values <- c(objective_values, objective_value)
      }
      missing <- max_objectives - length(objective_values)
      filler <- rep(FALSE, missing)
      objective_values <- c(objective_values, filler)
      if (is.null(individual$'rank')){
        rank <- FALSE
      }
      else{
        rank <- individual$'rank'
      }
      row_to_add = c(c(current_generation,individual_counter, individual$'generation_of_creation',rank,length(individual$'genotype'$'get_subtree_nodes'())),objective_values)
      reactive_variables$pareto[nrow(reactive_variables$pareto) + 1,] <- row_to_add
      individual_counter <- individual_counter + 1                                                                  
    }
    
    #Update progress
    current_gen_subset <- subset(reactive_variables$pareto, Gen==current_generation)
    for (objective_name in available_objectives){
      #objective_name = "nodes"
      current_gen_objective <- subset(current_gen_subset, select = c(objective_name))
      best_value <- as.numeric(max(current_gen_objective))
      mean_value <- as.numeric(mean(current_gen_objective[,objective_name]))
      std_dev <- as.numeric(sd(current_gen_objective[,objective_name]))
      low <- mean_value - std_dev
      high <- mean_value + std_dev
      reactive_variables$progress <- rbind(reactive_variables$progress, data.frame(Gen=current_generation,Objective_name=objective_name,Type="Best",Value=best_value,Low=best_value,High=best_value))
      reactive_variables$progress <- rbind(reactive_variables$progress, data.frame(Gen=current_generation,Objective_name=objective_name,Type="Mean",Value=mean_value,Low=low,High=high))
    }  
  }
  
  
  update_inclusion_of_objectives <- function(){
    #adds and removes objectives according to their corresponding checkboxes
    reactive_variables$objectives$Included <- c(input$accuracy_checkbox, input$nodes_checkbox, input$sensitivity_checkbox, input$specificity_checkbox)
    for (row in 1:nrow(reactive_variables$objectives)) {
      if (reactive_variables$objectives[row,"Included"] == TRUE){
        PDT$'add_objective'(objective_name = reactive_variables$objectives[row, "Objective_name"], to_max = reactive_variables$objectives[row, "To_max"])
      }
      else{
        PDT$'remove_objective'(objective_name = reactive_variables$objectives[row, "Objective_name"])
      }
    }
  }
  
  
  initiate_ea <- function(forest,dataset) {
    #Set up the EA to start evolving
    
    #Create a reference to the dataset in the EA, create attribute objects for each
    PDT$'adapt_to_data'(labels = dataset$Class, data=dataset) #the labels are hardcoded to Class
    
    #Verify if the trees from R are useful (more than x splits)
    bad_trees_count=0
    for (Ctree in forest) {
      if (nrow(Ctree$frame) < 5){
        
        bad_trees_count = bad_trees_count+1
      }
      else{
        rules <- tidyRules(Ctree)
        PDT$'insert_r_tree_to_population'(rules)
      }
    }
    print(paste0("Bad trees: ", bad_trees_count))
    
    #creates as many random trees as bad trees were found to fill the population
    for (i in 1:bad_trees_count){
      random_tree = PDT$'generate_random_tree'()
      PDT$'insert_tree_to_population'(random_tree)
    }

  update_inclusion_of_objectives()
  
  nodes_objective_index <<- 1
  accuracy_objective_index <<- 0
  names <- PDT$'get_attribute_names'()
  values <- PDT$'get_crucial_values'()
  len <- sapply(values,length)
  m_l <- max(len)
  len <- m_l - len
  crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
  colnames(crucial_values) <- names
  
  return (crucial_values)
}

seed_crucial_values <- function(){
  crucial_values <- initiate_ea(forest = Main()$Trees, dataset = Main()$Train_data)
  enable("evolve")
  disable("accuracy_checkbox")
  disable("nodes_checkbox")
  disable("sensitivity_checkbox")
  disable("specificity_checkbox")
  seeded_evolution <<- TRUE
  return (crucial_values)
}

######################
# Buttons ############

observeEvent(input$evolve, {
  print(paste0("Running ",input$generations," generations"))
  withProgress(message = "Evolving",
               value = 0,
               {
                 for (g in 1:input$generations){
                   PDT$'evolve'()
                   update_reactive_variables()
                   incProgress(1/input$generations)
                 }
               })
  enable("restart_evolution")
})


observeEvent(input$seed, {
  crucial_values <- seed_crucial_values()
  output$crucial_values = renderDT(crucial_values %>% datatable(selection=list(target="cell"),
                                                                options = list(scrollX = TRUE,
                                                                               #scrolly = TRUE,
                                                                               paginate = T,
                                                                               lengthMenu = c(5, 10, 15),
                                                                               pageLength = 15,
                                                                               initComplete = JS(
                                                                                 "function(settings, json) {",
                                                                                 "$(this.api().table().header()).css({'color': '#000'});",
                                                                                 "}")
  )) %>% DT::formatStyle(columns = names(crucial_values), color="blue"))

 
})


observeEvent(input$restart_evolution, { #Needs a lot of work
  PDT$'restart_evolution'()
  crucial_values <- seed_crucial_values()
  enable("accuracy_checkbox")
  enable("nodes_checkbox")
  enable("sensitivity_checkbox")
  enable("specificity_checkbox")
  reactive_variables$all_generations <- c()
})


observeEvent(input$update_tree, {
  output$network <- renderVisNetwork({
    PDT$'evaluate_population'()
    best_tree <- PDT$'get_best_individual'(objective_index = 0)$'genotype'
    best_tree_nodes <- best_tree$'get_subtree_nodes'()
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



########################
# Outputs ##############

output$evolution_progress <- renderPlot({
    
    objective_name = "accuracy"
    obj_subset <- subset(reactive_variables$progress, Objective_name == objective_name)
    ggplot(data = obj_subset, aes(x=Gen, y=Value, color=Type))+
      geom_point() + 
      geom_line() +
      geom_ribbon(aes(ymin=Low, ymax=High), linetype=2, alpha=0.1) +
      xlab("Generation") +
      ylab("Accuracy")

})


output$evolution_progress_nodes <- renderPlot({
  
  objective_name = "nodes"
  obj_subset <- subset(reactive_variables$progress, Objective_name == objective_name)
  ggplot(data = obj_subset, aes(x=Gen, y=Value, color=Type))+
    geom_point() + 
    geom_line() +
    geom_ribbon(aes(ymin=Low, ymax=High), linetype=2, alpha=0.1) +
    xlab("Generation") +
    ylab("Nodes")
})

output$pareto_front <- renderPlot({
  current_gen<-max(reactive_variables$pareto$Gen, na.rm = TRUE)
  current_gen_pareto <- subset(reactive_variables$pareto, Gen==current_gen)
  ggplot(current_gen_pareto, aes(x=accuracy, y=nodes, color=Rank)) + 
    geom_point(size=6) +
    theme_ipsum()
})


})


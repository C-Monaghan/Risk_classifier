source("Main.R")

# Library for running shinyapp
library(shiny)

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
               "No. of Decision Trees","Max. depth of the tree"),
      Value = as.character(c(input$method,
                             input$trees,input$max_depth)),
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
  
  max_depth<-reactive({
    input$max_depth
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
    
    Classifier(original_data(),  method(), trees(), max_depth())
    
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
  
  library("reticulate")

  PYTHON_DEPENDENCIES = c('pandas','numpy')
  virtualenv_create(envname = "myenv")
  virtualenv_install(envname = "myenv", packages = PYTHON_DEPENDENCIES)
  use_virtualenv("myenv")
  source_python("Source_EA.py")
  
  #Parameters
  available_objectives <- c("accuracy", "nodes", "gini", "entropy", "max_depth") #order matters
  #Is the objective to be maximised?
  to_max <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  #Default inclusion in the ui
  initially_included <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  
  #Initial setup
  PDT <- NULL
  disable("evolve")
  disable("gini_objective")
  disable("entropy_objective")
  disable("restart_evolution")
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
                                          "gini"=double(),
                                          "entropy"=double(),
                                          "max_depth"=integer()) #there should be a way to add the column names from the available_objectives variable
  reactive_variables$crucial_values_df <- NULL
  reactive_variables$view_by_index <- FALSE
  
  
  pareto_data <- reactive({
    subset(reactive_variables$pareto, Gen==input$pareto_gen)
  })
  
  ########################   
  # Functions ############
  
  get_best_tree <- function(){
    #Returns the best tree, the first objective has priority
    best_tree <- PDT$'get_best_individual'(objective_index=0)$'genotype'
    return(best_tree)
  }
  
  update_reactive_variables <- function(){
    current_generation <- PDT$'generation'
    
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
      reactive_variables$progress <- rbind(reactive_variables$progress, data.frame(Gen=current_generation,Objective_name=objective_name,Type="Max value",Value=best_value,Low=best_value,High=best_value))
      reactive_variables$progress <- rbind(reactive_variables$progress, data.frame(Gen=current_generation,Objective_name=objective_name,Type="Population mean",Value=mean_value,Low=low,High=high))
    }  
  }
  
  
  update_inclusion_of_objectives <- function(){
    #adds and removes objectives according to their corresponding checkboxes
    reactive_variables$objectives$Included <- c(input$accuracy_objective, input$nodes_objective, input$gini_objective, input$entropy_objective, input$max_depth_objective)
    for (row in 1:nrow(reactive_variables$objectives)) {
      if (reactive_variables$objectives[row,"Included"] == TRUE){
        PDT$'add_objective'(objective_name = reactive_variables$objectives[row, "Objective_name"], to_max = reactive_variables$objectives[row, "To_max"])
      }
      else{
        PDT$'remove_objective'(objective_name = reactive_variables$objectives[row, "Objective_name"])
      }
    }
  }
  
  
  initiate_gp <- function(forest,dataset) {
    #Set up the EA to start evolving
    PDT <<- DecisionTree_EA(tournament_size = input$tournament_size,
                    crossover_rate = input$crossover_rate,
                    mutation_rate = input$mutation_rate,
                    elitism_rate = input$elitism_rate,
                    #hall_of_fame_size = input$elitism_rate,
                    max_depth = input$max_depth_value,
                    max_nodes = input$max_nodes_value,
                    population_size = input$population_size,
                    forced_full = input$forced_full
                    )
    
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
    update_crucial_values()
    plot_crucial_values()
  }
  
  update_crucial_values <- function(){
    names <- PDT$'get_attribute_names'()
    values <- PDT$'get_crucial_values'()
    len <- sapply(values,length)
    m_l <- max(len)
    len <- m_l - len
    crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
    colnames(crucial_values) <- names
    reactive_variables$crucial_values_df <<- crucial_values
  }
  
  update_default_rates <- function(){
    objectives = 0
    if (input$accuracy_objective == TRUE) objectives <- objectives + 1
    if (input$entropy_objective == TRUE) objectives <- objectives + 1
    if (input$gini_objective == TRUE) objectives <- objectives + 1
    if (input$nodes_objective == TRUE) objectives <- objectives + 1
    if (input$max_depth_objective == TRUE) objectives <- objectives + 1
    if (objectives == 1){
      if (input$forced_full == TRUE){
        updateNumericInput(session, "mutation_rate", value = 0.3) #Full subtree mutation
        updateNumericInput(session, "crossover_rate", value = 0.6)
        updateNumericInput(session, "elitism_rate", value = 0.1)
        updateNumericInput(session, "posterior_mutation_probability", value = 0.05)
      } else{
        updateNumericInput(session, "mutation_rate", value = 0.5) #Subtree mutation
        updateNumericInput(session, "crossover_rate", value = 0.4)
        updateNumericInput(session, "elitism_rate", value = 0.1)
        updateNumericInput(session, "posterior_mutation_probability", value = 0)
      }
    } else{
      updateNumericInput(session, "mutation_rate", value = 0.6)
      updateNumericInput(session, "crossover_rate", value = 0.4)
      updateNumericInput(session, "elitism_rate", value = 0.0)
      updateNumericInput(session, "posterior_mutation_probability", value = 0)
    }
  }
  
  grant_sum_1 <- function(changed){
    if (changed == "mutation_rate"){
      mut_rate <- input$mutation_rate
      cross_rate <- 1 - mut_rate - input$elitism_rate
      elit_rate <- input$elitism_rate
      if (cross_rate < 0){
        elit_rate <- elit_rate + cross_rate
        cross_rate <- 0
      }
    }
    if (changed == "crossover_rate"){
      cross_rate <- input$crossover_rate
      mut_rate <- 1 - cross_rate - input$elitism_rate
      elit_rate <- input$elitism_rate
      if (mut_rate < 0){
        elit_rate <- elit_rate + mut_rate
        mut_rate <- 0
      }
    }
    if (changed == "elitism_rate"){
      elit_rate <- input$elitism_rate
      cross_rate <- 1 - elit_rate - input$mutation_rate
      mut_rate <- input$mutation_rate
      if (cross_rate < 0){
        mut_rate <- mut_rate + cross_rate
        cross_rate <- 0
      }
    }
    updateNumericInput(session, "mutation_rate", value = mut_rate)
    updateNumericInput(session, "crossover_rate", value = cross_rate)
    updateNumericInput(session, "elitism_rate", value = elit_rate)
  }
  
  toggle_objective_checkboxes <- function(){
    if ((input$accuracy_objective == FALSE) & (input$gini_objective == FALSE) & (input$entropy_objective == FALSE)){
      enable("accuracy_objective")
      enable("gini_objective")
      enable("entropy_objective")
    } else{
      if (input$accuracy_objective == TRUE){
        disable("gini_objective")
        disable("entropy_objective")
      }
      if (input$gini_objective == TRUE){
        disable("accuracy_objective")
        disable("entropy_objective")
      }
      if (input$entropy_objective == TRUE){
        disable("gini_objective")
        disable("accuracy_objective")
      }
    }
  }
  
  #~~~~ Download the network decision tree plot
  output$net<-downloadHandler(
    #Specify filename
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      mynetwork() %>% visSave(con)
    }
  )
  
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
  
  
  observeEvent(input$remove_split,{
    selection <- input$crucial_values_cells_selected
    values <- reactive_variables$crucial_values_df[selection]
    attribute_indexes <- selection[,2]
    all_attribute_names <- colnames(reactive_variables$crucial_values_df)
    attribute_names <- all_attribute_names[attribute_indexes]
    print(paste0("values", values))
    print(paste0("attribute_names", attribute_names))
    for(i in 1:nrow(selection)){
      value <- as.numeric(values[i])
      attribute_name <- attribute_names[i]
      PDT$'remove_crucial_value'(attribute_name=attribute_name,value=value)
    }
    update_crucial_values()
    plot_crucial_values()
  })
  
  
  observeEvent(input$initiate_gp, {
    initiate_gp(forest = Main()$Trees, dataset = Main()$Train_data)
    enable("evolve")
    isolate({
      updateTabsetPanel(session, "EA_tabs",
                        selected = "splits")
    })
  })
  
  
  observeEvent(input$restart_evolution, { #Needs a lot of work
    PDT$'restart_evolution'()
    initiate_gp(forest = Main()$Trees, dataset = Main()$Train_data)
  })
  
  
 mynetwork<-reactive({ visual_tree() })
      
  
  observeEvent(input$update_tree, {
    output$network <- renderVisNetwork({
      mynetwork()
    })
  })
    
  observeEvent(input$max_nodes_enabled, {toggle("max_nodes_value")})
  
  observeEvent(input$max_depth_enabled, {toggle("max_depth_value")})
  
  observeEvent(input$splits_done, { 
    PDT$'initial_setup'()
    isolate({
      updateTabsetPanel(session, "EA_tabs",
                        selected = "evolve")
      })
    })
  
  observeEvent(input$accuracy_objective, {
    update_default_rates()
    toggle_objective_checkboxes()
    })
  
  observeEvent(input$entropy_objective, {
    update_default_rates()
    toggle_objective_checkboxes()
  })
  
  observeEvent(input$gini_objective, {
    update_default_rates()
    toggle_objective_checkboxes()
  })
  
  observeEvent(input$nodes_objective, {
    update_default_rates()
    updateCheckboxInput(session, "forced_full", value = FALSE)
    })
  
  observeEvent(input$max_depth_objective, {
    update_default_rates()
    updateCheckboxInput(session, "forced_full", value = FALSE)
    })
  
  observeEvent(input$population_size, {updateNumericInput(session, "tournament_size", max = input$population_size - 1)})
  
  observeEvent(input$save_tree_python, {PDT$'save_tree'(get_best_tree())})
  
  observeEvent(input$forced_full, {
    update_default_rates()
    updateCheckboxInput(session, "max_depth_enabled", value = TRUE)
    updateCheckboxInput(session, "max_depth_objective", value = FALSE)
    updateCheckboxInput(session, "nodes_objective", value = FALSE)
    })
  
  observeEvent(input$mutation_rate, {grant_sum_1(changed="mutation_rate")})
  
  observeEvent(input$crossover_rate, {grant_sum_1(changed="crossover_rate")})
  
  observeEvent(input$elitism_rate, {grant_sum_1(changed="elitism_rate")})
  
  observeEvent(input$index_tree, {
    output$network <- renderVisNetwork({visual_tree(index=input$tree_index)})
  })
  
  
  ########################
  # Outputs ##############
  
  visual_tree <- function(index=NULL){
    PDT$'evaluate_population'()
    if (is.null(index)){
      best_tree <- get_best_tree()
    }
    else{
      #ind_index <- input$tree_index+1
      ind_index <- index
      print(paste("by index: ", ind_index))
      best_tree <- PDT$'get_tree_by_individual_index'(ind_index)
    }
    test_values <- PDT$'get_test_values'(test_data=Main()$Test_data, 
                                    test_labels=Main()$Test_data$Class,
                                    individual_index = input$tree_index)
    train_values <- PDT$'get_train_values'(individual_index = input$tree_index)
    generation_of_creation <- PDT$'population'[[input$tree_index+1]]$'generation_of_creation'
    df <- data.frame("Name"=character(),
                     "Value"=double(),
                     stringsAsFactors = FALSE)
    df[nrow(df) + 1,] <- c("Index",input$tree_index)
    df[nrow(df) + 1,] <- c("Created on gen",generation_of_creation)
    df[nrow(df) + 1,] <- c("Train accuracy",train_values[[1]])
    df[nrow(df) + 1,] <- c("Test accuracy",test_values[[1]])
    df[nrow(df) + 1,] <- c("Train entropy",train_values[[2]])
    df[nrow(df) + 1,] <- c("Test entropy",test_values[[2]])
    df[nrow(df) + 1,] <- c("Train gini index",train_values[[3]])
    df[nrow(df) + 1,] <- c("Test gini index",test_values[[3]])
    plot_tree_values(df)
    
    
    best_tree_nodes <- best_tree$'get_subtree_nodes'()
    connections <- best_tree$'get_connections'()
    connections
    c_from <- connections[[1]]
    c_to <- connections[[2]]
    c_color <- connections[[3]]
    new_edges <- data.frame(from = c_from, to=c_to, color=c_color)
    new_df <- data.frame(id=c(),label=c(), shape=c(), level=c())
    i=0
    for (node in best_tree_nodes){
      level = node$'get_my_depth'()
      label = (toString(node))
      color="lightblue"
      font_color = "black"
      if (node$'is_terminal'() == TRUE){
        shape="box"
        color="lightgreen"
      }
      else{
        if (node$'is_root'()){
          shape="box"
          #color="lightgreen"
        }
        else{
          shape="box"
        }
      }
      
      new_df <- rbind(new_df, data.frame(id=i,
                                         label=label, 
                                         shape=shape,
                                         level = level,
                                         color=color,
                                         font.color = font_color))
      i=i+1
    }
    #new_df
    net<-visNetwork(new_df, new_edges, height = "500px", width = "100%") %>% 
      visEdges(arrows = "from") %>% 
      visHierarchicalLayout()
    return (net)
  }
  
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
    
    #print(paste("filtered_indexes",filtered_indexes))
    #current_gen<-max(reactive_variables$pareto$Gen, na.rm = TRUE)
    #current_gen_pareto <- subset(reactive_variables$pareto, Gen==current_gen)
    current_gen_pareto <- pareto_data()
    value_at_gen1 <- max(select(subset(reactive_variables$pareto, Gen==1),accuracy))
    obj_names <- PDT$'get_objective_names'()
    filtered_indexes <- PDT$'get_filtered_individual_indexes'()
    #if (length(filtered_indexes)!=length(current_gen_pareto)){
    #  filtered_indexes <- c("0")
    #}
    ggplot(current_gen_pareto, aes(x=accuracy, y=nodes, color=Rank)) + #CHANGE HARDCODED MISSING
      geom_point(size=6) +
      #theme_ipsum() + 
      #geom_label_repel(aes(label = Individual_index),
      geom_label_repel(aes(label = filtered_indexes),                 
                                box.padding   = 0.35, 
                                point.padding = 0.5,
                                segment.color = 'grey50') +
      ggtitle(paste("Population in generation ", PDT$'generation' ) ) +
      xlab(obj_names[1]) +
      ylab(obj_names[2]) + 
      geom_vline(xintercept = value_at_gen1, linetype="dotted", 
                                      color = "blue", size=1)#+
      #grids(linetype = "dashed") +
      #theme_classic()
    
    #text(dist ~speed, labels=rownames(cars),data=cars, cex=0.9, font=2)
  })
  
  plot_crucial_values <- function(){
  output$crucial_values <- renderDT({reactive_variables$crucial_values_df %>% datatable(selection=list(target="cell"),
                                    options = list(scrollX = TRUE,
                                                   #scrolly = TRUE,
                                                   paginate = T,
                                                   lengthMenu = c(5, 10, 15),
                                                   pageLength = 15,
                                                   initComplete = JS(
                                                     "function(settings, json) {",
                                                     "$(this.api().table().header()).css({'color': '#000'});","}")
                                    )) %>% DT::formatStyle(columns = names(reactive_variables$crucial_values_df), color="blue")
  })}
  
  plot_tree_values <- function(index_tree_values_df){
  output$tree_values <- renderDT({index_tree_values_df})}
  
  
})


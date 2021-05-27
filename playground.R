source("Main.R")
library(reticulate)

# Loading the dataset
load("GermanCredit.Rdata")
data<-GermanCredit

C <- Classifier(data, choose_regression = "Ridge Regression", 100)
forest <- C$Trees
dataset <- C$Train_data
rpart.plot(forest[[1]])

use_virtualenv("temp_env")
reticulate::source_python("Source_EA.py")

crucial_values_df <- NULL

update_inclusion_of_objectives <- function(){
  PDT$'add_objective'(objective_name = "entropy", to_max = FALSE)
  #PDT$'add_objective'(objective_name = "max_depth", to_max = FALSE)
  PDT$'add_objective'(objective_name = "nodes", to_max = FALSE)
}

view_tree <- function(individual=NULL, best_tree = NULL){
  if (is.null(best_tree)){best_tree <- individual$'genotype'}
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
        color="lightgreen"
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
  net
}

PDT <<- DecisionTree_EA(tournament_size = 5,
                        crossover_rate = 0.4,
                        mutation_rate = 0.5,
                        elitism_rate = 0.1,
                        #hall_of_fame_size = 5,
                        max_depth = 6,
                        population_size = 100,
                        uniform_mutation_rate = 0,
                        forced_full = FALSE
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

show_dups <- function(){
  dups <- 0
  for (ind in PDT$population){
    if(ind$duplicated==TRUE){
      dups <- dups+1}
  }
  print(paste0("dups: ",dups))
}
show_objective_values <- function(pop = NULL){
  if (is.null(pop)){
    pop = PDT$population
  }
  for (ind in pop){
    print(paste0(ind$objective_values))
  }
}
show_depths <- function(){
  for (ind in PDT$population){
    print(paste("Max",ind$'genotype'$'get_max_depth'(),ind$'max_depth',"Min",ind$'genotype'$'get_min_depth'()))
  }
}
show_pareto <- function(){
  #print(paste("filtered_indexes",filtered_indexes))
  current_gen<-max(reactive_variables$pareto$Gen, na.rm = TRUE)
  current_gen_pareto <- subset(reactive_variables$pareto, Gen==current_gen)
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
    ylab(obj_names[2]) #+
  #grids(linetype = "dashed") +
  #theme_classic()
}
    update_inclusion_of_objectives()

names <- PDT$'get_attribute_names'()
values <- PDT$'get_crucial_values'()
len <- sapply(values,length)
m_l <- max(len)
len <- m_l - len
crucial_values <- data.frame(mapply( function(x,y) c( x , rep( NA , y ) ) , values , len ))
colnames(crucial_values) <- names
crucial_values_df <<- crucial_values


PDT$'_ensure_population_quality'()
#PDT$'evaluate_population'()
#PDT$'_fast_nondominated_sort'()
length(PDT$population)
PDT$generation
show_depths()
PDT$'evolve'()
PDT$'evolve'()
PDT$'evolve'()
show_dups()
show_objective_values()
best_ind <- PDT$'get_best_individual'(objective_index=0)
best_ind <- PDT$'population'[[2]]
new_ind <- PDT$generate_random_individual()
best_ind$generation_of_creation
best_ind$genotype$get_max_depth()
best_ind$genotype$get_min_depth()
PDT$meets_constraints(best_ind)
PDT$calculate_max_depth(best_ind)
best_ind$genotype$get_min_depth()
view_tree(best_ind)
view_tree(new_ind)
PDT$forced_full
best_ind$meets_constraints
best_ind$meets_constraints <- NULL
show_objective_values(PDT$"_sort_individuals"())
sorted_ind <- PDT$'_multiobjective_sort_individuals'()
for (ind in sorted_ind){
  print(paste0(ind$rank, " ", ind$crowding_distance, " ", ind$objective_values))
}



best_ind <- PDT$'get_best_individual'(objective_index=0)
best_ind <-  PDT$'population'[[86]]
best_ind$'objective_values'
view_tree(best_ind)
outputs <- PDT$'_shrink_useless_nodes'(best_ind$'genotype')
outputs[[1]]
PDT$'reduce_and_update'(best_ind)
view_tree(best_tree=best_ind$'genotype')
view_tree(best_ind)
for (x in 1:5){
  count=0
  for (ind in PDT$population){
    outputs <- PDT$'_shrink_useless_nodes'(ind$'genotype')
    if (outputs[[1]] == TRUE){
      count <- count+1
    }
  }
  print(count)
}

#best_ind$'genotype'$'reset_numbers'()
mutated_ind <- PDT$'_uniform_mutation'(best_ind, probability=0.9)
mutated_ind$'genotype'
length(best_ind$'genotype'$'get_subtree_nodes'())
length(mutated_ind$'genotype'$'get_subtree_nodes'())

mutated_ind2 <- PDT$'_uniform_mutation'(PDT$'population'[[2]], probability=0.3)
view_tree(best_ind)
view_tree(mutated_ind)
view_tree(mutated_ind2)
length(mutated_ind$'get_subtree_nodes'())
best_tree <- best_tree$'clean_and_reduce'()
best_ind$'genotype'$'children'[[1]]
old_node <- best_ind$'genotype'$'children'[[1]]
old_node$'get_path'()
test_node <- PDT$'generate_random_node_safe'(old_node=old_node)
path
test_node
PDT$'get_valid_attributes'()
view_tree(PDT$'population'[[86]])
reduced = PDT$'shrink_useless_nodes'(PDT$'population'[[86]]$'genotype')
view_tree(tree=reduced)

x = data.frame("a"=c(1,2,3),"b"=c(4,5,6))
max(select(x,a))

x[1,2]
colnames(x)[[1]]

test_tree <- PDT$generate_random_tree(method="Full",max_depth=3)
view_tree(best_tree=test_tree)
test_tree$get_max_depth()
nodes <- test_tree$get_subtree_nodes()
length(nodes)
path <- nodes[[15]]$get_path()
length(path)
best_ind <-  PDT$'population'[[86]]
view_tree(best_ind)
mutated <- PDT$'_subtree_mutation'(best_ind)
view_tree(mutated)

m <- matrix(1:9, nrow = 3, ncol = 3)
s <- m[,2]
s
for (r in 1:nrow(m)){
  print(r)
}
print(m)
typeof(m)
x <- c(1,2,4,8,9,7,6,4)
c<-x[s]
c

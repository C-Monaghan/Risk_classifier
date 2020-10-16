library(party)
library(rpart.plot)
library(glmnet)
library(rpart)
library(rattle)
library(RcppAlgos)
library(tidyverse)
library(caret)
library(leaps)
library("reticulate")
library("tidyrules")
library("dplyr")
library("pander")
library(ROCR)
#library(tidyrules)
require(caTools)
library(rlist)

# Loading the dataset
load("GermanCredit.Rdata")
default_data<-GermanCredit
#use_python("C:/Users/fredx/Anaconda3",required=T)
#source_python("Source_EA.py")

# tree.size <- function(tree) {
#   if (is.null(tree)) {
#     return(0)
#   } else {
#     return(1 + tree.size(tree$left) + tree.size(tree$right))
#   }
# }

# The main function
Classifier<-function(default_data,choose_regression = TRUE,selection=100){
  
  if(length(unique(na.omit(default_data[,1]))) <= 2L){
    
  # Cleaning the data before using
  default_data<-na.omit(default_data)
  x=model.matrix(default_data[,1]~.,default_data[,-1])[,-1]
  y=default_data[,1]
  
  ###################### Applying regressions
  
  if (choose_regression==0) {
    
    # ~~~~~ Ridge Regression
    cv.out_ridge=cv.glmnet(x,y,alpha=0,family = "binomial")
    bestlam_ridge=cv.out_ridge$lambda.min
    tmp_coef_ridge = coef(cv.out_ridge,s=bestlam_ridge)
    tmp_coef_ridge[tmp_coef_ridge!=0]
    
    # Reduced dataset
    default_data<-as.data.frame(x[, tmp_coef_ridge@i[-1]])
  }
  else {
    
    # ~~~~~ Lasso Regression
    cv.out_lasso=cv.glmnet(x,y,alpha=1,family = "binomial")
    bestlam_lasso=cv.out_lasso$lambda.min
    tmp_coef_lasso = coef(cv.out_lasso,s=bestlam_lasso)
    tmp_coef_lasso[tmp_coef_lasso!=0]
    
    # Reduced dataset
    default_data<-as.data.frame(x[, tmp_coef_lasso@i[-1]])
  }
  
  if (ncol(default_data)>25) { #not needed
    # Fit the full model 
    default_data<-data.frame(Class=y,default_data)
    x=model.matrix(default_data[,1]~.,default_data[,-1])[,-1]
    y=default_data[,1]
    
    # Stepwise regression model
    biggest <- formula(glm(default_data[,1]~.,default_data[,-1], family = "binomial"))
    fwd.model<-step(glm(default_data[,1]~1, data=default_data[,-1],family = "binomial"),
                    direction = "forward", scope = biggest,trace = 0)
    
    # Reduced dataset
    default_data<-as.data.frame(x[,names(fwd.model$coefficients[-1])])
    default_data<-data.frame(Class=y,default_data)
    
  }
  else {
    return(default_data)
  }
  
  # Changing the variable to binary which are stored as numeric
  #default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE]<-lapply(default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE],factor)
  
  ###################### Decision tree

  # You get the names of the columns
  Cols <- names(default_data)
  Cols <- Cols[! Cols %in% "Class"]
  n <- length(Cols)
  
  # You construct all possible combinations
  id <- unlist( lapply(1:n,function(i)combn(1:n,i,simplify=FALSE)) ,
                recursive=FALSE)
  
  id<-sample(id, 1000, replace=FALSE) 
  id<-lapply(id, function(x) list.remove(x,length(x)<2))
  id<-list.clean(id, function(x) length(x) == 0L, TRUE)
 
  
  # You paste them to formulas
  Formulas <- sapply(id,function(i)
    paste("Class~",paste(Cols[i],collapse="+")))
  
  #~~~~~~~~ Dividing the data into testing and training
  
  ## set the seed to make your partition reproducible
  set.seed(1) 
  
  ## 75% of the sample size
  sample = sample.split(default_data$Class, SplitRatio = .70)
  train = subset(default_data, sample == TRUE)
  test  = subset(default_data, sample == FALSE)
  
  # Storing all the combination of trees
  Forest = list()
  for(i in 1:selection) { #CHANGE: this is not random
    RPI = rpart(Formulas[[i]],data= train,method = "class", model=TRUE, y=TRUE)
    Forest[[i]] = RPI
  }
  
  #~~~~~~~~  TESTING PERFORMANCE
  
  # Predicting the performance of the trees
  pred = list()
  for(i in 1:selection) {
    RP<-predict(Forest[i],type="class",newdata=test)
    pred[[i]] = RP
  }
  
  prob = list()
  for(i in 1:selection){
    RP<-predict(Forest[i],type="prob",newdata=test)
    prob[[i]] = RP
  }
  
  forest_pred = list()
  for(i in 1:selection) {
    RP<-prediction(prob[[i]][[1]][,2],test[,1])
    forest_pred[[i]] = RP
  }
  
  # Performance of each trees
  forest_perf = list()
  for(i in 1:selection) {
    RP<-performance(forest_pred[[i]],"tpr","fpr")
    forest_perf[[i]] = RP
  }
  
  # AUROC of each trees
  forest_AUROC = list()
  for(i in 1:selection) {
    RP <- round(performance(forest_pred[[i]], measure = "auc")@y.values[[1]]*100, 2)
    forest_AUROC[[i]] = RP
  }
  
  # Gini Index of each trees
  forest_Gini = list()
  for(i in 1:selection) {
    RP<- (2*forest_AUROC[[i]] - 100)
    forest_Gini[[i]] = RP
  }
  
  # Making the table of the performance of the trees
  tab= list()
  for(i in 1:selection) {
    RP<-table(test[,1],pred[[i]][[1]])
    tab[[i]] = RP
  }
  
  # Accuracy of the treess
  acc= list()
  for(i in 1:selection) {
    RP<-sum(diag(tab[[i]]))/sum(tab[[i]])
    acc[[i]] = RP
  }
  
  # Unlisting the trees
  acc<-unlist(acc, use.names=FALSE)
  forest_Gini<-unlist(forest_Gini, use.names=FALSE)
  forest_AUROC<-unlist(forest_AUROC, use.names=FALSE)
  
  return(list(Reduced_data=default_data, Test_data=test, Train_data=train, Trees=Forest, Accuracy=acc, Model_Performance=forest_perf, AUROC=forest_AUROC, Gini_Index= forest_Gini)) 
  }
  else NULL
  }

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
  for (i in 1:bad_trees_count){
    PDT$'generate_random_tree'()
  }
}

# 
# initiate_ea <- function(Forest, tournament_size = 3, crossover_rate = 0.5, mutation_rate = 0.4) {
#   use_python("C:/Users/fredx/Anaconda3",required=T)
#   source_python("Source_EA.py") #temporal, for debugging
#   PDT <- DecisionTree_EA(tournament_size = tournament_size,
#                          crossover_rate = crossover_rate,
#                          mutation_rate = mutation_rate,
#                          elitism_rate = 0.1, 
#                          hall_of_fame_size = 3)
#   PDT$'adapt_to_data'(labels = C$Reduced_data$Class, data=C$Reduced_data)
#   initiate_population(Forest)
# }
# 
# evolve <- function(generations){
#   winner <- PDT$evolve(generations)
#   return(winner)
# }
# 
# testf <- function(){
#    na <- test_get_names()
#    return(na)
# }

# a<-Classifier(default_data,1,20)
# rpart.plot(a$Trees[[1]])
# a$Accuracy[1]
# a$Gini_Index[[1]]
# a$AUROC[[1]]
# a$Model_Performance[[1]]
# colnames(a$Reduced_data)
#summary(a$AUROC)
# summary(a$Gini_Index)


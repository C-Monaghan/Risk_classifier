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
library(caret)
library(ROCR)
library(tidyrules)
require(caTools)

# Loading the dataset
load("GermanCredit.Rdata")
GermanCredit<-GermanCredit[,c(10,1:9,11:62)]
default_data <- GermanCredit

# The main function
Classifier<-function(default_data,choose_regression = TRUE,selection=100){
  
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
  
  if (ncol(default_data)>25) {
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
  
  ###################### Decision tree
  
  # You get the names of the columns
  Cols <- names(default_data)
  Cols <- Cols[! Cols %in% "Class"]
  n <- length(Cols)
  selection=1000
  
  # You construct all possible combinations
  id <- unlist( lapply(1:n,function(i)combn(1:n,i,simplify=FALSE)) ,
                recursive=FALSE)
  id<-sample(id, selection, replace=FALSE) 
  
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
  for(i in 1:selection) {
    RPI = rpart(Formulas[[i]],data= train,method = "class")
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
  
  forest_perf = list()
  for(i in 1:selection) {
    RP<-performance(forest_pred[[i]],"tpr","fpr")
    forest_perf[[i]] = RP
  }
  
  forest_AUROC = list()
  for(i in 1:selection) {
    RP <- round(performance(forest_pred[[i]], measure = "auc")@y.values[[1]]*100, 2)
    forest_AUROC[[i]] = RP
  }
  
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



# a<-Classifier(default_data,1,200)
# rpart.plot(a$Trees[[1]])
# a$Accuracy[1]
# a$Gini_Index[[1]]
# a$AUROC[[1]]
# a$Model_Performance[[1]]
# colnames(a$Reduced_data)
#summary(a$AUROC)
# summary(a$Gini_Index)

#interpretation
C<-Classifier(default_data,1,100)
use_python("C:/Users/fredx/Anaconda3",required=T)

for (Ctree in unlist(C$Trees)) {
  rules <- tidyRules(unlist(C$Trees))
}

source_python("Source_EA.py")
#Creates the Python class
PDT <- DecisionTree_EA(tournament_size = 3, crossover_rate = 0.5, mutation_rate = 0.4, elitism_rate = 0.1, hall_of_fame_size = 3)
#Gives the data to python, python can relate now to the attributes and output_labels
PDT$'adapt_to_data'(labels = C$Reduced_data$Class, data=C$Reduced_data)
#Initialisation of the population with trees from R:
for (Ctree in C$Trees) {
  rules <- tidyRules(Ctree)
  PDT$'insert_r_tree_to_population'(rules)
}
#Logs: print the poll of crucial values for each attribute
for (att in PDT$'attributes'){
  print(att$'name')
  print(att$'crucial_values')
}
#Genetic operators test
PDT$'evaluate_population'()
ind1 = PDT$'tournament_selection'()
print(ind1$'genotype')
ind2 = PDT$'tournament_selection'()
print(ind2$'genotype')
crossovers = PDT$'one_point_crossover'(ind1,ind2)
t3 = ind2$'genotype'$'copy'()
print(t3)
ind4 = PDT$'mutate'(ind1)
print(ind4$'genotype')
ev_t1 = PDT$'evaluate_tree'(ind1$'genotype')
print(ev_t1)
ev_t2 = PDT$'evaluate_tree'(ind2$'genotype')
print(ev_t2)
ev_t3 = PDT$'evaluate_tree'(t3)
print(ev_t3)
ev_t4 = PDT$'evaluate_tree'(ind4$'genotype')
print(ev_t4)
#print(ind4$'genotype'$'visits_count')

#Evolution
winner <- PDT$evolve(30)
print(winner$'genotype')

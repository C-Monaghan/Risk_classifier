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

# Loading the dataset
load("GermanCredit.Rdata")
GermanCredit<-GermanCredit[,c(10,1:9,11:62)]
default_data <- GermanCredit

# The main function
Classifier<-function(default_data,choose_regression = TRUE,selection=1000) {
  
  # Cleaning the data before using
  default_data<-na.omit(default_data)
  x=model.matrix(default_data[,1]~.,default_data[,-1])[,-1]
  y=default_data[,1]
  
  set.seed(1)
  
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
  
  
  # You construct all possible combinations
  id <- unlist( lapply(1:n,function(i)combn(1:n,i,simplify=FALSE)) ,
                recursive=FALSE)
  id<-sample(id, selection, replace=FALSE)
  
  # You paste them to formulas
  Formulas <- sapply(id,function(i)
    paste("Class~",paste(Cols[i],collapse="+")))
  
  # Storing all the combination of trees
  Forest = list()
  for(i in 1:selection) {
    RPI = rpart(Formulas[[i]],data=default_data,method = "class")
    Forest[[i]] = RPI
  }
  
  #~~~~~~~~  TESTING PERFORMANCE
  pred = list()
  for(i in 1:selection) {
    RPI<-predict(Forest[i],type="class")
    pred[[i]] = RPI
  }
  
  tab= list()
  for(i in 1:selection) {
    RP<-table(default_data[,1],pred[[i]][[1]])
    tab[[i]] = RP
  }
  
  res= list()
  for(i in 1:selection) {
    RP<-sum(diag(tab[[i]]))/sum(tab[[i]])
    res[[i]] = RP
  }
  
  res<-unlist(res, use.names=FALSE)
  
  return(list(Data=dim(default_data), Trees=Forest,Performance=res))
}

# a<-Classifier(default_data,1,500)
# rpart.plot(a$Trees[[500]])



#interpretation
C<-Classifier(default_data,1,500)

rules_r <- tidyRules(fit.r)
print(rules_r)
print(rules_r[2])

use_python("C:/Users/fredx/Anaconda3",required=T)
source_python("Source_EA.py")
PDT <- DecisionTree_EA()
PDT$'adapt_to_data'(labels = GermanCredit$Class, data=GermanCredit)
tree <- PDT$'parse_tree_r'(rules_r)
#tree$'evaluate'(GermanCredit[2,])
porc <- PDT$'evaluate_tree'(tree)
porc  
PDT$'one_point_crossover'(tree,tree)

sample_tree <- list()
sample_tree[[1]] <- c("Duration",">","11")
sample_tree[[2]] <- c("Amount","<=","900") 
sample_tree[[3]] <- c("Age",">","21") 
sample_tree[[4]] <- c("Duration",">","22") 
sample_tree[[5]] <- c("Amount",">","11") 
sample_tree[[6]] <- c("Account.withus","<=","0.5") 
sample_tree[[7]] <- c("Account.for_car",">","0.5") 

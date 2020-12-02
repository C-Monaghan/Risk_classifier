# Loading Libraries
library(rpart.plot)
library(glmnet)
library(rpart)
library(caret)
library("reticulate")
library("tidyrules")
library("dplyr")
library(ROCR)
require(caTools)
library(DT)
library(visNetwork)
library(ggplot2)
library(arsenal)
library(hrbrthemes)
library(ggrepel)

# Loading the dataset
load("GermanCredit.Rdata")
default_data<-GermanCredit

 tree.size <- function(tree) {
   if (is.null(tree)) {
     return(0)
   } else {
     return(1 + tree.size(tree$left) + tree.size(tree$right))
   }
 }

# The main function
Classifier<-function(default_data,choose_regression = "Ridge Regression",selection=100,max_depth = 20){
  
  # For removing warnings
  options(warn=-1)
  
  # Omitting missing data 
  default_data<-na.omit(default_data)
  
  # Storing the class labels
  y=default_data[,1]
  
  # Choosing the type of regression
  if (choose_regression=="Ridge Regression") {
    
    # Cleaning the data before using
    default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE]<-lapply(default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE],factor)
    
    # Removing those numerical variables which have correlation between them
    y=default_data[,1]
    res<-cor(default_data[sapply(default_data, is.numeric)])
    default_data <- default_data[,!apply(res,2,function(x) any(x > 0.70|| x< -.70))]
    
    fac<-sapply(default_data , is.factor)
    unclass_fac<-sapply(default_data[,fac], unclass)
    default_data<-cbind(default_data[,!fac],unclass_fac)
    
    default_data<-data.frame(Class=y,default_data)
    
    ###################### Applying regressions
    
    # Making model.matrix by expanding factors to a set of dummy variables
    x=model.matrix(default_data[,1]~.,default_data[,-1])[,-1]
    y=default_data[,1]
    
    # ~~~~~ Ridge Regression
    cv.out_ridge=cv.glmnet(x,y,alpha=0,family = "binomial")
    bestlam_ridge=cv.out_ridge$lambda.min
    tmp_coef_ridge = coef(cv.out_ridge,s=bestlam_ridge)
    tmp_coef_ridge[tmp_coef_ridge!=0]
    
    # Reduced dataset
    default_data<-as.data.frame(x[, tmp_coef_ridge@i[-1]])
    default_data<-data.frame(Class=y,default_data)
  }
  else if(choose_regression=="Lasso Regression")  {
    
    # Cleaning the data before using
    default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE]<-lapply(default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE],factor)
    
    # Removing those numerical variables which have correlation between them
    y=default_data[,1]
    res<-cor(default_data[sapply(default_data, is.numeric)])
    default_data <- default_data[,!apply(res,2,function(x) any(x > 0.70|| x< -.70))]
    
    fac<-sapply(default_data , is.factor)
    unclass_fac<-sapply(default_data[,fac], unclass)
    default_data<-cbind(default_data[,!fac],unclass_fac)
    
    default_data<-data.frame(Class=y,default_data)
    
    ###################### Applying regressions
    
    # Making model.matrix by expanding factors to a set of dummy variables
    x=model.matrix(default_data[,1]~.,default_data[,-1])[,-1]
    y=default_data[,1]
    
    # ~~~~~ Lasso Regression
    cv.out_lasso=cv.glmnet(x,y,alpha=1,family = "binomial")
    bestlam_lasso=cv.out_lasso$lambda.min
    tmp_coef_lasso = coef(cv.out_lasso,s=bestlam_lasso)
    tmp_coef_lasso[tmp_coef_lasso!=0]
    
    # Reduced dataset
    default_data<-as.data.frame(x[, tmp_coef_lasso@i[-1]])
    default_data<-data.frame(Class=y,default_data)
  }
  else {
     
    # If no regression is selected it will return the original dataset
    default_data
  }
  
  default_data[,1]<-as.factor(default_data[,1])
  
  # Changing the variable to binary which are stored as numeric
  default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE]<-lapply(default_data[,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE],factor)
  nums <- default_data[sapply(default_data,is.numeric)]
  x<- default_data[ ,sapply(default_data, function(x) length(unique(na.omit(x))) <= 2L)==TRUE]
  xx<-ifelse(x[,-1] == "1", 0, 1)
  default_data<-data.frame(Class=y,nums,xx)
  
  
  # Checking the labels and changing them into Good and Bad.
  if(levels(default_data[,1])==c("FALSE","TRUE")){
    levels(default_data[,1])[levels(default_data[,1])=="FALSE"] <- "Bad"
    levels(default_data[,1])[levels(default_data[,1])=="TRUE"] <- "Good"
    
    
  }else if(levels(default_data[,1])==c("0","1")){
    levels(default_data[,1])[levels(default_data[,1])=="0"] <- "Bad"
    levels(default_data[,1])[levels(default_data[,1])=="1"] <- "Good"
    
  }else{
    default_data
  }
  
  
  ###################### Decision tree
  
  # You get the names of the columns
  Cols <- names(default_data)
  Cols <- Cols[! Cols %in% "Class"]
  n <- length(Cols)
  
  # Making combination of columns/variable names with minimum 3 variables
  minimum_columns <- 3
  id <- lapply(1:(selection),function(i)sample(seq(n),sample(seq(minimum_columns,n))))
  id[[1]] <- seq(n)
  
  
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
  for(i in 1:length(Formulas)) { #CHANGE: this is not random
    RPI = rpart(Formulas[[i]],data= train,method = "class", model=TRUE, y=TRUE,control=rpart.control( maxdepth = max_depth, cp=0.008))
    Forest[[i]] = RPI
  }
  
  # Remvoing those trees which are having only single node
  z  = list()
  zz = list()
  for (i in 1:length(Forest)){
    if(identical(Forest[[i]]$splits,NULL)){
      z[[i]]<-Forest[[i]]
    } else {
      zz[[i]]<-Forest[[i]]
    }
  }
  zz<- zz[!sapply(zz, is.null)]
  Forest<-zz
  
  #~~~~~~~~  TESTING PERFORMANCE
  
  # Predicting the performance of the trees
  pred = list()
  for(i in 1:length(Forest)){
    RP<-predict(Forest[i],type="class",newdata=test)
    pred[[i]] = RP
  }
  
  prob = list()
  for(i in 1:length(Forest)){
    RP<-predict(Forest[i],type="prob",newdata=test)
    prob[[i]] = RP
  }
  
  forest_pred = list()
  for(i in 1:length(Forest)) {
    RP<-prediction(prob[[i]][[1]][,2],test[,1])
    forest_pred[[i]] = RP
  }
  
  # Performance of each trees
  forest_perf = list()
  for(i in 1:length(Forest)) {
    RP<-performance(forest_pred[[i]],"tpr","fpr")
    forest_perf[[i]] = RP
  }
  
  # AUROC of each trees
  forest_AUROC = list()
  for(i in 1:length(Forest)) {
    RP <- round(performance(forest_pred[[i]], measure = "auc")@y.values[[1]]*100, 2)
    forest_AUROC[[i]] = RP
  }
  
  # Gini Index of each trees
  forest_Gini = list()
  for(i in 1:length(Forest)) {
    RP<- (2*forest_AUROC[[i]] - 100)
    forest_Gini[[i]] = RP
  }
  
  # Making the table of the performance of the trees
  tab= list()
  for(i in 1:length(Forest)) {
    RP<-table(test[,1],pred[[i]][[1]])
    tab[[i]] = RP
  }
  
  # Accuracy of the trees
  acc= list()
  for(i in 1:length(Forest)) {
    RP<-sum(diag(tab[[i]]))/sum(tab[[i]])
    acc[[i]] = RP
  }
  
  # Unlisting the trees to get the accuracy, gini index and AUROC of each trees
  acc<-unlist(acc, use.names=FALSE)
  acc<-acc*100
  acc<-round(acc,digits = 2)
  forest_Gini<-unlist(forest_Gini, use.names=FALSE)
  forest_AUROC<-unlist(forest_AUROC, use.names=FALSE)
  
  max_Acc<-max(acc)
  ind_max_acc<-which.max(acc)
  
  min_gini<-min(forest_Gini)
  ind_min_gini<-which.min(forest_Gini)
  
  max_AUROC<-max(forest_AUROC)
  ind_max_AUROC<-which.max(forest_AUROC)
  
  # List of parameters return from the function 
  return(list(Reduced_data=default_data, Test_data=test, Train_data=train, Trees=Forest, Accuracy=acc, max_Acc= max_Acc,ind_max_acc=ind_max_acc, Model_Performance=forest_perf, AUROC=forest_AUROC, max_AUROC=max_AUROC,ind_max_AUROC=ind_max_AUROC, Gini_Index= forest_Gini,min_gini=min_gini,ind_min_gini=ind_min_gini)) 
  
}
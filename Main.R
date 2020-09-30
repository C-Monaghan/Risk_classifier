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


load("GermanCredit.Rdata")

GermanCredit<-GermanCredit[,c(10,1:9,11:62)]

default_data <- GermanCredit

Classifier<-function(default_data,choose_regression = TRUE) {
  
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
                    direction = "forward", scope = biggest)
    
    # Reduced dataset
    default_data<-as.data.frame(x[,names(fwd.model$coefficients[-1])])
    default_data<-data.frame(Class=y,default_data)
    
  }
  else {
    return(default_data)
  }
  ###################### Decision tree
  
  return(list(Data=dim(default_data)))
}

# Trying decision trees
prop.table(table(default_data[,1]))

# Trying decision trees
## Recursive Partitioning and Regression Trees
# Fit the model
fit.r <- rpart(default_data[,1]~.,data=default_data)

# Examine the output
fit.r
summary(fit.r)

# Plot the tree
plot(fit.r)
text(fit.r,use.n=TRUE,cex=0.5,xpd=TRUE,col="red")

# Fit the model
fit.c <- ctree(default_data[,1]~.,data=default_data)

# Examine the output
fit.c
summary(fit.c)

# Plot the tree
plot(fit.c)

# ~~~~~ Recursive Partitioning and Regression Trees
# Fit the model
fit.r <- rpart(default_data[,1]~.,data=default_data)

# ~~~~~ Conditional Inference Trees
# Fit the model
fit.c <- ctree(default_data[,1]~.,data=default_data)

# You get the names of the columns
Cols <- names(default_data)
Cols <- Cols[! Cols %in% "Class"]
n <- length(Cols)
 

# You construct all possible combinations
id <- unlist( lapply(1:n,function(i)combn(1:n,i,simplify=FALSE)) ,
              recursive=FALSE)
id<-sample(id, 1000, replace=FALSE)

# You paste them to formulas
Formulas <- sapply(id,function(i)
  paste("Class~",paste(Cols[i],collapse="+")))


aa<-lapply(Formulas,function(i) ctree(as.formula(i),data=default_data))
bb<- lapply(Formulas,function(i) rpart(as.formula(i),data=default_data))
getwd()


# save(aa, file="ctree.RData")
# save(bb, file="rpart.RData")
# 
# load("ctree.RData")
# load("rpart.RData")
# 
# write.csv(ctree,file="ctree.csv")
# write.csv(rpart,file="rpart.csv")


Forest = list()
for(i in 1:1000) {
  RPI = rpart(Formulas[[i]],data=default_data,method = "class")
  Forest[[i]] = RPI
}


save(Forest,file = "Forest.RData" )
load("Forest.RData")
write.csv(Forestm,file="Forest.csv")
write.table(Forest,file = "Forest.csv")
library(data.table)
fwrite(Forest,file = "Forest.csv",sep = " " , sep2 = c("","|",""), row.names = T,quote = F,col.names = TRUE)

#~~~~~~~~  TESTING PERFORMANCE
library(caret)
pred = list()
for(i in 1:1000) {
  RPI<-predict(Forest[i],type="class")
  pred[[i]] = RPI
}

tab= list()
for(i in 1:1000) {
  RP<-table(default_data[,1],pred[[i]][[1]])
  tab[[i]] = RP
}

res= list()
for(i in 1:1000) {
  RP<-sum(diag(tab[[i]]))/sum(tab[[i]])
  res[[i]] = RP
}

res<-unlist(res, use.names=FALSE)
summary(res)
write.table(res,sep="",file = "res.csv",row.names = FALSE)

rpart.plot(bb[[1]])
plotcp(bb[[1]])
A<-as.list(aa)
B<-as.list(bb)



prp(fit.r, extra = 1)
plotcp(fit.r)

predict_red_org<-predict(fit.r,type="class")
tab_red_org<-table(default_data[,1],predict_red_org)
res_red_org<-sum(diag(tab_red_org))/sum(tab_red_org)
res_red_org
confusionMatrix(data = predict_red_org, reference = default_data[,1])


pred_a<-predict(A[[100]])
tab_b<-table(default_data[,1],pred_b)
res_a<-sum(diag(tab_a))/sum(tab_a)
res_a
confusionMatrix(data = pred_a, reference = default_data[,1])


pred_b<-predict(B[[100]],type="class")
tab_a<-table(default_data[,1],pred_a)
res_b<-sum(diag(tab_b))/sum(tab_b)
res_b
confusionMatrix(data = pred_b, reference = default_data[,1])


#interpretation
rules_r <- tidyRules(fit.r)
print(rules_r)
print(rules_r[2])


use_python("C:/Users/fredx/Anaconda3",required=T)
source_python("Source_EA.py")
C = DecisionTree_EA()
tree_from_r(rules_r)

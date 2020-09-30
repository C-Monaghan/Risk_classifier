load("GermanCredit.Rdata")

library(glmnet)

# Ridge Regression
GermanCredit<-na.omit(GermanCredit)
x=model.matrix(Class~.,GermanCredit)[,-1]
y=GermanCredit$Class

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,family = "binomial")
plot(ridge.mod)
coef(ridge.mod)

set.seed(1)
cv.out_ridge=cv.glmnet(x[train,],y[train],alpha=0,family = "binomial")
plot(cv.out_ridge)
bestlam_ridge=cv.out_ridge$lambda.min
bestlam_ridge
# 137.7314

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam_ridge,family = "binomial")
ridge.pred=predict(ridge.mod,s=bestlam_ridge,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred

grid<-10^seq(10,-2,length= 100)
out<-glmnet(x,y,alpha = 0,lambda = grid,family = "binomial")
ridge.coeff<-predict(out,type = "coefficients",s=bestlam_ridge)[1:20,]
ridge.coeff[ridge.coeff!=0]


# Lasso Regression
GermanCredit<-na.omit(GermanCredit)
x=model.matrix(Class~.,GermanCredit)[,-1]
y=GermanCredit$Class

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x[train,],y[train],alpha=1,family = "binomial")
plot(lasso.mod)
coef(lasso.mod)


set.seed(1)
cv.out_lasso=cv.glmnet(x[train,],y[train],alpha=1,family = "binomial")
plot(cv.out_lasso)
bestlam_lasso=cv.out_lasso$lambda.min
bestlam_lasso
# 137.7314

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam_lasso,family = "binomial")
lasso.pred=predict(lasso.mod,s=bestlam_lasso,newx=x[test,])
mean((lasso.pred-y.test)^2)
lasso.pred

grid<-10^seq(10,-2,length= 100)
out_lasso<-glmnet(x,y,alpha = 1,lambda = grid,family = "binomial")
lasso.coeff<-predict(out_lasso,type = "coefficients",s=bestlam_lasso)[1:20,]
lasso.coeff[lasso.coeff!=0]







# Trying decision trees
GermanCredit<-GermanCredit[,c(1,2,3,5,8,9,10,11,13,14,15,16,19,20)]
prop.table(table(GermanCredit$Class))

## Defining X and Y
x<-model.matrix(GermanCredit$Class~.,GermanCredit)[,-1]
y<-GermanCredit$Class

# Trying decision trees
library(rpart)
library(rattle)

## Recursive Partitioning and Regression Trees
# Fit the model
fit.r <- rpart(GermanCredit$Class~.,data=GermanCredit)

# Examine the output
fit.r
summary(fit.r)

# Plot the tree
plot(fit.r)
text(fit.r,use.n=TRUE,cex=0.5,xpd=TRUE,col="red")

library(party)
library(rpart.plot)
# Fit the model
fit.c <- ctree(GermanCredit$Class~.,data=GermanCredit)

# Examine the output
fit.c
summary(fit.c)

# Plot the tree
plot(fit.c)


# You get the names of the columns
Cols <- names(GermanCredit)
Cols <- Cols[! Cols %in% "Class"]
n <- length(Cols)

# You construct all possible combinations
id <- unlist( lapply(1:n,function(i)combn(1:n,i,simplify=FALSE)) ,
              recursive=FALSE)
id<-sample(id, 1000, replace=FALSE)

# You paste them to formulas
Formulas <- sapply(id,function(i)
  paste("Class~",paste(Cols[i],collapse="+")))

aa<-lapply(Formulas,function(i) ctree(as.formula(i),data=GermanCredit))
bb<- lapply(Formulas,function(i) rpart(as.formula(i),data=GermanCredit))
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
  RPI = rpart(Formulas[[i]],data=GermanCredit,method = "class")
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
  RP<-table(GermanCredit$Class,pred[[i]][[1]])
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
tab_red_org<-table(GermanCredit$Class,predict_red_org)
res_red_org<-sum(diag(tab_red_org))/sum(tab_red_org)
res_red_org
confusionMatrix(data = predict_red_org, reference = GermanCredit$Class)


pred_a<-predict(A[[100]])
tab_b<-table(GermanCredit$Class,pred_b)
res_a<-sum(diag(tab_a))/sum(tab_a)
res_a
confusionMatrix(data = pred_a, reference = GermanCredit$Class)


pred_b<-predict(B[[100]],type="class")
tab_a<-table(GermanCredit$Class,pred_a)
res_b<-sum(diag(tab_b))/sum(tab_b)
res_b
confusionMatrix(data = pred_b, reference = GermanCredit$Class)



#interpretation
library("tidyrules")
library("dplyr")
library("pander")
rules_r <- tidyRules(fit.r)
print(rules_r)
print(rules_r[2])

library("reticulate")
use_python("C:/Users/fredx/Anaconda3",required=T)
source_python("Source_EA.py")
C = DecisionTree_EA()
tree_from_r(rules_r)



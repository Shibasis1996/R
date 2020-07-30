df <- read.csv("E:/R/r project data/College_admission.csv",header=TRUE)
View(df)

#SUMMARY OF DATASET
summary(df)

#SEARCHING FOR MISSSING VALUES ACROSS THE DATASET
which(is.na(df))

#OUTLIERS
plot(df$gre,df$gpa, main="With Outliers", xlab="gre", ylab="gpa", pch="*", col="red", cex=1)

boxplot(df$gre)
outlier_values <- boxplot.stats(df$gre)$out
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
#TREATMENT FOR GRE
qnt <- quantile(df$gre, probs=c(.25, .75), na.rm = T)
caps <- quantile(df$gre, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(df$gre, na.rm = T)
df$gre[df$gre < (qnt[1] - H)] <- caps[1]


boxplot(df$gpa)
outlier_values <- boxplot.stats(df$gpa)$out
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
#OUTLIER TREATMENT FOR GPA
qnt <- quantile(df$gpa,probs=c(.25, .75), na.rm = T)
caps <- quantile(df$gpa, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(df$gpa, na.rm = T)
df$gpa[df$gpa < (qnt[1] - H)] <- caps[1]


plot(df$gre,df$gpa, main="Without Outliers", xlab="gre", ylab="gpa", pch="*", col="red", cex=1)

#Structure of DATASET
str(df)

#CONVERTING INT TO FACTOR FOR CATEGORICAL VALUES
df$admit = as.factor(df$admit)
df$ses = as.factor(df$ses)
df$Gender_Male = as.factor(df$Gender_Male)
df$Race = as.factor(df$Race)
df$rank = as.factor(df$rank)

#NORMALIZATION

hist(df$gre)
hist(df$gpa)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$gre <- normalize(df$gre)
hist(df$gre)

df$gpa <- normalize(df$gpa)
hist(df$gpa)

shapiro.test(df$gre)
shapiro.test(df$gpa)



#VARIABLE REDUCTION
df$admit <- as.numeric(df$admit)
df$ses<-as.numeric(df$ses)
df$Gender_Male<-as.numeric(df$Gender_Male)
df$Race<-as.numeric(df$Race)
df$rank<-as.numeric(df$rank)

round(cor(df),1)

df<- df[,-4:-5]


df$admit = as.factor(df$admit)
df$Race = as.factor(df$Race)
df$rank = as.factor(df$rank)

str(df)

#TRAIN_TEST SPLIT
library(caTools)
split<- sample.split(df,SplitRatio = 0.8)
training_set<- subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)

#LoGISTIC REGRESSION

log_model <- glm(admit~.,data=training_set,family="binomial")
summary(log_model)

varImp(log_model, scale = FALSE)

prediction <- predict(log_model,test_set,type="response")
y_pred <- rep(1,80)
y_pred[prediction>0.5]<-2
table(y_pred,test_set$admit) #ACC=72.5%

#DECISION TREE

library(rpart)
library(rpart.plot)

regTree <- rpart(admit~.,method="class",data=training_set,control = rpart.control(maxdepth = 3))
rpart.plot(regTree,box.palette = "RdBu")

full_tree <- rpart(admit~.,method="class",data=training_set,control = rpart.control(cp=0))
rpart.plot(full_tree,box.palette = "RdBu")

plotcp(regTree)

minCp <- regTree$cptable[which.min(regTree$cptable[,"xerror"]),"CP"]

prun_tree<-prune(full_tree,cp=minCp)

rpart.plot(prun_tree)

full_pred <- predict(full_tree,test_set,type = "class")
table(full_pred,test_set$admit) #ACC = 68.75

reg_pred <- predict(regTree,test_set,type = "class")
table(reg_pred,test_set$admit) #ACC = 68.75

prun_pred <- predict(prun_tree,test_set,type = "class")
table(prun_pred,test_set$admit) #ACC = 75

#SVM
library(e1071)
svmfit<- svm(admit~.,data = training_set,kernel="linear",type="C-classification")
summary(svmfit)

svm_pred<- predict(svmfit,test_set,type="class")
table(svm_pred,test_set$admit) 


plot(svmfit,training_set,gre~gpa)  #ACC=67.5

#RANDOM FOREST
library(randomForest)

forest<- randomForest(admit~.,data=training_set,ntree=500)
pred <- predict(forest,test_set,type="class")
table(pred,test_set$admit) #ACC= 72.5


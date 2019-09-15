#2)Use the wbcd dataset and predict the type of cancer.
#Justify the reason whichever method you will use. 
#Out of multiple variables which variables have max importance. 
#What is your prediction accuracy and confidence interval of it.

getwd()
setwd("R/Machine_Learning/mini-Project/")
# Read Data
data <- read.csv("wbcd.csv")
#benign (not cancerous) malignant (cancerous)
data$id=NULL
str(data)
data$diagnosis <- as.factor(data$diagnosis)
table(data$diagnosis)

# Data Partition
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]
library(randomForest)
rf <- randomForest(diagnosis~., data=train,importance = TRUE)
plot(rf)
# Prediction & Confusion Matrix - train data

p1 <- predict(rf, train)
t1=table(p1, train$diagnosis)
acc1=sum(diag(t1))/sum(t1)
acc1
# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$diagnosis)
t2 = table(p2, test$diagnosis)
acc2=sum(diag(t2))/sum(t2)
acc2
## Changing ntree an mtry value and checking prediction accuracy
##default ntree=500 and mtry=squareroot of variables
#number of predictors sampled for spliting at each node.
rf2 <- randomForest(diagnosis~., data=train,
                    ntree = 1,
                    mtry = 1)
p4 <- predict(rf2, test)
t4 = table(p4, test$diagnosis)
acc4=sum(diag(t4))/sum(t4)
acc4
# Variable Importance --->>Dotchart of variable importance as measured
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
#Classification-->> Naive Bayes Bayes Theorem p(A|b)= wala

m1=naive_bayes(diagnosis~area_worst+perimeter_worst+radius_worst+points_mean+points_worst+perimeter_mean+radius_mean+area_mean+concavity_mean+area_se,data = train)
plot(m1)
y1=predict(m1,train)
table(train$diagnosis)
table(y1)
pred=predict(m1,test)
tab1=table(test$diagnosis)
tab2=table(pred)

tyy = table(pred, test$diagnosis)
accc=sum(diag(tyy))/sum(tyy)*100

vv=c(acc2*100,acc4*100,accc)
print(vv)
barplot(vv,ylim = c(0,120),xlab ="Accuracy Of Model",ylab = "Accuracy of Models",names.arg = c("Random Forest1","Random Forest2","Classifiers_Acc"),col = c('red','blue','green'))

library(dplyr)
library(readxl)
library(pROC)
library(ggplot2)
library(caret)

main_traindata<- read_excel("C:/Users/HP/OneDrive/Desktop/College_SEM 6/NTCC\\Training Data.xlsx")
train_data<-read_excel("C:/Users/HP/OneDrive/Desktop/College_SEM 6/NTCC\\traindata.xlsx")
testdata<- read_excel("C:/Users/HP/OneDrive/Desktop/College_SEM 6/NTCC/Test Data.csv (1)\\testdata.xlsx")
str(train_data)
str(testdata)
train_data$Id<- NULL
testdata$ID<-NULL



#fix class disbalance
ggplot(data = train_data, aes(x = Risk_Flag)) + geom_bar()
w_fact <- sum(train_data$Risk_Flag)/sum(1-train_data$Risk_Flag)
train_data[c("Weights")] <- (train_data$Risk_Flag == 0) * w_fact + (train_data$Risk_Flag == 1) * 1
View(train_data)


#making traning data
library(caTools)
set.seed(188)
split=sample.split(train_data$Risk_Flag,SplitRatio= 0.75)
train_train_set=subset(train_data,split==TRUE)
train_test_set= subset(train_data,split==FALSE)




#Exploratory Data Analysis...........................
train_data%>%
  count(Risk_Flag)
barplot(table(train_data$Risk_Flag), xlab = "Risk Flag", ylab = "Number of people who applied for loans")
#Loan given vs marital status::::::::::::married= 1, single = 0
print(ggplot(train_data, aes(x=Risk_Flag))+geom_bar()+facet_grid(.~ marital_status)+ggtitle("Loan Status by Marital Status of Applicant"))
#Loan given vs House ownership
print(ggplot(main_traindata, aes(x=Risk_Flag))+geom_bar()+facet_grid(.~House_Ownership)+ggtitle("Loan Status by House Ownership Status of Applicant"))
#Loan Given Vs Car Ownership
print(ggplot(main_traindata, aes(x=Risk_Flag))+geom_bar()+facet_grid(.~Car_Ownership)+ggtitle("Loan Status by Car Ownership Status of Applicant"))



#################.................................
#logistic model


logistic<-glm(Risk_Flag ~ Age+Experience+marital_status+rented+owned+norent_noown+car_own+CURRENT_JOB_YRS+CURRENT_HOUSE_YRS,data = train_train_set,family = "binomial", weights = train_train_set$Weights)
summary(logistic)



#...............

#predict model
pmmodel= predict.glm(object = logistic,newdata = train_test_set,type = "response")

#Confusion matrix
confusionMatrix(as.factor(as.double(pmmodel>.5)),as.factor(train_test_set$Risk_Flag))
sensitivity(train_test_set, pmmodel)
#confution matrix in another way
tab= table(pmmodel>0.5,train_test_set$Risk_Flag)
tab



#accuracy of the matrix
sum(diag(tab))/sum(tab)*100

#ROC Curve
p <- predict(logistic, newdata=train_test_set,  type="response")
pr <- prediction(p, train_test_set$Risk_Flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,
     main= "ROC CURVE")

#Accuracy of the curve
x=performance(pr,"acc")
max= which.max(slot(x,"y.values")[[1]])
acc=slot(x,"y.values")[[1]][max]
acc

#Area Under The ROC curve
auc(train_test_set$Risk_Flag,pmmodel)



#####################............................
#Random Forest
library(randomForest)
output.forest<-randomForest(Risk_Flag~Age+Experience+marital_status+rented+owned+norent_noown+car_own
                            +CURRENT_JOB_YRS+CURRENT_HOUSE_YRS,
                            data= train_train_set, ntree=200)
print(output.forest)


#predict model
pmmodel1= predict(output.forest,newdata = train_test_set,type = "response")



#confution matrix
confusionMatrix(as.factor(as.double(pmmodel1>.5)),as.factor(train_test_set$Risk_Flag))

#confusion matrix in another way
tab= table(pmmodel1>0.5,train_test_set$Risk_Flag)
tab
#accuracy
sum(diag(tab))/sum(tab)*100

# ROC curve
roc_rf <- roc(train_test_set$Risk_Flag, as.numeric(unlist(pred["1"])), smooth = TRUE, plot = TRUE)
roc_rf
#Area Under The ROC curve
auc(train_test_set$Risk_Flag,pmmodel1)


#Roc comparison

roc1 <- plot.roc(train_test_set$Risk_Flag, pmmodel, 
                 main="ROC comparison", percent=TRUE, col= "red")
roc2 <- lines.roc(train_test_set$Risk_Flag, as.numeric(unlist(pred["1"])),smooth = TRUE, percent=TRUE, col="blue")




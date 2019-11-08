#11-7-19_74 patients _ IFN genes and score and SLICC total score not high vs low
#0-2 is labeled as 1 and >=3 as 2
# Data read in off of biowulf

# Read Data
setwd("/data/amarnanian/Data Files Wd")
data <- read.csv("/spin1/USERS1/amarnanian/Data Files Wd/11-7-19 74 patients for R cleaned.csv", header = TRUE, 
                 na.strings = " ")

data <- data.frame(data)
str(data)
data$slicc_factor <- as.factor(data$slicc_factor)
str(data)


#Rem variables
data$Date.of.IFN.score <-NULL
data$SLICC.DATE <-NULL
data$SLICC..output. <- NULL
data$Median.SLEDAI..Output. <- NULL
data$DATE_Dif<-NULL
#data$Age<-NULL
#data$Monocytes....Blood.<-NULL
#data$Monocytes.Abs..Blood.<-NULL

data$SLEDAI_SEIZURE <- as.factor(data$SLEDAI_SEIZURE)
data$SLEDAI_PSYCHOSIS <- as.factor(data$SLEDAI_PSYCHOSIS)
data$SLEDAI_OBS <- as.factor(data$SLEDAI_OBS)
data$SLEDAI_VISUAL_DISTURB <- as.factor(data$SLEDAI_VISUAL_DISTURB)
data$SLEDAI_CRANIAL_NERVE <- as.factor(data$SLEDAI_CRANIAL_NERVE)
data$SLEDAI_LUPUS_HEADACHE <- as.factor(data$SLEDAI_LUPUS_HEADACHE)
data$SLEDAI_CVA <- as.factor(data$SLEDAI_CVA)
data$SLEDAI_VASCULITIS <- as.factor(data$SLEDAI_VASCULITIS)
data$SLEDAI_ARTHRITIS <- as.factor(data$SLEDAI_ARTHRITIS)
data$SLEDAI_MYOSITIS <- as.factor(data$SLEDAI_MYOSITIS)
data$SLEDAI_URINARY_CAST <- as.factor(data$SLEDAI_URINARY_CAST)
data$SLEDAI_HEMATURIA <- as.factor(data$SLEDAI_HEMATURIA)
data$SLEDAI_PROTEINURIA <- as.factor(data$SLEDAI_PROTEINURIA)
data$SLEDAI_PYURIA <- as.factor(data$SLEDAI_PYURIA)
data$SLEDAI_RASH <- as.factor(data$SLEDAI_RASH)
data$SLEDAI_ALOPECIA <- as.factor(data$SLEDAI_ALOPECIA)
data$SLEDAI_MUCOSAL_ULCERS <- as.factor(data$SLEDAI_MUCOSAL_ULCERS)
data$SLEDAI_PLEURISY <- as.factor(data$SLEDAI_PLEURISY)
data$SLEDAI_PERICARDITIS <- as.factor(data$SLEDAI_PERICARDITIS)
data$SLEDAI_LOW_COMPLEMENT <- as.factor(data$SLEDAI_LOW_COMPLEMENT)
data$SLEDAI_INC_DNA_BIND <- as.factor(data$SLEDAI_INC_DNA_BIND)
data$SLEDAI_FEVER <- as.factor(data$SLEDAI_FEVER)
data$SLEDAI_THROMBOCYTOPENIA <- as.factor(data$SLEDAI_THROMBOCYTOPENIA)
data$SLEDAI_LEUKOPENIA <- as.factor(data$SLEDAI_LEUKOPENIA)

str(data)










table(data$slicc_factor)

#This is the distribution of the SLICC overall.
#0  1  2  3  4  5  6  8 
#13 18 13 11  9  6  2  2

#The two group seperation - 
# 0  1 
#44 30 


#Data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6777, 0.333))
train <- data[ind==1,]
test <- data[ind==2,]

summary(data)
prop.table(table(data$slicc_factor))
barplot(prop.table(table(data$slicc_factor)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "SLICC Output")


#Default RF settings - Train data
library(randomForest)
set.seed(222)
rf <- randomForest(slicc_factor~., data=train, importance = TRUE, proximity = TRUE)
print(rf)     

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$slicc_factor, positive = '1')

#Prediction and confusion- test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$slicc_factor, positive = '1')

plot(rf)


t <- tuneRF(train[,-54], train[,54],
               stepFactor = 0.5,
               plot = TRUE,
               ntreeTry = 400,
               trace = TRUE,
               improve = 0.05)

#Lowest at 7 or 14
rf <- randomForest(slicc_factor~., data=train,
                      ntree = 400,
                      mtry = 7,
                      importance = TRUE,
                      proximity = TRUE)

print(rf)
plot(rf)

#now this is with tuned RF model
#Train
p1 <- predict(rf, train)
confusionMatrix(p1, train$slicc_factor, positive = '1')

#Test
p2 <- predict(rf, test)
confusionMatrix(p2, test$slicc_factor, positive = '1')

#pretty horrible --- have Pvalue with acc > NIR of 0.57928

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")


#VariableImpPlot
varImpPlot(rf)
varImpPlot(rf, type = NULL,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

#IFN signature is high so that is good.
#Take out SLEDAI input as well

importance(rf)
rf$importance
#write.table(rf$importance, file="10-21-19RFexportedimportancevariables.csv", sep=",")


## Variable Used
varUsed(rf)
varused <-varUsed(rf)
write.table(varused, file="exportedvariablesusedLupus10-21-19test.csv", sep=",")


# Partial Dependence Plot
partialPlot(rf, data, Low.Complement, "1")
partialPlot(rf, data, Low.Complement, "2")


#Just exported one by one. Mya not even use these plots in figure.

#Can add to existing plot... can only add from the same group to each other though e.g.
partialPlot(rf, train, P01009, "2", plot = TRUE, add = FALSE)
partialPlot(rf273, train273, P08670.4, "2", plot=T, add=T)
?partialPlot
#X axis is the value ... and the Y axis is the arbitrary units for or less likely?Arbitrary. When higher then ___ then more likley to predict.

# Extract Single Tree
getTree(rf, 2, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
rftrain <- randomForest(BinaryMedianSledai~., data=train, importance = TRUE, proximity = TRUE)
rftest <- randomForest(BinaryMedianSledai~., data=test, importance = TRUE, proximity = TRUE)
#Unclear what this means? - all the data applied with the model built base don training data?
MDSplot(rf, data$BinaryMedianSledai)

#Changes the color of same dots because whether correct or not?
MDSplot(rf, train$Later.Sledai..Categorized.)
MDSplot(rf, test$Later.Sledai..Categorized.)

MDSplot(rftrain, train$Later.Sledai..Categorized.)
MDSplot(rftest, test$Later.Sledai..Categorized.)



#For ROC AUC Plot -- why is it elbow lke that?
library(pROC)
p2 <- predict(rf, test)
confusionMatrix(p2, test$Later.Sledai..Categorized.)

##table(factor(, levels=min(test):max(test)), 
      #factor(test, levels=min(test):max(test)))


?predict
rf.roc <- roc(test$Later.Sledai..Categorized., predictor= factor(p2,ordered = TRUE))
rf.roc <- plot.roc(test$Later.Sledai..Categorized., predictor= factor(p2,ordered = TRUE), legacy.axes = TRUE)

?roc
auc <- auc(rf.roc)
auc_legend <- round (auc,4)
legend (0.6,0.2,auc_legend, title="AUC Lupus test 10-1", cex=1.0)


#ForROC AUC Plot
require(pROC)
predictions <-as.data.frame(predict(rf,test,type="prob"))



#HOw can we make it so it isn't an elbow liek that? Not plotting probability.

# predict class and then attach test class
#predictionsLN$ <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
#predictionsLN$observed <- test$condition
#head(predictions)



smooth.roc <- smooth.roc(rf.roc)
#error

# Use predict with type="prob" to get class probabilities
#iris.predictions <- predict(mn.net, newdata=iris.test, type="prob")
#head(iris.predictions)
# This can be used directly in multiclass.roc:
#multiclass.roc(iris.test$Species, iris.predictions)



rf.roc <- roc(test$Later.Sledai..Categorized., predictor= factor(p2,ordered = TRUE))
#error

 
######NewModel with less variables
rm(list = ls(all.names = TRUE))

#See the different attributes
attributes(rf)

#[1] "call"            "type"            "predicted"       "err.rate"        "confusion"      
#[6] "votes"           "oob.times"       "classes"         "importance"      "importanceSD"   
#[11] "localImportance" "proximity"       "ntree"           "mtry"            "forest"         
#[16] "y"               "test"            "inbag"           "terms"          


attributes(rf$votes)




#$class



require(randomForest)
data

# This will make drop a class to make it a 2 class problem
dataROCtest <-data[-which(data$Later.Sledai..Categorized.=="Sledai High"),]
dataROCtest$Later.Sledai..Categorized.<-as.factor(as.character(dataROCtest$Later.Sledai..Categorized.))

set.seed(71)
rf <- randomForest(Later.Sledai..Categorized.~., data=train, importance = TRUE, proximity = TRUE)
rftest <-randomForest()

require(pROC)
rf.roc<-roc(data$Later.Sledai..Categorized.,rf$votes[,2])
#Issues says need sto be same level --- gives same AUC but can't plot
plot(rf.roc)
auc(rf.roc)

plot.separation = function(rf,...) {
  triax.plot(rf$votes,...,col.symbols = c("#FF0000FF",
                                          "#00FF0010",
                                          "#0000FF10")[as.numeric(rf$y)])
}




   
    





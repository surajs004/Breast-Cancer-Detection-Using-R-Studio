install.packages("mlbench")
data(BreastCancer,package='mlbench')
bc <- BreastCancer[complete.cases(BreastCancer),]
View(bc)
str(bc)

glm(Class~Cell.shape, family="binomial",data=bc)
# remove id column
bc <- bc[,-1]

# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
table(bc$Class)
install.packages("caret")
library(caret)

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)

# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]
table(trainData$Class)

# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)
table(down_train$Class)

#up sample
set.seed(100)
up_train <- upSample(x=trainData[,colnames(trainData) %ni% "Class"],y=trainData$Class)
table(up_train$Class)

#logistic model
logitmod <- glm(Class~Cl.thickness+Cell.size+Cell.shape,family = "binomial",data = down_train)
summary(logitmod)      

#predicting
pred <- predict(logitmod, newdata = testData, type = "response")

#So if pred is greater than 0.5, it is malignant else it is benign.
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

#predicting accuracy
mean(y_pred == y_act)


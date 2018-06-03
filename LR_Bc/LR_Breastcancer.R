https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/

library(mlbench)
data(BreastCancer)
# keep complete rows
bc <- BreastCancer[complete.cases(BreastCancer), ]  

dim(bc)
nrow(bc)
summary(bc)
str(bc)
names(bc)

# remove id column
bc <- bc[,-1]

names(bc)
str(bc)

# convert to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

str(bc)

table(bc$Class)

# Change Y values to 1's and 0's
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)

table(bc$Class)
bc$Class <- factor(bc$Class, levels = c(0, 1))

str(bc$Class)
bc$Class <- factor(bc$Class, levels = c(0, 1))
str(bc$Class)

# Prep Training and Test data.
library(caret)
# define 'not in' func
'%ni%' <- Negate('%in%')
# prevents printing scientific notations.  
options(scipen=999)  
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

# Class distribution of train data
table(trainData$Class)

# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)


# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

# Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)

summary(logitmod)

pred <- predict(logitmod, newdata = testData, type = "response")
pred

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

# Accuracy
mean(y_pred == y_act)  # 94%



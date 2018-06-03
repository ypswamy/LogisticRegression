## read data and create relevant variables
credit1 <- read.csv("C:\\Users\\Pandu\\Desktop\\Pandu_ course\\CASE_STUDY\\TBD\\GermanCredit\\germancredit.csv")
credit1
names(credit1)
dim(credit1)
nrow(credit1)
summary(credit1)
str(credit1)
plot(credit1)
pairs(credit1)

str(credit1$Default)
credit1$Default <- factor(credit1$Default)
str(credit1$history)

summary(credit1$history)
str(credit1$history)
levels(credit1$history)
credit1$history = factor(credit1$history,levels=c("A30","A31","A32","A33","A34"))
levels(credit1$history) = c("good","good","poor","poor","terrible")
summary(credit1$history)
str(credit1$history)
levels(credit1$history)

summary(credit1$foreign)
str(credit1$foreign)
levels(credit1$foreign)
credit1$foreign <- factor(credit1$foreign, levels=c("A201","A202"),labels=c("foreign","german"))
summary(credit1$foreign)
str(credit1$foreign)
levels(credit1$foreign)

summary(credit1$rent)
str(credit1$rent)
levels(credit1$rent)
credit1$rent <- factor(credit1$housing=="A151")
summary(credit1$rent)
str(credit1$rent)
levels(credit1$rent)

summary(credit1$purpose)
str(credit1$purpose)
levels(credit1$purpose)
credit1$purpose <- factor(credit1$purpose,
levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit1$purpose) <-
c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
summary(credit1$purpose)
str(credit1$purpose)
levels(credit1$purpose)

## for demonstration, cut the dataset to these variables
names(credit1)
credit <- credit1[,c("Default","duration","amount","installment","age",
"history", "purpose","foreign","rent")]
names(credit)
credit[1:3,]
summary(credit) # check out the data

## create a design matrix
## factor variables are turned into indicator variables
## the first column of ones is omitted
Xcred <- model.matrix(Default~.,data=credit)[,-1]
Xcred[1:3,]

## creating training and prediction datasets
## select 900 rows for estimation and 100 for testing
set.seed(1)
train <- sample(1:1000,900)
xtrain <- Xcred[train,]
xnew <- Xcred[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]
credglm=glm(Default~.,family=binomial,data=data.frame(Default=ytrain,xtrain))
summary(credglm)

## prediction: predicted default probabilities for cases in test set
ptest <- predict(credglm,newdata=data.frame(xnew),type="response")
data.frame(ynew,ptest)

## What are our misclassification rates on that training set?
## We use probability cutoff 1/6
## coding as 1 (predicting default) if probability 1/6 or larger
gg1=floor(ptest+(5/6))
ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/100
error


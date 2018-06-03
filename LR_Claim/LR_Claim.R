claimants <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LR_Claim/claimants.csv")
View(claimants)
attach(claimants)
dim(claimants)
structure(claimants)
str(claimants)
nrow(claimants)
colnames(claimants)
claimants


# Linear Regression
fit<-lm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS)
summary(fit)
# Linear regression technique can not be employed

# Logistic Regression 
logit<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit)

# Odds Ratio
exp(coef(logit))

# Confusion matrix table 
prob <- predict(logit,type=c("response"),claimants)
prob
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
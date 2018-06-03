DiabetesData=data.table(read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LR_Diab/diabetes.csv'))

##Quick data summary
summary(DiabetesData)


##Scatter plot matrix
require(GGally)
ggpairs(DiabetesData,lower = list(continuous='smooth'))

###first model
glm1=glm(Outcome~.,DiabetesData, family=binomial(link="logit"))
summary(glm1)
require(stargazer)
stargazer(glm1,type='text')

#The overall model is significant. As expected the glucose rate has the lowest p-value #of all the variables. However, Age, Insulin and Skin Thickness are not good predictors #of Diabetes.

#Since some variables are not significant, removing them is a good way to improve model #robustness. In the second model, SkinThickness, Insulin, and Age are removed.

###second model
glm2=glm(Outcome~.,data=DiabetesData[,c(1:3,6:7,9),with=F], family=binomial(link="logit"))
summary(glm2)

#The model AIC (Akaike information criteria) is lower with a value of 740 vs 741.445 in #the first model. Hence we will choose the second model for further predictions.

Now that we have our model, let’s access its performance.

###Correctly classified observations
mean((glm2$fitted.values>0.5)==DiabetesData$Outcome)

#Around 77.4% of all observations are correctly classified. Due to class imbalance, we #need to go further with a confusion matrix.

###Confusion matrix count
RP=sum((glm2$fitted.values>=0.5)==DiabetesData$Outcome & DiabetesData$Outcome==1)
FP=sum((glm2$fitted.values>=0.5)!=DiabetesData$Outcome & DiabetesData$Outcome==0)
RN=sum((glm2$fitted.values>=0.5)==DiabetesData$Outcome & DiabetesData$Outcome==0)
FN=sum((glm2$fitted.values>=0.5)!=DiabetesData$Outcome & DiabetesData$Outcome==1)
confMat<-matrix(c(RP,FP,FN,RN),ncol = 2)
colnames(confMat)<-c("Pred Diabetes",’Pred no diabetes’)
rownames(confMat)<-c("Real Diabetes",’Real no diabetes’)
print(confMat)

#The model is good to detect people who do not have diabetes. However, its performance #on ill people is not great (only 154 out of 268 have been correctly classified).
#You can also get the percentage of Real/False Positive/Negative:

###Confusion matrix proportion
RPR=RP/sum(DiabetesData$Outcome==1)*100
FNR=FN/sum(DiabetesData$Outcome==1)*100
FPR=FP/sum(DiabetesData$Outcome==0)*100
RNR=RN/sum(DiabetesData$Outcome==0)*100
confMat<-matrix(c(RPR,FPR,FNR,RNR),ncol = 2)
colnames(confMat)<-c("Pred Diabetes",'Pred no diabetes')
rownames(confMat)<-c("Real Diabetes",'Real no diabetes')
confMat

####Plot and decision boundaries
require(ggplot2)
DiabetesData$Predicted<-glm2$fitted.values
ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Predicted>0.5))+
geom_point(size=2,alpha=0.5)
ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Outcome==(Predicted>0.5)))+
geom_point(size=2,alpha=0.5)

ggplot(DiabetesData,aes(x=BMI,y=Glucose,color=Outcome==(Predicted>0.5)))+geom_point(size=2,alpha=0.5)
BMI_plot=data.frame(BMI=((min(DiabetesData$BMI-2)*100):(max(DiabetesData$BMI+2)*100))/100,
 Glucose=mean(DiabetesData$Glucose),
 Pregnancies=mean(DiabetesData$Pregnancies),
 BloodPressure=mean(DiabetesData$BloodPressure),
 DiabetesPedigreeFunction=mean(DiabetesData$DiabetesPedigreeFunction))
BMI_plot$Predicted=predict(glm2,BMI_plot,type = 'response')
ggplot(BMI_plot,aes(x=BMI,y=Predicted))+geom_line()
 
Glucose_plot=data.frame(Glucose=((min(DiabetesData$Glucose-2)*100):(max(DiabetesData$Glucose+2)*100))/100,
 BMI=mean(DiabetesData$BMI),
 Pregnancies=mean(DiabetesData$Pregnancies),
 BloodPressure=mean(DiabetesData$BloodPressure),
 DiabetesPedigreeFunction=mean(DiabetesData$DiabetesPedigreeFunction))
Glucose_plot$Predicted=predict(glm2,Glucose_plot,type = 'response')
ggplot(Glucose_plot,aes(x=Glucose,y=Predicted))+geom_line()
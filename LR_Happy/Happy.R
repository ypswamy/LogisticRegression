http://require(data.table)
Happiness_Data=data.table(read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LR_Happy/2016.csv'))
colnames(Happiness_Data)&amp;lt;-gsub('.','',colnames(Happiness_Data),fixed=T)

require(ggplot2)
require(GGally)
ggpairs(Happiness_Data[,c(4,7:13),with=F],
 lower = list(
 continuous = "smooth"
 ))
 
 ##First model
model1<-lm(Happiness.Score~Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.,data=Happiness_Data)

##Quick summary
sum1=summary(model1)
sum1
require(stargazer)
stargazer(model1,type='text')

##R²
sum1$r.squared*100
##Coefficients
sum1$coefficients
##p-value
df(sum1$fstatistic[1],sum1$fstatistic[2],sum1$fstatistic[3])
 
##Confidence interval of the coefficient
confint(model1,level = 0.95)
confint(model1,level = 0.99)
confint(model1,level = 0.90)

#Residual analysis

###Visualisation of residuals
ggplot(model1,aes(model1$residuals))+geom_histogram(bins=20,aes(y=..density..))+
 geom_density(color='blue')+
 geom_vline(xintercept = mean(model1$residuals),color='red')+
 stat_function(fun=dnorm,
 color="red",size=1,
 args=list(mean=mean(model1$residuals), 
 sd=sd(model1$residuals)))+xlab('residuals values')
 
 #The plot shows no strong evidence of heteroscedasticity.
 
#Analysis of colinearity

require('car')
vif(model1)

#All the VIF are less than 5, and hence there is no sign of colinearity.


#What drives happiness
#Now let’s compute standardised betas to see what really drives happiness.

##Standardized betas
std_betas=sum1$coefficients[-1,1]*
data.table(model1$model)[,lapply(.SD,sd),.SDcols=2:6]/sd(model1$model$HappinessScore)
std_betas

#Though the code above may seem complicated, it is just computing the #standardised betas for all variables std_beta=beta*sd(x)/sd(y).
#The top three coefficients are Health and Life expectancy, Family and GDP #per Capita.


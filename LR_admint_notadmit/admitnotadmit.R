#https://stats.idre.ucla.edu/r/dae/logit-regression/


# A researcher is interested in how variables, such as GRE (Graduate Record Exam scores), 
# GPA (grade point average) and prestige of the undergraduate institution, effect admission 
# into graduate school. The response variable, admit/don’t admit, is a binary variable.

# This dataset has a binary response (outcome, dependent) variable called admit. There are 
# three predictor variables: gre, gpa and rank. We will treat the variables gre and gpa as 
# continuous. The variable rank takes on the values 1 through 4. Institutions with a rank of 
# 1 have the highest prestige, while those with a rank of 4 have the lowest.

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

summary(mydata)
sapply(mydata,sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

#we convert rank to a factor to indicate that rank should be treated as a categorical variable.
mydata$rank <- factor(mydata$rank)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)

#For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
#For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
#The indicator variables for rank have a slightly different interpretation. For example, having attended 
#an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds 
#of admission by -0.675.


## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

#We can test for an overall effect of rank using the wald.test function
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

#The first line of code below creates a vector l that defines the test we want to perform. In this case, 
#we want to test the difference (subtraction) of the terms for rank=2 and rank=3 (i.e., the 4th and 5th terms in the model). 
#The second line of code below uses L=l to tell R that we wish to base the test on the vector l (rather than using the Terms 
#option as we did above).

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)


#The chi-squared test statistic of 5.5 with 1 degree of freedom is associated with a p-value of 0.019, 
#indicating that the difference between the coefficient for rank=2 and the coefficient for rank=3 is statistically significant.


#exponentiate the coefficients and interpret them as odds-ratios.
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))


#We will start by calculating the predicted probability of admission at each value of rank, holding 
#gre and gpa at their means.

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

## view data frame
newdata1

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")

#We are going to plot these, so we will create 100 values of gre between 200 and 800, at each value of rank (i.e., 1, 2, 3, and 4).

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",    se = TRUE))
newdata3 <- within(newdata3, {    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
    size = 1)

#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
#Chi Square

with(mylogit, null.deviance - deviance)

#The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the mode, and can be obtained using:

with(mylogit, df.null - df.residual)

#Finally, the p-value can be obtained using:

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of less than 0.001 
#tells us that our model as a whole fits significantly better than an empty model. This is sometimes 
#called a likelihood ratio test (the deviance residual is -2*log likelihood). To see the model’s log 
#likelihood

logLik(mylogit)
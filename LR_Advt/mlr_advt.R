https://uc-r.github.io/linear_regression

# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Load data (remove row numbers included as X1 variable)
advertising <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LR_Advt/Advertising.csv")

head(advertising)
summary(advertising)
str(advertising)
dim(advertising)
names(advertising)
length(advertising)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
train <- advertising[sample, ]
test <- advertising[!sample, ]


model2 <- lm(sales ~ TV + radio + newspaper, data = train)
summary(model2)
tiny(model2)
glance(model2)
confint(model2)

plot(model2)

list(model1 = broom::glance(model1), 
     model2 = broom::glance(model2))

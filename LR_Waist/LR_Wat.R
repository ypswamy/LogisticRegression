wc.at <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/LR_Waist/wc-at.csv")

wc.at
head(wc.at)
nrow(wc.at)
summary(wc.at)
str(wc.at)
dim(wc.at)
colnames(wc.at)
View(wc.at)
attach(wc.at)
plot(Waist,AT)
rm(list = ls())
getwd()
setwd("/Users/brijeshgoharia/Desktop/SEM 1/Predictive")
organic.df <- read.csv("organics.csv", header = T, na.strings = c(""))

sapply(organic.df,function(x) sum(is.na(x)))
sapply(organic.df, function(x) length(unique(x)))

row.has.na <- apply(organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
organic1.df <- organic.df[,-c(1,6)]

organic1.df <- organic1.df[complete.cases(organic1.df[ , 4:5]),]
sapply(organic1.df,function(x) sum(is.na(x)))
sapply(organic1.df, function(x) length(unique(x)))
row.has.na <- apply(organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#Imputation for NA's
organic1.df$DemAge[is.na(organic1.df$DemAge)] <- mean(organic1.df$DemAge, na.rm = T)
organic1.df$DemAffl[is.na(organic1.df$DemAffl)] <- mean(organic1.df$DemAffl, na.rm = T)
organic1.df$DemCluster[is.na(organic1.df$DemCluster)] <- mean(organic1.df$DemCluster, na.rm = T)
organic1.df$PromTime[is.na(organic1.df$PromTime)] <- mean(organic1.df$PromTime, na.rm = T)
row.has.na <- apply(organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#Dummies
for(level in unique(organic1.df$DemClusterGroup)) {
  organic1.df[level] <- ifelse(organic1.df$DemClusterGroup == level, 1, 0)
}

for(level in unique(organic1.df$DemReg)) {
  organic1.df[level] <- ifelse(organic1.df$DemReg == level, 1, 0)
}

for(level in unique(organic1.df$DemTVReg)) {
  organic1.df[level] <- ifelse(organic1.df$DemTVReg == level, 1, 0)
}

for(level in unique(organic1.df$PromClass)) {
  organic1.df[level] <- ifelse(organic1.df$PromClass == level, 1, 0)
}

organic1.df <- organic1.df[,-c(4:7,11)]

#Data Partitioning
set.seed(112233)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]

#Running Logistic Regression Model
logit.reg <- glm(TargetBuy ~ DemAffl + DemAge + DemCluster + PromSpend + PromTime, data = train.df, family = binomial(link='logit'))

logit.reg <- glm(TargetBuy ~ ., data = train.df, family = binomial(link='logit'))
options(scipen=999)
summary(logit.reg)

for(i in seq(1,1023,1)){
  assign(paste("logit.reg.valid.pred",i,sep = ""),predict(get(paste("logit.reg",i,sep="")), valid.df[,-6],type="response"))
  assign(paste("logit.reg.valid.accuracy",i,sep = ""),confusionMatrix(ifelse(get(paste("logit.reg.valid.pred",i,sep=""))<0.5,0,1), valid.df$TargetBuy)$overall[1])
  ifelse(as.numeric(get(paste("logit.reg.valid.accuracy",i,sep = "")))>0.7993, print(as.numeric(get(paste("logit.reg.valid.accuracy",i,sep = "")))) && print(i),c<-1+2)
}


logit.reg1.pred <- predict(logit.reg, valid.df[,-6], type = "response")
data.frame(actual = valid.df$TargetBuy, predicted = round(logit.reg1.pred,2))

#confusion Matrix
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.49,1,0)), as.factor(valid.df$TargetBuy))
cmatrix

#determine best cutoff
accT = c()
for(cutoff in seq(0, 1, 0.05)){
  cm <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > cutoff, 1, 0)), as.factor(valid.df$TargetBuy))
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0, 1, 0.05), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0,1))
lines(1-accT ~ seq(0,1,0.05), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1,2), merge = T)

#Scored data
score.organic <- read.csv("Scoreorganics.csv")
#create dummy variables
for(level in unique(test_1$Category)){
  test_1[level] <- ifelse(test_1$Category == level, 1, 0)
}

for(level in unique(test_1$currency)){
  test_1[level] <- ifelse(test_1$currency == level, 1, 0)
}

for(level in unique(test_1$endDay)){
  test_1[level] <- ifelse(test_1$endDay == level, 1, 0)
}

load("bestmodelfile")
test_1$m1_score <- predict(logit.reg,type='response',test_1)
test_1$m1_score <- ifelse(test_1$m1_score > 0.5,1,0)
test_1$m1_score
misClasificError <- mean(test_1$m1_score != valid.df$Competitive.[36:70])
print(paste('Accuracy',1-misClasificError))
table(test_1$m1_score, valid.df$Competitive.)
confusionMatrix(table(test_1$m1_score,))

sapply(score.organic,function(x) sum(is.na(x)))
sapply(score.organic, function(x) length(unique(x)))

row.has.na <- apply(score.organic, 1, function(x){any(is.na(x))})
sum(row.has.na)
score.organic1.df <- score.organic[,-c(1,6)]
score.organic1.df <- score.organic1.df[complete.cases(score.organic1.df[ , 4:5]),]
sapply(score.organic1.df,function(x) sum(is.na(x)))
sapply(score.organic1.df, function(x) length(unique(x)))
row.has.na <- apply(score.organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#Imputation for NA's
score.organic1.df$DemAge[is.na(score.organic1.df$DemAge)] <- mean(score.organic1.df$DemAge, na.rm = T)
score.organic1.df$DemAffl[is.na(score.organic1.df$DemAffl)] <- mean(score.organic1.df$DemAffl, na.rm = T)
score.organic1.df$DemCluster[is.na(score.organic1.df$DemCluster)] <- mean(score.organic1.df$DemCluster, na.rm = T)
score.organic1.df$PromTime[is.na(score.organic1.df$PromTime)] <- mean(score.organic1.df$PromTime, na.rm = T)
row.has.na <- apply(score.organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#Dummies
for(level in unique(score.organic1.df$DemClusterGroup)) {
  score.organic1.df[level] <- ifelse(score.organic1.df$DemClusterGroup == level, 1, 0)
}

for(level in unique(score.organic1.df$DemReg)) {
  score.organic1.df[level] <- ifelse(score.organic1.df$DemReg == level, 1, 0)
}

for(level in unique(score.organic1.df$DemTVReg)) {
  score.organic1.df[level] <- ifelse(score.organic1.df$DemTVReg == level, 1, 0)
}

for(level in unique(score.organic1.df$PromClass)) {
  score.organic1.df[level] <- ifelse(score.organic1.df$PromClass == level, 1, 0)
}

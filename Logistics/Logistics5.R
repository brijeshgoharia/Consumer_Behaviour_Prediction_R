rm(list = ls())
getwd()
setwd("/Users/brijeshgoharia/Desktop/SEM 1/Predictive")
organic.df <- read.csv("organics.csv", header = T)
for(i in 1:dim(organic.df)[1]){
  organic.df$DemGender[organic.df$DemGender == ""] <- "U"}

for(i in 1:dim(organic.df)[1]){
  organic.df$DemClusterGroup [organic.df$DemClusterGroup == ""] <- NA
}
for(i in 1:dim(organic.df)[1]){
  organic.df$DemReg [organic.df$DemReg == ""] <- NA
}
for(i in 1:dim(organic.df)[1]){
  organic.df$DemTVReg [organic.df$DemTVReg == ""] <- NA
}
for(i in 1:dim(organic.df)[1]){
  organic.df$PromClass [organic.df$PromClass == ""] <- NA
}

sapply(organic.df,function(x) sum(is.na(x)))
sapply(organic.df, function(x) length(unique(x)))

row.has.na <- apply(organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
organic1.df <- organic.df[,-c(1,13)]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
#partial Removal of N.A
organic1.df <- organic1.df[complete.cases(organic1.df[ ,c(4,6:8)]),]
sapply(organic1.df,function(x) sum(is.na(x)))
sapply(organic1.df, function(x) length(unique(x)))
row.has.na <- apply(organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#library(Amelia)
#missmap(organic.df, main = "Missing values vs observed")
#Imputation for NA's
organic1.df$DemAge[is.na(organic1.df$DemAge)] <- mean(organic1.df$DemAge, na.rm = T)
organic1.df$DemAffl[is.na(organic1.df$DemAffl)] <- mean(organic1.df$DemAffl, na.rm = T)
organic1.df$DemCluster[is.na(organic1.df$DemCluster)] <- mean(organic1.df$DemCluster, na.rm = T)
organic1.df$PromTime[is.na(organic1.df$PromTime)] <- mean(organic1.df$PromTime, na.rm = T)
row.has.na <- apply(organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

for(level in unique(organic1.df$DemGender)){
  organic1.df[level] <- ifelse(organic1.df$DemGender == level,1,0)
}
colnames(organic1.df)[which(names(organic1.df) == "U")] <- "Unknown"
colnames(organic1.df)[which(names(organic1.df) == "F")] <- "Female"
colnames(organic1.df)[which(names(organic1.df) == "M")] <- "Male"

for(level in unique(organic1.df$DemClusterGroup)){
  organic1.df[level] <- ifelse(organic1.df$DemClusterGroup == level,1,0)
}
for(level in unique(organic1.df$DemReg)){
  organic1.df[level] <- ifelse(organic1.df$DemReg == level,1,0)
}
for(level in unique(organic1.df$DemTVReg)){
  organic1.df[level] <- ifelse(organic1.df$DemTVReg == level,1,0)
}
for(level in unique(organic1.df$PromClass)){
  organic1.df[level] <- ifelse(organic1.df$PromClass == level,1,0)
}

organic1.df <- organic1.df[,-c(4:8)]
colnames(organic1.df)[which(colnames(organic1.df) == 'F')] <- 'F.'
#Data Partitioning
set.seed(5555)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]

logit.reg <- glm(TargetBuy ~ DemAffl + DemAge  + Female + Unknown  + Male + North + F. + Border + Silver + PromTime,  data = train.df, family = binomial(link='logit'))
logit.reg <- glm(TargetBuy ~ DemAffl + DemAge + DemCluster + Unknown + Female + Male + PromSpend + PromTime, data = train.df, family = binomial(link='logit'))

logit.reg <- glm(TargetBuy ~ ., data = train.df, family = binomial(link='logit'))
options(scipen=999)
summary(logit.reg)

for(i in seq(1,1023,1)){
  assign(paste("logit.reg.valid.pred",i,sep = ""),predict(get(paste("logit.reg",i,sep="")), valid.norm.df[,-12],type="response"))
  assign(paste("logit.reg.valid.accuracy",i,sep = ""),confusionMatrix(ifelse(get(paste("logit.reg.valid.pred",i,sep=""))<0.5,0,1), valid.norm.df$TargetBuy)$overall[1])
  ifelse(as.numeric(get(paste("logit.reg.valid.accuracy",i,sep = "")))>0.8156, print(as.numeric(get(paste("logit.reg.valid.accuracy",i,sep = "")))) && print(i),c<-1+2)
}


logit.reg1.pred <- predict(logit.reg, valid.df[,-6], type = "response")
data.frame(actual = valid.df$TargetBuy, predicted = round(logit.reg1.pred,2))

#confusion Matrix
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.47,1,0)), as.factor(valid.df$TargetBuy))
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

#Scoredata
score.organic.df <- read.csv("scoreorganicswithoutcomes.csv")
for(i in 1:dim(score.organic.df)[1]){
  score.organic.df$DemGender[score.organic.df$DemGender == ""] <- "U"}

for(i in 1:dim(score.organic.df)[1]){
  score.organic.df$DemClusterGroup [score.organic.df$DemClusterGroup == ""] <- NA
}
for(i in 1:dim(score.organic.df)[1]){
  score.organic.df$DemReg [score.organic.df$DemReg == ""] <- NA
}
for(i in 1:dim(score.organic.df)[1]){
  score.organic.df$DemTVReg [score.organic.df$DemTVReg == ""] <- NA
}
for(i in 1:dim(score.organic.df)[1]){
  score.organic.df$PromClass [score.organic.df$PromClass == ""] <- NA
}

sapply(score.organic.df,function(x) sum(is.na(x)))
sapply(score.organic.df, function(x) length(unique(x)))

row.has.na <- apply(score.organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
#organic1.df <- score.organic.df[,-c(1,13)]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
#partial Removal of N.A
score.organic1.df <- score.organic.df[complete.cases(score.organic.df[ ,c(5:9)]),]
sapply(score.organic1.df,function(x) sum(is.na(x)))
sapply(score.organic1.df, function(x) length(unique(x)))
row.has.na <- apply(score.organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#library(Amelia)
#missmap(score.organic.df, main = "Missing values vs observed")
#Imputation for NA's
score.organic1.df$DemAge[is.na(score.organic1.df$DemAge)] <- mean(score.organic1.df$DemAge, na.rm = T)
score.organic1.df$DemAffl[is.na(score.organic1.df$DemAffl)] <- mean(score.organic1.df$DemAffl, na.rm = T)
score.organic1.df$DemCluster[is.na(score.organic1.df$DemCluster)] <- mean(score.organic1.df$DemCluster, na.rm = T)
score.organic1.df$PromTime[is.na(score.organic1.df$PromTime)] <- mean(score.organic1.df$PromTime, na.rm = T)
row.has.na <- apply(score.organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

for(level in unique(score.organic1.df$DemGender)){
  score.organic1.df[level] <- ifelse(score.organic1.df$DemGender == level,1,0)
}
colnames(score.organic1.df)[which(names(score.organic1.df) == "U")] <- "Unknown"
colnames(score.organic1.df)[which(names(score.organic1.df) == "F")] <- "Female"
colnames(score.organic1.df)[which(names(score.organic1.df) == "M")] <- "Male"

for(level in unique(score.organic1.df$DemClusterGroup)){
  score.organic1.df[level] <- ifelse(score.organic1.df$DemClusterGroup == level,1,0)
}
for(level in unique(score.organic1.df$DemReg)){
  score.organic1.df[level] <- ifelse(score.organic1.df$DemReg == level,1,0)
}
for(level in unique(score.organic1.df$DemTVReg)){
  score.organic1.df[level] <- ifelse(score.organic1.df$DemTVReg == level,1,0)
}
for(level in unique(score.organic1.df$PromClass)){
  score.organic1.df[level] <- ifelse(score.organic1.df$PromClass == level,1,0)
}

score.organic1.df <- score.organic1.df[,-c(5:9)]
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'F')] <- 'F.'

score.organic1.df$pred <- round(predict(logit.reg, newdata=score.organic1.df, type='response'),2)
fitted.results <- ifelse(score.organic1.df$pred > 0.47,1,0)
library(caret)
confusionMatrix(table(as.factor(fitted.results), as.factor(score.organic1.df$TargetBuy)))


score.organic1.df$Classification <- ifelse(score.organic1.df$pred>0.54, 1, 0)
score.organic1.df$Classification
score.organic1.df

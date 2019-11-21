rm(list = ls())
setwd("/Users/brijeshgoharia/Desktop/SEM 1/Predictive")
organic.df <- read.csv("organics.csv", header = T, na.strings = c(""))

sapply(organic.df,function(x) sum(is.na(x)))
sapply(organic.df, function(x) length(unique(x)))

row.has.na <- apply(organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
organic1.df <- organic.df[,-c(1,13)]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
#partial Removal of N.A
organic1.df <- organic1.df[complete.cases(organic1.df[ ,4:8]),]
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
str(organic1.df)
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

str(organic1.df)

#Data Partitioning
set.seed(112244)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]

#Running Logistic Regression Model
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
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.5,1,0)), as.factor(valid.df$TargetBuy))
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

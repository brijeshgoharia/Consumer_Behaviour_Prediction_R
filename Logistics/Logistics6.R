rm(list = ls())
setwd("/Users/brijeshgoharia/Desktop/SEM 1/Predictive")
organic.df <- read.csv("organics.csv")

#Imputing Missing Gender with U and other missing values with NA
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

#Checking Number of NA's
sapply(organic.df,function(x) sum(is.na(x)))
sapply(organic.df, function(x) length(unique(x)))

row.has.na <- apply(organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
organic1.df <- organic.df[,-c(1,13)]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
organic1.df <- organic1.df[complete.cases(organic1.df[ ,1:11]),]
sapply(organic1.df,function(x) sum(is.na(x)))
sapply(organic1.df, function(x) length(unique(x)))
row.has.na <- apply(organic1.df, 1, function(x){any(is.na(x))})
sum(row.has.na)

#Creating Dummies
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
str(organic1.df)

#Data Partitioning
set.seed(119998)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]

#Logistic Regression and Performing Step wise to find the best Predictors

#logit.reg <- glm(TargetBuy ~ ., data = train.df, family = binomial(link='logit'))
#library(forecast)
#housing.d.back <- step(logit.reg, direction="backward")
#summary(housing.d.back) 
#housing.d.back.pred <- predict(housing.d.back, valid.df)
#accuracy(housing.d.back.pred, valid.df$TargetBuy)

#null = lm(TargetBuy ~ 1, data = train.df)
#housing.d.forw <- step(null, scope=list(lower=null, upper=logit.reg), direction="forward")
#summary(housing.d.forw)
#housing.d.forw.pred <- predict(housing.d.forw, valid.df)
#accuracy(housing.d.forw.pred, valid.df$TargetBuy)

#housing.d.step <- step(null, scope=list(upper=logit.reg), data = train.df, direction="both")
#summary(housing.d.step) 
#housing.d.step.pred <- predict(housing.d.step, valid.df)
#accuracy(housing.d.step.pred, valid.df$TargetBuy)

logit.reg <- glm(TargetBuy ~ DemAffl + DemAge  + Female + Unknown  + Male + North + F. + Border + Silver + PromTime,  data = train.df, family = binomial(link='logit'))
options(scipen=999)
summary(logit.reg)

logit.reg1.pred <- predict(logit.reg, valid.df[,-6], type = "response")
data.frame(actual = valid.df$TargetBuy, predicted = round(logit.reg1.pred,2))

#confusion Matrix
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.54,1,0)), as.factor(valid.df$TargetBuy))
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
#score.organic1.df <- score.organic.df[,-1]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
score.organic1.df <- score.organic.df[complete.cases(score.organic.df[ ,c(1:4, 8, 11)]),]
sapply(score.organic1.df,function(x) sum(is.na(x)))
sapply(score.organic1.df, function(x) length(unique(x)))
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
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'South East')] <- 'SouthEast'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'South West')] <- 'SouthWest'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'Wales & West')] <- 'WalesnWest'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'N West')] <- 'NWest'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'N East')] <- 'NEast'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'S & S East')] <- 'SnSEast'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'C Scotland')] <- 'CScotland'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'S West')] <- 'SWest'
colnames(score.organic1.df)[which(colnames(score.organic1.df) == 'N Scot')] <- 'NScot'
str(score.organic1.df)
dim(score.organic1.df)

score.organic1.df$pred <- round(predict(logit.reg, newdata=score.organic1.df, type='response'),2)
fitted.results <- ifelse(score.organic1.df$pred > 0.54,1,0)
library(caret)
confusionMatrix(table(as.factor(fitted.results), as.factor(score.organic1.df$TargetBuy)))


score.organic1.df$Classification <- ifelse(score.organic1.df$pred>0.54, 1, 0)
score.organic1.df$Classification
score.organic1.df



write.table(score.organic1.df, file="mydata.csv",sep=",",row.names=F)
write.csv(data.frame(score.organic1.df), "file.csv")


rm(list = ls())
setwd("C://R directory")

organic.df <- read.csv("organics.csv", header = T, na.strings = c(""))

sapply(organic.df,function(x) sum(is.na(x)))
sapply(organic.df, function(x) length(unique(x)))

row.has.na <- apply(organic.df, 1, function(x){any(is.na(x))})
sum(row.has.na)
organic1.df <- organic.df[,-c(1,13)]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
#partial Removal of N.A
organic1.df <- organic1.df[complete.cases(organic1.df[ ,1:11]),]
sapply(organic1.df,function(x) sum(is.na(x)))
sapply(organic1.df, function(x) length(unique(x)))
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

colnames(organic1.df)[which(names(organic1.df) == "South East")] <- "SouthEast"
colnames(organic1.df)[which(names(organic1.df) == "N West")] <- "NWest"
colnames(organic1.df)[which(names(organic1.df) == "Wales & West")] <- "WalesWest"
colnames(organic1.df)[which(names(organic1.df) == "South West")] <- "SouthWest"
colnames(organic1.df)[which(names(organic1.df) == " East")] <- "SouthEast"
colnames(organic1.df)[which(names(organic1.df) == "N East")] <- "NEast"
colnames(organic1.df)[which(names(organic1.df) == "S & S East")] <- "SSEast"
colnames(organic1.df)[which(names(organic1.df) == "C Scotland")] <- "CScotland"
colnames(organic1.df)[which(names(organic1.df) == "S West")] <- "SWest"
colnames(organic1.df)[which(names(organic1.df) == "N Scot")] <- "NScot"
str(organic1.df)

organic2.df<- organic1.df[,-6]
organic2.df$TargetBuy <- organic1.df$TargetBuy

#Data Partitioning
set.seed(98666)
train.index <- sample(c(1:dim(organic2.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic2.df[train.index, ]
valid.df <- organic2.df[-train.index, ]

#normalize the data
train.norm.df <- train.df
valid.norm.df <- valid.df
organic.norm.df <- organic2.df

library(FNN)
library(caret)

norm.values <- preProcess(train.df[, 1:35], method=c("center", "scale"))
train.norm.df[,1:35] <- predict(norm.values, train.df[, 1:35])
valid.norm.df[,1:35] <- predict(norm.values, valid.df[, 1:35])
organic.norm.df[,1:35] <- predict(norm.values, organic2.df[, 1:35])


# normalize
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

#KNN
#KNN is available in Library FNN
#install.packages("FNN")
library(FNN)
organic.knn <- knn(train = train.norm.df[, 1:35], test = valid.norm.df[1, 1:35], cl = train.norm.df[, 36], k = 3)

attr(organic.knn, "nn.index")
row.names(train.df)[attr(organic.knn, "nn.index")]
attr(organic.knn, "nn.dist")

#KNN accuracy
accuracy.df <- data.frame(k =  seq(1, 20, 1), accuracy = rep(0, 20)) #inialize a data frame

for(i in 1:20) {
  organic.knn.pred <- knn(train.norm.df[, 1:35], valid.norm.df[, 1:35], cl = train.norm.df[,36], k=i)
  accuracy.df[i, 2] <- confusionMatrix(as.factor(organic.knn.pred), as.factor(valid.norm.df[,36]))$overall[1]
} 
accuracy.df


rm(list = ls())
setwd("/Users/brijeshgoharia/Desktop/SEM 1/Predictive")
organic.df <- read.csv("organics.csv")

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

colnames(organic1.df)[which(colnames(organic1.df) == 'F')] <- 'F.'
colnames(organic1.df)[which(colnames(organic1.df) == 'South East')] <- 'SouthEast'
colnames(organic1.df)[which(colnames(organic1.df) == 'South West')] <- 'SouthWest'
colnames(organic1.df)[which(colnames(organic1.df) == 'Wales & West')] <- 'WalesnWest'
colnames(organic1.df)[which(colnames(organic1.df) == 'N West')] <- 'NWest'
colnames(organic1.df)[which(colnames(organic1.df) == 'N East')] <- 'NEast'
colnames(organic1.df)[which(colnames(organic1.df) == 'S & S East')] <- 'SnSEast'
colnames(organic1.df)[which(colnames(organic1.df) == 'C Scotland')] <- 'CScotland'
colnames(organic1.df)[which(colnames(organic1.df) == 'S West')] <- 'SWest'
colnames(organic1.df)[which(colnames(organic1.df) == 'N Scot')] <- 'NScot'
str(organic1.df)

#Data Partitioning
set.seed(11111)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]


# normalize
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

#NueralNetwork
organic.nn <- neuralnet(TargetBuy ~ DemAffl + DemAge + PromTime + Unknown + Female + 
                          F. +  North + Border + Silver ,
                        data = train.norm.df, linear.output = F, hidden = 2, stepmax = 1e6 )

organic.nn <- neuralnet(TargetBuy ~ DemAffl + DemAge + DemCluster + PromSpend + PromTime + Unknown + Female + Male + 
                          C + D + F. + A + B + E + U + Midlands + North + SouthEast + Scottish + 
                          SouthWest + WalesnWest + NWest + East + NEast + London + SnSEast + CScotland + 
                          SWest + Yorkshire + Border + NScot + Gold + Silver + Tin + Platinum,
                        data = train.norm.df, linear.output = F, hidden = 2, stepmax = 1e6 )

plot(organic.nn)
prediction(organic.nn)

organic.nn.pred  = compute (organic.nn, valid.norm.df[,c("DemAffl","DemAge","PromTime","Unknown","Female","F."
                                                ,"North","Border","Silver")])

organic.nn.pred <- compute(organic.nn, data.frame(valid.norm.df$DemAffl, valid.norm.df$DemAge, valid.norm.df$DemCluster, valid.norm.df$PromSpend, valid.norm.df$PromTime, valid.norm.df$Unknown, valid.norm.df$Female, valid.norm.df$Male, 
                                                  valid.norm.df$C, valid.norm.df$D, valid.norm.df$F., valid.norm.df$A, valid.norm.df$B, valid.norm.df$E, valid.norm.df$U, valid.norm.df$Midlands, valid.norm.df$North, valid.norm.df$SouthEast, valid.norm.df$Scottish, 
                                                  valid.norm.df$SouthWest, valid.norm.df$WalesnWest, valid.norm.df$NWest, valid.norm.df$East, valid.norm.df$NEast, valid.norm.df$London, valid.norm.df$SnSEast, valid.norm.df$CScotland,
                                                  valid.norm.df$SWest, valid.norm.df$Yorkshire, valid.norm.df$Border, valid.norm.df$NScot , valid.norm.df$Gold, valid.norm.df$Silver, valid.norm.df$Tin, valid.norm.df$Platinum))

#confusion matrix
cmatrix <- confusionMatrix(as.factor(ifelse(organic.nn.pred$net.result > 0.53, 1, 0)), as.factor(valid.norm.df$TargetBuy))
cmatrix

#Scoredata
score.organic.df <- read.csv("Scoreorganics.csv")

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
score.organic1.df <- score.organic.df[,-1]
#For Complete Removal of N.A
#final[complete.cases(final), ] or use na.omit
score.organic1.df <- score.organic1.df[complete.cases(score.organic1.df[ ,c(1:3, 7, 10)]),]
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

score.organic1.df <- score.organic1.df[,-c(4:8)]

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

score.organic.nn.pred  = compute (score.organic1.df, valid.norm.df[,c("DemAffl","DemAge","PromTime","Unknown","Female","F."
                                                                      ,"North","Border","Silver")])

cmatrix <- confusionMatrix(as.factor(ifelse(score.organic.nn.pred$net.result > 0.53, 1, 0)), as.factor(valid.norm.df$TargetBuy))
cmatrix


scoreauction.df$pred <- round(predict(organic.nn.pred, newdata=score.organic1.df, type='response'), )
scoreauction.df$Classification <- ifelse(scoreauction.df$pre>0.5, 1, 0)
scoreauction.df  


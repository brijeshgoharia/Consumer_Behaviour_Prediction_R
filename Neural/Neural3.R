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
set.seed(98661)
train.index <- sample(c(1:dim(organic1.df)[1]),dim(organic1.df)[1]*0.6)
train.df <- organic1.df[train.index, ]
valid.df <- organic1.df[-train.index, ]


# normalize
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

#NueralNetwork
organic.nn <- neuralnet(TargetBuy ~ DemAffl + DemAge + DemCluster + PromSpend + PromTime + Unknown + Female + Male + 
                          C + D + F. + A + B + E + U + Midlands + North + SouthEast + Scottish + 
                          SouthWest + WalesnWest + NWest + East + NEast + London + SnSEast + CScotland + 
                          SWest + Yorkshire + Border + NScot + Gold + Silver + Tin + Platinum,
                        data = train.norm.df, linear.output = F, hidden = 2, stepmax = 1e6 )


plot(organic.nn)
prediction(organic.nn)

organic.nn.pred <- compute(organic.nn, data.frame(valid.norm.df$DemAffl, valid.norm.df$DemAge, valid.norm.df$DemCluster, valid.norm.df$PromSpend, valid.norm.df$PromTime, valid.norm.df$Unknown, valid.norm.df$Female, valid.norm.df$Male, 
                                                  valid.norm.df$C, valid.norm.df$D, valid.norm.df$F., valid.norm.df$A, valid.norm.df$B, valid.norm.df$E, valid.norm.df$U, valid.norm.df$Midlands, valid.norm.df$North, valid.norm.df$SouthEast, valid.norm.df$Scottish, 
                                                  valid.norm.df$SouthWest, valid.norm.df$WalesnWest, valid.norm.df$NWest, valid.norm.df$East, valid.norm.df$NEast, valid.norm.df$London, valid.norm.df$SnSEast, valid.norm.df$CScotland,
                                                  valid.norm.df$SWest, valid.norm.df$Yorkshire, valid.norm.df$Border, valid.norm.df$NScot , valid.norm.df$Gold, valid.norm.df$Silver, valid.norm.df$Tin, valid.norm.df$Platinum))

#confusion matrix
cmatrix <- confusionMatrix(as.factor(ifelse(organic.nn.pred$net.result > 0.49, 1, 0)), as.factor(valid.norm.df$TargetBuy))
cmatrix

#determine best cutoff
accT = c()
for(cutoff in seq(0, 1, 0.05)){
  cm <- confusionMatrix(as.factor(ifelse(organic.nn.pred$net.result > cutoff, 1, 0)), as.factor(valid.norm.df$TargetBuy))
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0, 1, 0.05), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0,1))
lines(1-accT ~ seq(0,1,0.05), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1,2), merge = T)








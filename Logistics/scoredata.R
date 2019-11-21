Scored Data:
  rm(list = ls())
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
  organic1.df <- score.organic.df[,-1]
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
  
  
  scoreauction.df$pred <- round(predict(organic.nn.pred, newdata=score.organic1.df, type='response'),2)
  scoreauction.df$Classification <- ifelse(scoreauction.df$pre>0.5, 1, 0)
  scoreauction.df  
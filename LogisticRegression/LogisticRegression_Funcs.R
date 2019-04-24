# Machine Learning Functions

# Librarys ----


# update.packages(old.packages())
data_tree_Flag <- require(data.tree)
dplyr_Flag <- require(dplyr)
nloptr_Flag <- require(nloptr)
# tidyverse_Flag <- library(tidyverse)

if(!data_tree_Flag){
  install.packages("data.tree")
  library(data.tree)
}
if(!dplyr_Flag){
  install.packages("dplyr")
}
if(!nloptr_Flag){
  install.packages("nloptr")
}


# Functions ----

sig_eval <- function(x){
  val <- 1/(1+exp(-x))
  return(val)
}




trn_LogReg_SGD_MAP <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),epoch=10,var=0.5,gamma0=0.5,d0=gamma0,labelInd=ncol(trndata),iters=1,objVal=0){
  
  M_cnt    <- nrow(trndata)
  
  sampInds <- sample(1:M_cnt,M_cnt,replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  data_x <- cbind(rep(1,M_cnt),data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below (assume provided as 1 and 0)
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  lR <- gamma0/(1+(gamma0/d0)*iters)
  
  for(i in sampInds){
    
    y_sig <- as.numeric(labels[i] - sig_eval(wgts%*%data_x[i,])) # data_x dimensions flip for some reason, no need for t()
    
    dJ   <- 1/var * wgts + M_cnt*y_sig*t(data_x[i,])
    
    wgts <- wgts - lR*dJ
    
    objVal[length(objVal)+1] = M_cnt*log(1+exp(-labels[i]*wgts%*%data_x[i,])) + 1/var*wgts%*%t(wgts)
    
  }
  
  iters = iters + 1   # Update iteration value
  epoch = epoch - 1 
  
  if(epoch < 1){
    return(list("LogReg"=wgts,"ObjVal"=objVal[-1]))
  }else{
    return(trn_LogReg_SGD_MAP(trndata=trndata,wgts=wgts,epoch=epoch,var=var,gamma0=gamma0,d0=d0,labelInd=labelInd,iters=iters,objVal=objVal))
    
  }
  
}




trn_LogReg_SGD_MLE <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),epoch=10,gamma0=0.5,d0=gamma0,labelInd=ncol(trndata),iters=1,objVal=0){
  
  M_cnt    <- nrow(trndata)
  
  sampInds <- sample(1:M_cnt,M_cnt,replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  data_x <- cbind(rep(1,M_cnt),data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below (assume provided as 1 and 0)
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  lR <- gamma0/(1+(gamma0/d0)*iters)
  
  for(i in sampInds){
    
    y_sig <- as.numeric(labels[i] - sig_eval(wgts%*%data_x[i,])) # data_x dimensions flip for some reason, no need for t()
    
    dJ   <- wgts + M_cnt*y_sig*t(data_x[i,])
    
    wgts <- wgts - lR*dJ
    
    objVal[length(objVal)+1] = M_cnt*log(1+exp(-labels[i]*wgts%*%data_x[i,]))
    
  }
  
  iters = iters + 1   # Update iteration value
  epoch = epoch - 1 
  
  if(epoch < 1){
    return(list("LogReg"=wgts,"ObjVal"=objVal[-1]))
  }else{
    return(trn_LogReg_SGD_MLE(trndata=trndata,wgts=wgts,epoch=epoch,gamma0=gamma0,d0=d0,labelInd=labelInd,iters=iters,objVal=objVal))
    
  }
  
}




eval_LogReg_SGD <- function(testdata,LogReg_Wgts,labelInd=ncol(testdata)){
  
  M_cnt    <- nrow(testdata)
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  data_x <- cbind(rep(1,M_cnt),data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    preds[i] <- LogReg_Wgts %*% data_x[i,]
    
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}






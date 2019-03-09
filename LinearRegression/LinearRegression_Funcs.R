# Linear Regression Functions


# Libraries

dplyr_Flag <- require(dplyr)
# tidyverse_Flag <- library(tidyverse)

if(!dplyr_Flag){
  install.packages("dplyr")
}



# Functions

trn_batch_GradDsct <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),lrnRate=0.5,tol=1E-3,totCst=0,labelInd=ncol(trndata)){
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  gradJ_vec <- vector(mode="numeric",length=length(wgts))
  curr_err <- vector(mode="numeric",length=nrow(trndata))
  
  for(i in 1:nrow(trndata)){
    
    curr_err[i] <- as.numeric(labels[i] - wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata)))
    
    gradJ_vec <- gradJ_vec + curr_err[i]*as.numeric(c(1,data_x[i,]))
    
  }
  
  wgts_upd <- wgts + lrnRate*gradJ_vec
  totCst[length(totCst)+1] = 0.5*(sum(curr_err^2))
  
  if(norm(wgts,type="2")>1E100){
    return("Unstable")
  }else if(norm(wgts_upd - wgts,type="2") < tol){
    return(list("Weights"=wgts_upd,"TrnCst"=totCst[-1]))
  }else{
    return(trn_batch_GradDsct(trndata=trndata,wgts=wgts_upd,lrnRate=lrnRate,tol=tol,totCst=totCst,labelInd=labelInd))
    
  }
  
}




eval_batch_GradDsct <- function(testdata,gradDsct_wgts,labelInd=ncol(testdata)){
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    preds[i] <- gradDsct_wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(testdata))
    
    curr_err[i] <- as.numeric(labels[i] - preds[i])
    
  }
  
  totCst = 0.5*(sum(curr_err^2))
  
  return(list("SSE" = totCst, "Preds" = preds))
  
  
}




trn_stoch_GradDsct <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),lrnRate=0.5,tol=1E-3,totCst=0,labelInd=ncol(trndata)){
  
  sampInds <- sample(1:nrow(trndata),nrow(trndata),replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(trndata))
  
  ind = 1
  
  for(i in sampInds){
    
    curr_err[ind] <- as.numeric(labels[i] - wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata)))
    
    wgts_upd <- wgts + lrnRate*curr_err[ind]*as.numeric(c(1,data_x[i,]))
    totCst[length(totCst)+1] = 0.5*(sum(curr_err^2))
    
    if(abs(totCst[length(totCst)]-totCst[length(totCst)-1]) < tol & curr_err[i] < tol){
      return(list("Weights"=wgts_upd,"TrnCst"=totCst[-1]))
    }
    
    wgts <- wgts_upd
    
  }
  
  return(trn_stoch_GradDsct(trndata=trndata,wgts=wgts_upd,lrnRate=lrnRate,tol=tol,totCst=totCst,labelInd=labelInd))
  
  
}
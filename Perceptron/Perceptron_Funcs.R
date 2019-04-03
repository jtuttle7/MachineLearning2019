# Machine Learning Functions

# Librarys ----


# update.packages(old.packages())
data_tree_Flag <- require(data.tree)
dplyr_Flag <- require(dplyr)
# tidyverse_Flag <- library(tidyverse)

if(!data_tree_Flag){
  install.packages("data.tree")
  library(data.tree)
}
if(!dplyr_Flag){
  install.packages("dplyr")
}


# Functions ----

trn_stnd_Perceptron <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),lrnRate=0.5,epoch=10,labelInd=ncol(trndata)){
  
  sampInds <- sample(1:nrow(trndata),nrow(trndata),replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  for(i in sampInds){
    
    curr_ans <- wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata))
    
    if(curr_ans >= 0){curr_ans = 1} # Transform prediction to label of 1 or -1
    else{curr_ans = -1}
    
    if(labels[i]*curr_ans < 0){ # If wrong prediction, update weights
      wgts <- wgts + lrnRate*labels[i]*as.numeric(c(1,data_x[i,]))
    }
    
  }
  
  epoch = epoch - 1
  
  if(epoch < 1){
    return(wgts)
  }else{
    return(trn_stnd_Perceptron(trndata=trndata,wgts=wgts,lrnRate=lrnRate,epoch=epoch,labelInd=labelInd))
    
  }
  
}




eval_stnd_Perceptron <- function(testdata,stnd_wgt_Prcpt,labelInd=ncol(testdata)){
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    preds[i] <- stnd_wgt_Prcpt %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(testdata))
    
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}




trn_vote_Perceptron <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),lrnRate=0.5,epoch=10,trk_wgts=vector(mode="list",length=1),trk_cnts=0,labelInd=ncol(trndata)){
  
  trk_wgts[[length(trk_wgts)]] = wgts
  
  sampInds <- sample(1:nrow(trndata),nrow(trndata),replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  for(i in sampInds){
    
    curr_ans <- wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata))
    
    if(curr_ans >= 0){curr_ans = 1} # Transform prediction to label of 1 or -1
    else{curr_ans = -1}
    
    if(labels[i]*curr_ans < 0){ # If wrong prediction, update weights
      wgts <- wgts + lrnRate*labels[i]*as.numeric(c(1,data_x[i,]))
      trk_wgts[[length(trk_wgts)+1]] = wgts
      trk_cnts[length(trk_cnts)+1] = 1
    }else{
      trk_cnts[length(trk_cnts)] = trk_cnts[length(trk_cnts)] + 1
    }
    
  }
  
  epoch = epoch - 1
  
  if(epoch < 1){
    return(list("Wgt_Vecs"=trk_wgts,"Wgt_Counts"=trk_cnts))
  }else{
    return(trn_vote_Perceptron(trndata,wgts=wgts,lrnRate=lrnRate,epoch=epoch,trk_wgts=trk_wgts,trk_cnts=trk_cnts,labelInd=labelInd))
  }
}




eval_vote_Perceptron <- function(testdata,vote_Prcpt,labelInd=ncol(testdata)){
  
  wgt_vec <- vote_Prcpt$Wgt_Vecs
  wgt_cnts<- vote_Prcpt$Wgt_Counts
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    # Query all weight vectors and get their vote
    for(j in 1:length(wgt_cnts)){ 
      curr_ans <- wgt_vec[[j]] %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(testdata))
      
      if(curr_ans >= 0){curr_ans = 1} # Transform prediction to label of 1 or -1
      else{curr_ans = -1}
      
      preds[i] <- preds[i] + wgt_cnts[j]*curr_ans
    }
    
    # Reduce voted prediction to a label of 1 or -1
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}




trn_avg_Perceptron <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),lrnRate=0.5,epoch=10,avg_wgt=wgts,labelInd=ncol(trndata)){
  
  sampInds <- sample(1:nrow(trndata),nrow(trndata),replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  for(i in sampInds){
    
    curr_ans <- wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata))
    
    if(curr_ans >= 0){curr_ans = 1} # Transform prediction to label of 1 or -1
    else{curr_ans = -1}
    
    if(labels[i]*curr_ans < 0){ # If wrong prediction, update weights
      wgts <- wgts + lrnRate*labels[i]*as.numeric(c(1,data_x[i,]))
    }
    
    avg_wgt <- avg_wgt + wgts
    
  }
  
  epoch = epoch - 1
  
  if(epoch < 1){
    return(avg_wgt)
  }else{
    return(trn_avg_Perceptron(trndata=trndata,wgts=wgts,lrnRate=lrnRate,epoch=epoch,avg_wgt=avg_wgt,labelInd=labelInd))
    
  }
  
}




eval_avg_Perceptron <- function(testdata,avg_Prcpt,labelInd=ncol(testdata)){
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    preds[i] <- avg_Prcpt %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(testdata))
    
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}
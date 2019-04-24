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

trn_Prime_SVM_SGD <- function(trndata,wgts=matrix(0,nrow=1,ncol=ncol(trndata)),epoch=10,Cval=0.5,gamma0=0.5,d0=gamma0,labelInd=ncol(trndata),iters=1,objVal=0){
  
  sampInds <- sample(1:nrow(trndata),nrow(trndata),replace = F)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below (assume provided as 1 and 0)
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  lR <- gamma0/(1+(gamma0/d0)*iters)
  
  for(i in sampInds){
    
    curr_ans <- wgts %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(trndata))
    
    if(labels[i]*curr_ans <= 1){  # Within margin or wrong
      wgts <- (1-lR)*wgts + lR*Cval*nrow(trndata)*matrix(as.numeric(c(1,data_x[i,])),ncol=ncol(trndata))
    } 
    else{               # Correct classification
      wgts[-1] <- (1-lR)*wgts[-1]
    }
    
    objVal[length(objVal)+1] = 0.5*(t(wgts[-1]) %*% wgts[-1]) + Cval*nrow(trndata)*max(0,1-labels[i]*curr_ans)
    
  }
  
  iters = iters + 1   # Update iteration value
  epoch = epoch - 1 
  
  if(epoch < 1){
    return(list("SVM"=wgts,"ObjVal"=objVal))
  }else{
    return(trn_Prime_SVM_SGD(trndata=trndata,wgts=wgts,epoch=epoch,Cval=Cval,gamma0=gamma0,d0=d0,labelInd=labelInd,iters=iters,objVal=objVal))
    
  }
  
}




eval_Prime_SVM_SGD <- function(testdata,Prime_SVM_vec,labelInd=ncol(testdata)){
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  labels <- testdata[,labelInd]
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  for(i in 1:nrow(testdata)){
    
    preds[i] <- Prime_SVM_vec %*% matrix(as.numeric(c(1,data_x[i,])),nrow=ncol(testdata))
    
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}




trn_Dual_SVM <- function(trndata,Cval=1,Kernel="Direct",gamma=1/nrow(trndata),macPrec=1e-5,labelInd=ncol(trndata)){
  
  data_x  <- as.matrix(trndata[,-labelInd])
  labels  <- as.matrix(trndata[,labelInd])
  
  alphs   <- matrix(0,nrow=nrow(trndata),ncol=1)
  
  I_vec   <- matrix(1.0,nrow=1,ncol=nrow(trndata))
  
  y       <- labels
  
  y[]     <- sapply(y, as.numeric)
  
  
  
  if(Kernel=="Direct"){
    
    Kmat  <- data_x %*% t(data_x)
    
  }else if(Kernel=="Gaussian"){
    
    Kmat = matrix(0,nrow=nrow(data_x),ncol=nrow(data_x))
    
    for(i in 1:nrow(data_x)){
      for(j in 1:nrow(data_x)){
        
        Kmat[i,j] <- exp(-(norm(data_x[i,]-data_x[j,],type="2")^2/gamma))
        
      }
    }
  }
  
  
  
  # Define Objective Function
  
  eval_f <- function( x, y, I_vec, Kmat){
    
    x_sqmat <- x %*% I_vec
    
    y_sqmat <- y %*% I_vec
    
    obj <- 0.5*sum(y_sqmat%*%t(y_sqmat) * x_sqmat%*%t(x_sqmat) * Kmat) - sum(x)
    
    return(obj)
  }
  
  
  # Define Gradient of Objective Function
  
  eval_grad_f <- function( x, y, I_vec, Kmat){
    
    y_sqmat <- y %*% I_vec
    
    grad <- (y_sqmat%*%t(y_sqmat) * Kmat) %*% x  - 1
    
    return(grad)
    
  }
  
  
  
  # Define Equality Constraint
  
  eval_g_eq <- function( x, y, I_vec, Kmat){
    return(sum(x*y))
  }
  
  
  
  # Define Jacobian of Equality
  
  eval_jac_g_eq <- function( x, y, I_vec, Kmat){
    return(y)
  }
  
  
  
  
  # Solve Optimization Problem
  
  res <- nloptr(x0            = alphs,
                eval_f        = eval_f,
                eval_grad_f   = eval_grad_f,
                lb            = rep(0,nrow(trndata)),
                ub            = rep(Cval,nrow(trndata)),
                eval_g_eq     = eval_g_eq,
                eval_jac_g_eq = eval_jac_g_eq,
                opts          = list("algorithm"="NLOPT_LD_SLSQP","xtol_rel"=1.0e-6),
                y             = y,
                I_vec         = I_vec,
                Kmat          = Kmat)

  
  slvd_alphas <- res$solution
  
  act_alphas_ind <- slvd_alphas > macPrec
  
  SV_alphas <- slvd_alphas[act_alphas_ind]
  
  trn_SV <- trndata[act_alphas_ind,]
  
  return(list("SV_alphas"=SV_alphas,"trn_SV"=trn_SV,"OptRes"=res,"SV_inds"=which(slvd_alphas > macPrec)))
  
  
}




pred_dual_SVM <- function(features,dual_trn_list,Kernel="Direct"){
  
  ftrs <- matrix(features,ncol=1)
  
  trn_alphs <- as.matrix(dual_trn_list$SV_alphas)
  
  trn_SV    <- as.matrix(dual_trn_list$trn_SV[,-ncol(dual_trn_list$trn_SV)])
  
  trn_labs  <- as.matrix(dual_trn_list$trn_SV[,ncol(dual_trn_list$trn_SV)])
  
  
  if(Kernel=="Direct"){
    
    Kmat  <- trn_SV %*% ftrs
    
  }else if(Kernel=="Gaussian"){
    
    Kmat = matrix(0,nrow=nrow(trn_SV),ncol=1)
    
    for(k in 1:nrow(Kmat)){
      Kmat[k] <- exp(-(norm(trn_SV[k,]-ftrs,type="2")^2/gamma))
    }
    
  }
  
  preds <- sum(trn_alphs*trn_labs*Kmat)
    
  if(preds >= 0){preds = 1} # Transform prediction to label of 1 or -1
  else{preds = -1}
  
  return(preds)
  
}
  



eval_dual_SVM <- function(tstdata,dual_trn_list,Kernel="Direct",labelInd=ncol(tstdata)){

  data_x <- as.matrix(tstdata[,-labelInd])
  labels <- as.matrix(tstdata[,labelInd])
  
  curr_err <- vector(mode="numeric",length=nrow(tstdata))
  preds    <- curr_err
  
  for(i in 1:nrow(tstdata)){
    
    preds[i] <- pred_dual_SVM(data_x[i,],dual_trn_list,Kernel=Kernel)
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(tstdata)
  
  return(list("Error" = totErr, "Preds" = preds))
  
}






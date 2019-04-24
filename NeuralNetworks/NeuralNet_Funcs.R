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




NN_trn <- function(trndata,hid_struct=c(25,25),Wgt_Init="StdNorm",epoch=10,gamma0=0.5,d0=gamma0,labelInd=ncol(trndata)){
  
  M_cnt    <- nrow(trndata)
  
  data_x <- trndata[,-labelInd]
  data_x <- as.matrix(data_x)
  data_x <- cbind(rep(1,M_cnt),data_x)
  labels <- trndata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below (assume provided as 1 and 0)
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  
  # Build matrix structures
  
  aug_hid <- hid_struct + 1
  
  tot_struct <- c(ncol(data_x),aug_hid,1)
  
  # Network Weights
  wgtList <- vector(mode="list",length=length(tot_struct)-1)
  
  for(i in 1:length(wgtList)){
    
    if(Wgt_Init=="StdNorm"){
      wgtList[[i]] <- matrix(rnorm(n=tot_struct[i+1]*tot_struct[i]),ncol=tot_struct[i+1],nrow=tot_struct[i])
    }else{
      wgtList[[i]] <- matrix(0,ncol=tot_struct[i+1],nrow=tot_struct[i])
    }

  }
  
  
  # Derivative List Matrix mirrors Weights
  derivWgtList <- wgtList
  
  # Node Sums
  nodeSums <- vector(mode="list",length=length(tot_struct))
  
  
  for(i in 1:(length(nodeSums))){
    nodeSums[[i]] = vector(mode="numeric",length=tot_struct[i])
  }
  
  
  # Node Outputs
  nodeOuts <- nodeSums
  
  # Ability to track combined derivatives down net
  cmbd_Derivs <- nodeOuts
  
  
  iters = 1
  
  return(NN_forBck(trndata=trndata,data_x=data_x,labels=labels,wgtList=wgtList,derivWgtList=derivWgtList,cmbd_Derivs=cmbd_Derivs,nodeSums=nodeSums,nodeOuts=nodeOuts,iters=iters,epoch=epoch,gamma0=gamma0,d0=d0,tot_struct=tot_struct))
  
  
  
  
  
  
}




NN_forBck <- function(trndata,data_x,labels,wgtList,derivWgtList,cmbd_Derivs,nodeSums,nodeOuts,iters,epoch,gamma0,d0,tot_struct){
  
  M_cnt    <- nrow(trndata)
  
  sampInds <- sample(1:M_cnt,M_cnt,replace = F)
  
  lR <- gamma0/(1+(gamma0/d0)*iters)
  
  
  for(i in sampInds){
    
    # Forward Pass
    
    nodeOuts[[1]] = data_x[i,]
    
    for(lay in 2:length(nodeOuts)){
      
      nodeSums[[lay]][1] <- 1
      
      if(lay == length(nodeOuts)){
        nde = 1
        nodeSums[[lay]][nde] <- as.numeric(t(as.matrix(nodeOuts[[lay-1]])) %*% as.matrix(wgtList[[lay-1]][,nde]))
      }else{
      
        for(nde in 2:length(nodeSums[[lay]])){
          
          nodeSums[[lay]][nde] <- as.numeric(t(as.matrix(nodeOuts[[lay-1]])) %*% as.matrix(wgtList[[lay-1]][,nde]))
        }
        
      }
      
      if(lay == length(nodeOuts)){ # Linear Output
        nodeOuts[[lay]] <- nodeSums[[lay]]
      }else{
        nodeOuts[[lay]][1] <- 1
        for(nde in 2:length(nodeOuts[[lay]])){ # Hidden Activation = Sigmoid
          
          nodeOuts[[lay]][nde] <- sig_eval(nodeSums[[lay]][nde])
    
        }
      }
      
    }
    
    
    
    # Back Prop 
    
    cmbd_Derivs <- lapply(cmbd_Derivs,"*",0) # Reset combined derivatives for back prop
    
    dLdy <- nodeOuts[[length(nodeOuts)]][1] - labels[i]
    
    for(lay in length(nodeOuts):2){
    
      for(ndeFrom in 1:tot_struct[lay-1]){
        if(lay == length(nodeOuts)){
          # Treat Output Weights
          derivWgtList[[lay-1]][ndeFrom,1] <- dLdy*nodeOuts[[lay-1]][ndeFrom]
          cmbd_Derivs[[lay-1]][ndeFrom] <- dLdy*wgtList[[lay-1]][ndeFrom,1]
          
        }else{
          # Treat Hidden Layer Weights
          for(ndeTo in 2:tot_struct[lay]){
            derivWgtList[[lay-1]][ndeFrom,ndeTo] <- cmbd_Derivs[[lay]][ndeTo]*(sig_eval(nodeSums[[lay]][ndeTo])*(1-sig_eval(nodeSums[[lay]][ndeTo])))*nodeOuts[[lay-1]][ndeFrom]
            cmbd_Derivs[[lay-1]][ndeFrom] <- cmbd_Derivs[[lay-1]][ndeFrom] + cmbd_Derivs[[lay]][ndeTo]*(sig_eval(nodeSums[[lay]][ndeTo])*(1-sig_eval(nodeSums[[lay]][ndeTo])))*wgtList[[lay-1]][ndeFrom,ndeTo]
            
            
          }
          
        }
      
      }
    
    
    # Update all weights in this layer
    
    wgtList[[lay-1]] <- wgtList[[lay-1]] - lR*derivWgtList[[lay-1]]
    
    }
    
  }
    
  
  iters = iters + 1   # Update iteration value
  epoch = epoch - 1 
  
  if(epoch < 1){
    return(wgtList)
  }else{
    return(NN_forBck(trndata=trndata,data_x=data_x,labels=labels,wgtList=wgtList,derivWgtList=derivWgtList,cmbd_Derivs=cmbd_Derivs,nodeSums=nodeSums,nodeOuts=nodeOuts,iters=iters,epoch=epoch,gamma0=gamma0,d0=d0,tot_struct=tot_struct))
    
  }
  
  
  
}




pred_NN <- function(testdata,trnd_NN,labelInd=ncol(testdata)){
  
  M_cnt    <- nrow(testdata)
  
  data_x <- testdata[,-labelInd]
  data_x <- as.matrix(data_x)
  data_x <- cbind(rep(1,M_cnt),data_x)
  labels <- testdata[,labelInd]
  
  # Turn labels into 1 and -1 for calculations below (assume provided as 1 and 0)
  
  inds_1 <- labels == 1
  
  labels[!inds_1] <- -1
  
  curr_err <- vector(mode="numeric",length=nrow(testdata))
  preds    <- curr_err
  
  wgtList <- trnd_NN
  
  # Node Sums
  nodeSums <- vector(mode="list",length=length(wgtList)+1)
  
  
  nodeSums[[1]] = vector(mode="numeric",length=ncol(data_x))
  
  for(i in 2:(length(nodeSums)-1)){
    nodeSums[[i]] = vector(mode="numeric",length=nrow(wgtList[[i]]))
  }
  nodeSums[[length(nodeSums)]] <- 0
  
  
  # Node Outputs
  nodeOuts <- nodeSums
  
  
  
  for(i in 1:M_cnt){
    
    
    # Forward Pass
    
    nodeOuts[[1]] = data_x[i,]
    
    for(lay in 2:length(nodeOuts)){
      
      nodeSums[[lay]][1] <- 1
      
      if(lay == length(nodeOuts)){
        nde = 1
        nodeSums[[lay]][nde] <- as.numeric(t(as.matrix(nodeOuts[[lay-1]])) %*% as.matrix(wgtList[[lay-1]][,nde]))
      }else{
        
        for(nde in 2:length(nodeSums[[lay]])){
          
          nodeSums[[lay]][nde] <- as.numeric(t(as.matrix(nodeOuts[[lay-1]])) %*% as.matrix(wgtList[[lay-1]][,nde]))
        }
        
      }
      
      if(lay == length(nodeOuts)){ # Linear Output
        nodeOuts[[lay]] <- nodeSums[[lay]]
      }else{
        nodeOuts[[lay]][1] <- 1
        for(nde in 2:length(nodeOuts[[lay]])){ # Hidden Activation = Sigmoid
          
          nodeOuts[[lay]][nde] <- sig_eval(nodeSums[[lay]][nde])
          
        }
      }
      
    }
    
    
    
    preds[i] <- nodeOuts[[length(nodeOuts)]][1]
    
    if(preds[i] >= 0){preds[i] = 1} # Transform prediction to label of 1 or -1
    else{preds[i] = -1}
    
    if(labels[i]*preds[i] < 0){ # Record error
      curr_err[i] = 1
    }
    
  }
  
  totErr = sum(curr_err)/nrow(testdata)
  
  return(list("Error" = totErr, "Preds" = preds))
    
}

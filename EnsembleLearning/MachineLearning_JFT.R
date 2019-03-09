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

info_gain <- function(S_purity,weights,subS_purity){
  
  subS_sum <- 0
  
  for(i in 1:length(subS_purity)){
    
    subS_sum <- subS_sum + weights[i]*subS_purity[i]
    
  }
  
  gain <- S_purity - subS_sum
  
  return(round(gain,4))
  
  
}




entrpy_col <- function(data_col,d_labels,wgts = NA){
  
  if(is.na(sum(wgts))){
    wgts <- rep(1,length(d_labels))/length(d_labels)
  }
  
  if(is.factor(data_col)){
    data_col <- sapply(data_col,as.character)
  }
  
  if(is.factor(d_labels)){
    d_labels <- sapply(d_labels,as.character)
  }
  
  # Find Unique observations of Attribute
  uq_obs <- unique(data_col)
  
  # Find unique labels
  uq_labs <- unique(d_labels)
  
  # Combine into small matrix
  comb_d_l <- cbind(data_col,d_labels)
  
  
  # Separate data and labels into distinct attributes
  
  subsets    <- vector(mode="list",length=length(uq_obs))
  subset_wgt <- vector(mode="list",length=length(uq_obs))
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = comb_d_l[sav,]
    subset_wgt[[i]] = wgts[sav]
  }
  
  # Initialize output
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  entrpy_subs <- weights
  
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    weights[i] = sum(subset_wgt[[i]])/sum(wgts)
  }
  
  
  # Compute entropy of each unique observation
  
  for(i in 1:length(entrpy_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    curr_subset_wgt <- subset_wgt[[i]]
    tot_wgtsubs <- sum(curr_subset_wgt)
    
    # Find number of each unique label within each subset and calcualte entropy
    for(j in 1:length(uq_labs)){
      same_lab <- tryCatch(curr_subset[,2]==uq_labs[j],error=function(e){return(curr_subset[2]==uq_labs[j])})
      
      num_uq_labs[j] <- sum(curr_subset_wgt[same_lab])
      
      entrpy_subs[i] <- entrpy_subs[i] - (num_uq_labs[j]/tot_wgtsubs)*log((num_uq_labs[j]/tot_wgtsubs),2)
      
      if(is.na(entrpy_subs[i])){
        entrpy_subs[i] <- 0
      }
    }
    
  }
  
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  entrpy_all <- 0
  
  # Find total entropy of attribute
  for(i in 1:length(uq_labs)){
    same_lab <- d_labels==uq_labs[i]
    num_uq_labs[i] <- sum(wgts[same_lab])
    
    entrpy_all <- entrpy_all - (num_uq_labs[i]/sum(wgts))*log((num_uq_labs[i]/sum(wgts)),2)
    
    if(is.na(entrpy_all)){
      entrpy_all <- 0
    }
  }
  
  
  
  
  return(list("pur_all"=entrpy_all,"weights"=weights,"pur_subs"=entrpy_subs))
  
}




Mj_Error <- function(data_col,d_labels,wgts = NA){
  
  if(is.na(sum(wgts))){
    wgts <- rep(1,length(d_labels))/length(d_labels)
  }
  
  if(is.factor(data_col)){
    data_col <- sapply(data_col,as.character)
  }
  
  if(is.factor(d_labels)){
    d_labels <- sapply(d_labels,as.character)
  }
  
  # Find Unique observations of Attribute
  uq_obs <- unique(data_col)
  
  # Find unique labels
  uq_labs <- unique(d_labels)
  
  # Combine into small matrix
  comb_d_l <- cbind(data_col,d_labels)
  
  
  # Separate data and labels into distinct attributes
  
  subsets <- vector(mode="list",length=length(uq_obs))
  subset_wgt <- subsets
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = comb_d_l[sav,]
    subset_wgt[[i]] = wgts[sav]
  }
  
  # Initialize output
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  maj_err_subs <- weights
  
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    weights[i] = sum(subset_wgt[[i]])
  }
  
  
  # Compute majority error of each unique observation
  
  for(i in 1:length(maj_err_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    curr_subset_wgt <- subset_wgt[[i]]
    tot_wgtsubs <- sum(curr_subset_wgt)
    
    # Find number of each unique label within each subset
    for(j in 1:length(uq_labs)){
      
      same_lab <- tryCatch(curr_subset[,2]==uq_labs[j],error=function(e){return(curr_subset[2]==uq_labs[j])})
      
      num_uq_labs[j] <- sum(curr_subset_wgt[same_lab])
    }
    
    # Calculate majority error
    maj_err_subs[i] <- (tot_wgtsubs - max(num_uq_labs))/tot_wgtsubs
    
  }
  
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  # Find majority error of attribute
  for(i in 1:length(uq_labs)){
    same_lab <- d_labels==uq_labs[i]
    
    num_uq_labs[i] <- sum(wgts[same_lab])
  }
  
  maj_err_all <- (sum(wgts) - max(num_uq_labs))/sum(wgts)
  
  
  
  return(list("pur_all"=maj_err_all,"weights"=weights,"pur_subs"=maj_err_subs))
  
}




gini <- function(data_col,d_labels,wgts = NA){
  
  if(is.na(sum(wgts))){
    wgts <- rep(1,length(d_labels))/length(d_labels)
  }
  
  if(is.factor(data_col)){
    data_col <- sapply(data_col,as.character)
  }
  
  if(is.factor(d_labels)){
    data_col <- sapply(d_labels,as.character)
  }
  
  # Find Unique observations of Attribute
  uq_obs <- unique(data_col)
  
  # Find unique labels
  uq_labs <- unique(d_labels)
  
  # Combine into small matrix
  comb_d_l <- cbind(data_col,d_labels)
  
  
  # Separate data and corresponding labels into distinct attributes
  
  subsets <- vector(mode="list",length=length(uq_obs))
  subset_wgt <- subsets
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = comb_d_l[sav,]
    subset_wgt[[i]] = wgts[sav]
  }
  
  # Initialize output
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  gini_subs <- weights
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    weights[i] = sum(subset_wgt[[i]])
  }
  
  
  # Compute GINI Index of each unique observation
  
  for(i in 1:length(gini_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    curr_subset_wgt <- subset_wgt[[i]]
    tot_wgtsubs <- sum(curr_subset_wgt)
    
    # Find number of each unique label within each subset
    for(j in 1:length(uq_labs)){
      
      same_lab <- tryCatch(curr_subset[,2]==uq_labs[j],error=function(e){return(curr_subset[2]==uq_labs[j])})
      
      num_uq_labs[j] <- sum(curr_subset_wgt[same_lab])
    }
    
    # Calculate GINI
    
    gini_sum <- sum((num_uq_labs/tot_wgtsubs)^2)
    
    gini_subs[i] <- 1 - gini_sum
    
  }
  
  # Find GINI of attribute
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  for(i in 1:length(uq_labs)){
    same_lab <- d_labels==uq_labs[i]
    num_uq_labs[i] <- sum(wgts[same_lab])
  }
  
  gini_sum <- sum((num_uq_labs/sum(wgts))^2)
  
  gini_all <- 1 - gini_sum
  
  
  
  return(list("pur_all"=gini_all,"weights"=weights,"pur_subs"=gini_subs))
  
}




entrpy <- function(probs){
  
  if(is.na(sum(probs))){
    return(0)
  }
  
  val <- 0
  
  for(i in 1:length(probs)){
    
    if(probs[i] == 0){
      
    }else{
      val <- val + probs[i]*log(probs[i],2)
    }
    
  }
  
  result <- -val
  
  return(result)
  
}




trn_ID3 <- function(treeVar,data,depth,PurityMethod="Entropy",missingData_ID=NA,numericColInds=0,wgts=NA,ftr_splt_sz=NA,labelInd=ncol(data),levelName="Base",firstCall=1){
  
  # If no weights provided, make everything equal
  
  if(is.na(sum(wgts))){
    wgts <- rep(1,nrow(data))/nrow(data)
  }
  
  
  # Do some preprocessing on training data for missing data and numeric input columns
  if(firstCall){
    
    # Address missing information. Replace with majority value of that attribute
    
    treeVar$mostCom_eachAtt <- vector(mode="character",length=ncol(data)) # Store most common attribute from training set
    
    for(i in 1:ncol(data)){
      
      uq_atts <- unique(data[,i])
      num_uq_atts <- vector(mode="numeric",length=length(uq_atts))
      
      for(j in 1:length(uq_atts)){
        num_uq_atts[j] = sum(data[,i]==uq_atts[j])
      }
      
      srt <- sort(num_uq_atts,index.return=T,decreasing=T)
      
      com2not <- srt$ix
      commInd <- 1
      
      while(uq_atts[com2not[commInd]] %in% missingData_ID){
        commInd <- commInd + 1
      }
      
      data[(data[,i] %in% missingData_ID),i] = uq_atts[com2not[commInd]]
      treeVar$mostCom_eachAtt[i] <- uq_atts[com2not[commInd]]
      
    }
    
    
    
    treeVar$medianVal <- vector(mode="numeric",length=length(numericColInds)) # Store median value from training set
    
    # If numeric, separate based on being above or below the median value of column
    if(numericColInds[1]){
      
      for(i in 1:length(numericColInds)){
        
        rplCol = as.numeric(data[,numericColInds[i]])
        
        abvMd <- rplCol > median(rplCol)
        data[abvMd,numericColInds[i]] = paste(">",median(rplCol))
        data[!abvMd,numericColInds[i]] = paste("<=",median(rplCol))
        treeVar$medianVal[i] = median(rplCol)
        
      }
      
    }
  }
  
  
  # Convert data into matrix class to avoid issues with referencing factors
  # Will turn everything into character class (if any character entry anywhere in dataset), so don't try doing math on numbers now. If need to, will have to address factors differently (use data.frame somehow)
  data = as.matrix(data)
  
  
  # Transform numeric data into binary classification using median as transfer point
  
  
  # Determine which purity calculation method we're using
  if(PurityMethod=="Entropy"){
    pur_calc <- function(data_col,d_labels,wgts=NA){
      entrpy_col(data_col,d_labels,wgts=wgts)
    }
  }else if(PurityMethod=="Majority Error"){
    pur_calc <- function(data_col,d_labels,wgts=NA){
      Mj_Error(data_col,d_labels,wgts=wgts)
    }
  }else if(PurityMethod=="GINI"){
    pur_calc <- function(data_col,d_labels,wgts=NA){
      gini(data_col,d_labels,wgts=wgts)
    }
  }
  
  
  # Split out data and labels
  curr_data <- data[,-labelInd]
  curr_labs <- data[,labelInd]
  
  if(is.null(ncol(curr_data))){
    last_att = 1
  }else{
    last_att = 0
  }
  
  
  # Identify unique labels at this point
  uq_labs <- unique(curr_labs)
  
  
  # Save Splitting Variable of the Current Branch
  treeVar$splitVar <- levelName
  
  
  # If only one label exists, create leaf node
  if(length(uq_labs) == 1){
    treeVar$AddChild(as.character(uq_labs[1]))
    
    
  }else if(depth==1){ # If reached maximum user-specified depth, create leaf node with majority label
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    for(i in 1:length(num_uq_labs)){
      
      curr_labs_inds <- curr_labs==uq_labs[i]
      
      num_uq_labs[i] <- sum(wgts[curr_labs_inds])
      
    }
    
    maj_lab_ind <- which.max(num_uq_labs)
    
    treeVar$AddChild(as.character(uq_labs[maj_lab_ind]))
    
    
  }else{
    
    # If the last attribute to split on, create new node, and new node beneath that, one for attribute, then the last to identify label
    
    if(last_att){
      
      for(i in 1:length(curr_data)){
        
        firstChild <- treeVar$AddChild(curr_data[i])
        leaf <- firstChild$AddChild(as.character(curr_labs[i]))
        
      }
      
      
    }else{
      
      cols2use <- seq(1,ncol(curr_data),by=1)
      
      # Resample features to use if training random forest
      if(!is.na(ftr_splt_sz)){
        if(ncol(curr_data) < ftr_splt_sz){}
        else{
          cols2use <- sample(1:ncol(curr_data),ftr_splt_sz,replace=F)
        }
      }
      
      # Place to store info gain of each attribute for this node
      treeVar$ig <- vector(mode="numeric",length=ncol(curr_data))#col_curr_data)
      
      for(i in cols2use){
        puritys <- pur_calc(curr_data[,i],curr_labs,wgts=wgts)
        treeVar$ig[i] <- info_gain(puritys$pur_all,puritys$weights,puritys$pur_subs)
      }
      
      # Find column of maximum info gain
      max_ig_ind <- which.max(treeVar$ig)
      
      # Identify Attributes in that column
      uq_split_atts <- unique(curr_data[,max_ig_ind])
      
      
      # Store most common observation of this attribute in case of missing data
      # This sets up re-routing variable to choose most common observation of this attribute for when prediction occurs (treeVar$mostCommAtt)
      num_uq_atts <- vector(mode="numeric",length=length(uq_split_atts))
      
      for(i in 1:length(num_uq_atts)){
        
        comAttWgt <- curr_labs==uq_split_atts[i]
        
        num_uq_atts[i] <- sum(wgts[comAttWgt])
        
      }
      
      mostComm_ind <- which.max(num_uq_atts)
      
      treeVar$mostCommAtt <- uq_split_atts[mostComm_ind]
      
      
      
      # Split data up by those attributes and recursively call trainer to get down to leaf node or max depth
      for(i in 1:length(uq_split_atts)){
        
        
        keepInds_split <- curr_data[,max_ig_ind] == uq_split_atts[i]
        
        keepData <- curr_data[keepInds_split,-max_ig_ind]
        keepLabs <- curr_labs[keepInds_split]
        keepWgts <- wgts[keepInds_split]
        
        keepFull <- cbind(keepData,keepLabs)
        
        newdepth = depth - 1
        
        newNode <- treeVar$AddChild(uq_split_atts[i])
        
        trn_ID3(newNode,keepFull,newdepth,wgts=keepWgts,ftr_splt_sz=ftr_splt_sz,PurityMethod=PurityMethod,levelName=colnames(curr_data)[max_ig_ind],firstCall=0)
        
        
      }
      
    }
    
  }
  
  
}




predict_ID3 <- function(tree,features,numericOut=F,missingData_ID=NA){
  
  # Arrived at point where next branch gives label, return that label
  
  if(tree$children[[1]]$isLeaf){
    if(numericOut){
      return(as.numeric(tree$children[[1]]$name))
    }else{
      return(tree$children[[1]]$name)
    }
  }
  
  # Grab appropriate child as designated by the feature name (determined from what the next child was split on) and corresponding feature in input observation
  child <- tree$children[[features[[tree$children[[1]]$splitVar]]]]
  
  
  # If the child it called is null (never trained on or doesn't exist), or it matches the ID specified by the user of missing data, grab most common attribute tree
  if(is.null(child) | is.na(features[[tree$children[[1]]$splitVar]])){
    child <- tree$children[[tree$mostCommAtt]]
  }else if(length(missingData_ID)>1){
    if(features[[tree$children[[1]]$splitVar]] %in% missingData_ID){
      child <- tree$children[[tree$mostCommAtt]]
    }
  }
  
  # Recursively call until arrive at leaf node
  return(predict_ID3(child,features,numericOut))
  
}




test_ID3 <- function(tree,testdata,missingData_ID=NA,numericColInds=0,numericOut=F,labelInd=ncol(testdata)){
  
  
  # Preprocessing of test data to match structure of tree as determiend by training set
  
  
  
  # Address missing information. Replace with majority value of that attribute from training set
  
  for(i in 1:ncol(testdata)){
    
    testdata[(testdata[,i] %in% missingData_ID),i] = tree$mostCom_eachAtt[i]
    
  }
  
  
  # If numeric, separate based on being above or below the median value of column from training set
  if(numericColInds[1]){
    
    for(i in 1:length(numericColInds)){
      
      rplCol = as.numeric(testdata[,numericColInds[i]])
      
      abvMd <- rplCol > tree$medianVal[i]
      testdata[abvMd,numericColInds[i]] = paste(">",tree$medianVal[i])
      testdata[!abvMd,numericColInds[i]] = paste("<=",tree$medianVal[i])
      
    }
    
  }
  
  
  
  if(is.null(try(nrow(testdata)))){
    testLength = 1
  }else{
    testLength=nrow(testdata)
  }
  
  # Create a vector to store predictions in
  if(numericOut){
    ID3_Predictions <- vector(mode="numeric",length=testLength)
  }else{
    ID3_Predictions <- vector(mode="character",length=testLength)
  }
  
  
  # Move through all test data and predict label
  for(i in 1:length(ID3_Predictions)){
    
    curr_features = testdata[i,]
    
    if(is.factor(curr_features)){
      curr_features=sapply(curr_features,as.character)
    }
    
    ID3_Predictions[i]=predict_ID3(tree=tree, features=curr_features, numericOut=numericOut, missingData_ID=missingData_ID)
    
  }
  
  # Calculate Prediction Error of Model
  predError <- (length(ID3_Predictions) - sum(ID3_Predictions==testdata[,labelInd]))/length(ID3_Predictions)
  
  return(list("Error"=predError,"Preds"=ID3_Predictions))
  
}




ErrAlong_Trees <- function(testdata,models,numericColInds=0,missingData_ID=NA,labelInd=ncol(testdata)){
  
  # Error Computation
  
  totErr <- vector(mode="numeric",length=length(models))
  
  uq_labs = unique(testdata[,labelInd])
  
  uq_labs_curSum <- vector(mode="numeric",length=length(uq_labs))
  
  
  # List to store vectors for each row of training set weighted answer votes
  list_Sums <- vector(mode="list",length=nrow(testdata))
  
  curPreds <- vector(length=nrow(testdata))
  
  # Vectors in each space of list initialized to 0
  for(i in 1:length(list_Sums)){
    list_Sums[[i]] = uq_labs_curSum
  }
  
  
  # Move each tree through all dataset rows and store preds
  for(i in 1:length(models)){
    for(j in 1:nrow(testdata)){
      
      uq_labs_curSum = list_Sums[[j]]
      
      # Determine prediction of each row from each tree
      curResult = test_ID3(tree=models[[i]],testdata=testdata[j,],missingData_ID=missingData_ID,numericColInds=numericColInds)
      
      # Store the current prediction value
      curPred <- curResult$Preds
      
      # Identify which index the prediction is for vote tracking
      labID <- which(uq_labs==curPred)
      
      # Increase vote of current winner
      uq_labs_curSum[labID] = uq_labs_curSum[labID] + 1
      # Update this rows weighted votes
      list_Sums[[j]] = uq_labs_curSum
      # Store boosted tree prediction for this iteration
      curPreds[j] = uq_labs[which.max(uq_labs_curSum)]
      
    }
    
    # Now moved through all rows with one tree, determine error of this stump on whole dataset, and error of boosted tree to this point
    
    totErr[i] = sum(curPreds!=testdata[,ncol(testdata)])/nrow(testdata)
    
  }
  
  return(totErr)
  
  
  
}




ErrAlongRndFrst <- function(testdata,models,numericColInds=0,missingData_ID=NA,labelInd=ncol(testdata)){
  
  # Error Computation
  
  totErr <- vector(mode="numeric",length=length(models))
  
  uq_labs = unique(testdata[,labelInd])
  
  uq_labs_curSum <- vector(mode="numeric",length=length(uq_labs))
  
  
  # List to store vectors for each row of training set weighted answer votes
  list_Sums <- vector(mode="list",length=nrow(testdata))
  
  curPreds <- vector(length=nrow(testdata))
  
  # Vectors in each space of list initialized to 0
  for(i in 1:length(list_Sums)){
    list_Sums[[i]] = uq_labs_curSum
  }
  
  
  # Move each tree through all dataset rows and store preds
  for(i in 1:length(models)){
    for(j in 1:nrow(testdata)){
      
      uq_labs_curSum = list_Sums[[j]]
      
      # Determine prediction of each row from each tree
      curResult = RandForst_ID3_tst(randForst_Model=models[i],testdata=testdata[j,],missingData_ID=missingData_ID,numericColInds=numericColInds)
      
      # Store the current prediction value
      curPred <- curResult$Preds
      
      # Identify which index the prediction is for vote tracking
      labID <- which(uq_labs==curPred)
      
      # Increase vote of current winner
      uq_labs_curSum[labID] = uq_labs_curSum[labID] + 1
      # Update this rows weighted votes
      list_Sums[[j]] = uq_labs_curSum
      # Store boosted tree prediction for this iteration
      curPreds[j] = uq_labs[which.max(uq_labs_curSum)]
      
    }
    
    # Now moved through all rows with one tree, determine error of this stump on whole dataset, and error of boosted tree to this point
    
    totErr[i] = sum(curPreds!=testdata[,ncol(testdata)])/nrow(testdata)
    
  }
  
  return(totErr)
  
  
  
}




adaBoost_trn <- function(trn_data,iters,numericColInds=0,PurityMethod="Entropy",missingData_ID=NA,labelInd=ncol(trn_data)){
  
  # Initialize first even weights, and an alpha vector
  wgts <- rep(1,nrow(trn_data))/nrow(trn_data)
  alph <- vector(mode="numeric",length=iters)
  errs <- alph
  
  stump_list <- vector(mode="list",length=iters)
  
  for(i in 1:iters){
    
    stump_list[[i]] <- Node$new(paste0("stump",i))
    
    # Train stump with current weights
    trn_ID3(treeVar=stump_list[[i]],data=trn_data,depth=2,PurityMethod=PurityMethod,missingData_ID=missingData_ID,numericColInds=numericColInds,wgts=wgts,labelInd=labelInd)
    
    # Determine train error
    curr_tst_res <- test_ID3(tree=stump_list[[i]],testdata=trn_data,missingData_ID=missingData_ID,numericColInds=numericColInds,numericOut=F,labelInd=labelInd)
    
    # Model predictions for each feature
    curr_preds <- curr_tst_res$Preds
    
    # Identify correct and incorrect predictions
    corr <- curr_preds == trn_data[,labelInd]
    incorr <- curr_preds != trn_data[,labelInd]
    
    # Determine Weighted Error
    wgt_err <- sum(wgts[incorr])/sum(wgts)
    if(wgt_err==0.5){wgt_err = wgt_err + runif(1,-0.01,0.01)}
    errs[i] <- wgt_err
    
    # Compute current alpha value ("log" is natural log)
    alph[i] = (1/2)*log((1-wgt_err)/wgt_err)
    
    # Update weights
    wgts[corr] <- wgts[corr] * exp(-alph[i])
    wgts[incorr] <- wgts[incorr] * exp(alph[i])
    
    # Normalize weights
    Z = sum(wgts)
    wgts <- wgts/Z
    
    
  }
  
  return(list("Trees"=stump_list,"alphas"=alph,"wgt_err"=errs))
  
  
}




adaBoost_pred <- function(adaBoost_ModelList,features,numericOut=F,missingData_ID=NA){
  
  stumpList <- adaBoost_ModelList$Trees
  alphas <- adaBoost_ModelList$alphas
  preds <- vector(length=length(alphas))
  
  for(i in 1:length(alphas)){
    
    preds[i] = predict_ID3(tree=stumpList[[i]],features=features,numericOut=numericOut,missingData_ID=missingData_ID)
    
  }
  
  uq_labs <- unique(preds)
  wgt_sum <- vector(mode="numeric",length=length(uq_labs))
  
  for(i in 1:length(uq_labs)){
    
    same_lab <- preds == uq_labs[i]
    wgt_sum[i] <- sum(alphas[same_lab])
    
  }
  
  winner <- which.max(wgt_sum)
  
  return(uq_labs[winner])
  
  
}




adaBoost_tst <- function(adaBoost_ModelList,testdata,numericColInds=0,missingData_ID=NA,numericOut=F,labelInd=ncol(testdata)){
  
  
  # Preprocessing of test data to match structure of tree as determined by training set
  
  # Address missing information. Replace with majority value of that attribute from training set
  
  for(i in 1:ncol(testdata)){
    
    testdata[(testdata[,i] %in% missingData_ID),i] = adaBoost_ModelList$Trees[[1]]$mostCom_eachAtt[i]
    
  }
  
  # If numeric, separate based on being above or below the median value of column from training set
  if(numericColInds[1]){
    
    for(i in 1:length(numericColInds)){
      
      rplCol = as.numeric(testdata[,numericColInds[i]])
      
      abvMd <- rplCol > adaBoost_ModelList$Trees[[1]]$medianVal[i]
      testdata[abvMd,numericColInds[i]] = paste(">",adaBoost_ModelList$Trees[[1]]$medianVal[i])
      testdata[!abvMd,numericColInds[i]] = paste("<=",adaBoost_ModelList$Trees[[1]]$medianVal[i])
      
    }
    
  }
  
  # Determine number of entries in test dataset
  if(is.null(try(nrow(testdata)))){
    testLength = 1
  }else{
    testLength=nrow(testdata)
  }
  
  # Create a vector to store predictions in
  if(numericOut){
    ada_Predictions <- vector(mode="numeric",length=testLength)
  }else{
    ada_Predictions <- vector(mode="character",length=testLength)
  }
  
  
  # Move through all test data and predict label
  for(i in 1:length(ada_Predictions)){
    
    curr_features = tryCatch(testdata[i,],error=function(e){return(testdata)})
    
    if(is.factor(curr_features)){
      curr_features=sapply(curr_features,as.character)
    }
    
    ada_Predictions[i]=adaBoost_pred(adaBoost_ModelList,curr_features,numericOut=numericOut,missingData_ID=missingData_ID)
    
  }
  
  # Calculate Prediction Error of Model (in this function, "testdata" is a dataframe, which is why the next line doesn't break. If for some reason a matrix or true vector was input, would need to alter how labelInd is determined by default)
  predError <- (length(ada_Predictions) - sum(ada_Predictions==testdata[,labelInd]))/length(ada_Predictions)
  
  return(list("Error"=predError,"Preds"=ada_Predictions))
  
  
}




bagged_ID3_trn <- function(trn_data,iters,numericColInds=0,bagPct=0.35,w_replace=T,ftr_splt_sz=NA,PurityMethod="Entropy",missingData_ID=NA,labelInd=ncol(trn_data)){
  
  tree_list <- vector(mode="list",length=iters)
  
  num_bag_samps <- floor(nrow(trn_data)*bagPct)
  
  for(i in 1:iters){
    
    bag_inds <- sample(1:nrow(trn_data),num_bag_samps,replace=w_replace)
    
    bag_data <- trn_data[bag_inds,]
    
    tree_list[[i]] <- Node$new(paste0("stump",i))
    
    # Train tree with bagged dataset
    trn_ID3(treeVar=tree_list[[i]],data=bag_data,depth=ncol(trn_data)-1,ftr_splt_sz=ftr_splt_sz,PurityMethod=PurityMethod,missingData_ID=missingData_ID,numericColInds=numericColInds,labelInd=labelInd)
    
    
  }
  
  return(tree_list)
  
}




bagged_ID3_pred <- function(bagged_Model,features,numericOut=F,missingData_ID=NA){
  
  bag_preds <- vector(length=length(bagged_Model))
  
  for(i in 1:length(bagged_Model)){
    
    bag_preds[i] = predict_ID3(tree=bagged_Model[[i]],features=features,numericOut=numericOut,missingData_ID=missingData_ID)
    
  }
  
  uq_labs <- unique(bag_preds)
  votes   <- vector(mode="numeric",length=length(uq_labs))
  
  for(i in 1:length(uq_labs)){
    
    same_lab <- bag_preds == uq_labs[i]
    votes[i] <- sum(same_lab)
    
  }
  
  winner <- which.max(votes)
  
  return(uq_labs[winner])
  
  
}




bagged_ID3_tst <- function(bagged_Model,testdata,numericColInds=0,missingData_ID=NA,numericOut=F,labelInd=ncol(testdata)){
  
  # Preprocessing of test data to match structure of tree as determined by training set
  
  # Address missing information. Replace with majority value of that attribute from training set
  
  for(i in 1:ncol(testdata)){
    
    testdata[(testdata[,i] %in% missingData_ID),i] = bagged_Model[[1]]$mostCom_eachAtt[i]
    
  }
  
  # If numeric, separate based on being above or below the median value of column from training set
  if(numericColInds[1]){
    
    for(i in 1:length(numericColInds)){
      
      rplCol = as.numeric(testdata[,numericColInds[i]])
      
      abvMd <- rplCol > bagged_Model[[1]]$medianVal[i]
      testdata[abvMd,numericColInds[i]] = paste(">",bagged_Model[[1]]$medianVal[i])
      testdata[!abvMd,numericColInds[i]] = paste("<=",bagged_Model[[1]]$medianVal[i])
      
    }
    
  }
  
  # Determine number of entries in test dataset
  if(is.null(try(nrow(testdata)))){
    testLength = 1
  }else{
    testLength=nrow(testdata)
  }
  
  # Create a vector to store predictions in
  if(numericOut){
    bag_Preds <- vector(mode="numeric",length=testLength)
  }else{
    bag_Preds <- vector(mode="character",length=testLength)
  }
  
  iterBagErr <- vector(mode="numeric",length=testLength)
  
  
  # Move through all test data and predict label
  for(i in 1:length(bag_Preds)){
    
    curr_features = tryCatch(testdata[i,],error=function(e){return(testdata)})
    
    if(is.factor(curr_features)){
      curr_features=sapply(curr_features,as.character)
    }
    
    bag_Preds[i] <- bagged_ID3_pred(bagged_Model,curr_features,numericOut=numericOut,missingData_ID=missingData_ID)
    
    
  }
  
  # Determine error 
  predErr <- (nrow(testdata) - sum(bag_Preds==testdata[,labelInd]))/nrow(testdata)
  
  return(list("Error"=predErr,"Preds"=bag_Preds))
  
  
  
}




RandForst_ID3_trn <- function(trn_data,iters,numericColInds=0,ftr_size=ceiling(ncol(trn_data)/3),bagPct=0.35,PurityMethod="Entropy",missingData_ID=NA,labelInd=ncol(trn_data)){
  
  forest = vector(mode="list",length=iters)
  
  for(i in 1:length(forest)){
    
    
    # Train tree with bagged dataset
    forest[[i]] <- bagged_ID3_trn(trn_data=trn_data,iters=1,numericColInds=numericColInds,bagPct=bagPct,ftr_splt_sz=ftr_size)
    
  }
  
  return(forest)
  
}




RandForst_ID3_pred <- function(randForst_Model,features,numericOut=F,missingData_ID=NA){
  
  forst_preds <- vector(length=length(randForst_Model))
  
  for(i in 1:length(randForst_Model)){
    
    forst_preds[i] = bagged_ID3_pred(bagged_Model=randForst_Model[[i]],features=features,numericOut=numericOut,missingData_ID=missingData_ID)
    
  }
  
  uq_labs <- unique(forst_preds)
  votes   <- vector(mode="numeric",length=length(uq_labs))
  
  for(i in 1:length(uq_labs)){
    
    same_lab <- forst_preds == uq_labs[i]
    votes[i] <- sum(same_lab)
    
  }
  
  winner <- which.max(votes)
  
  return(uq_labs[winner])
  
  
}




RandForst_ID3_tst <- function(randForst_Model,testdata,numericColInds=0,missingData_ID=NA,numericOut=F,labelInd=ncol(testdata)){
  
  # Preprocessing of test data to match structure of tree as determined by training set
  
  # Address missing information. Replace with majority value of that attribute from training set
  
  for(i in 1:ncol(testdata)){
    
    testdata[(testdata[,i] %in% missingData_ID),i] = randForst_Model[[1]][[1]]$mostCom_eachAtt[i]
    
  }
  
  # If numeric, separate based on being above or below the median value of column from training set
  if(numericColInds[1]){
    
    for(i in 1:length(numericColInds)){
      
      rplCol = as.numeric(testdata[,numericColInds[i]])
      
      abvMd <- rplCol > randForst_Model[[1]][[1]]$medianVal[i]
      testdata[abvMd,numericColInds[i]] = paste(">",randForst_Model[[1]][[1]]$medianVal[i])
      testdata[!abvMd,numericColInds[i]] = paste("<=",randForst_Model[[1]][[1]]$medianVal[i])
      
    }
    
  }
  
  # Determine number of entries in test dataset
  if(is.null(try(nrow(testdata)))){
    testLength = 1
  }else{
    testLength=nrow(testdata)
  }
  
  # Create a vector to store predictions in
  if(numericOut){
    forst_Preds <- vector(mode="numeric",length=testLength)
  }else{
    forst_Preds <- vector(mode="character",length=testLength)
  }
  
  
  # Move through all test data and predict label
  for(i in 1:length(forst_Preds)){
    
    curr_features = tryCatch(testdata[i,],error=function(e){return(testdata)})
    
    if(is.factor(curr_features)){
      curr_features=sapply(curr_features,as.character)
    }
    
    forst_Preds[i] <- RandForst_ID3_pred(randForst_Model,curr_features,numericOut=numericOut,missingData_ID=missingData_ID)
    
    
  }
  
  # Determine error 
  predErr <- (nrow(testdata) - sum(forst_Preds==testdata[,labelInd]))/nrow(testdata)
  
  return(list("Error"=predErr,"Preds"=forst_Preds))
  
  
  
}




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
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
    curr_subset_wgt <- subset_wgt[[i]]
    weights[i] = sum(curr_subset_wgt)
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




trn_ID3 <- function(treeVar,data,depth,PurityMethod="Entropy",missingData_ID=NA,numericColInds=0,labelInd=ncol(data),levelName="Base",firstCall=1){
  
  
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
    pur_calc <- function(data_col,d_labels){
      entrpy_col(data_col,d_labels)
    }
  }else if(PurityMethod=="Majority Error"){
    pur_calc <- function(data_col,d_labels){
      Mj_Error(data_col,d_labels)
    }
  }else if(PurityMethod=="GINI"){
    pur_calc <- function(data_col,d_labels){
      gini(data_col,d_labels)
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
      
      num_uq_labs[i] <- sum(curr_labs==uq_labs[i])
      
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
      
      
      # Place to store info gain of each attribute for this node
      treeVar$ig <- vector(mode="numeric",length=ncol(curr_data))#col_curr_data)
      
      for(i in 1:length(treeVar$ig)){
        puritys <- pur_calc(curr_data[,i],curr_labs)
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
        
        num_uq_atts[i] <- sum(curr_labs==uq_split_atts[i])
        
      }
      
      mostComm_ind <- which.max(num_uq_atts)
      
      treeVar$mostCommAtt <- uq_split_atts[mostComm_ind]
      
      
      
      # Split data up by those attributes and recursively call trainer to get down to leaf node or max depth
      for(i in 1:length(uq_split_atts)){
        
        
        keepInds_split <- curr_data[,max_ig_ind] == uq_split_atts[i]
        
        keepData <- curr_data[keepInds_split,-max_ig_ind]
        keepLabs <- curr_labs[keepInds_split]
        
        keepFull <- cbind(keepData,keepLabs)
        
        newdepth = depth - 1
        
        newNode <- treeVar$AddChild(uq_split_atts[i])
        
        trn_ID3(newNode,keepFull,newdepth,PurityMethod=PurityMethod,levelName=colnames(curr_data)[max_ig_ind],firstCall=0)
        
        
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
  
  return(predError)
  
}


# Decision Tree Training Algorithm

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


# *** ----




# Functions ----

# uq_entries <- function(data_vec){
#   
#   # if(is.factor(data_vec)){
#   #   data_vec <- sapply(data_vec,as.character)
#   # }
#   
#   uq_obs <- vector(mode=class(data_vec[1]))
#   
#   uq_obs[1] <- data_vec[1]
#   
#   trk_obs <- 2
#   
#   if(length(data_vec)==1){
#     return(uq_obs)
#   }else{
# 
#     for(i in 2:length(data_vec)){
#       if(sum(data_vec[i]==uq_obs) == 0){
#         uq_obs[trk_obs] = data_vec[i]
#         trk_obs = trk_obs + 1
#       }
#     }
#   }
#   
#   return(uq_obs)
#   
# }




info_gain <- function(S_purity,weights,subS_purity){
  
  subS_sum <- 0
  
  for(i in 1:length(subS_purity)){
    
    subS_sum <- subS_sum + weights[i]*subS_purity[i]
    
  }
  
  gain <- S_purity - subS_sum
  
  return(round(gain,4))
  
  
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




entrpy_col <- function(data_col,d_labels){
  
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
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = as.data.frame(comb_d_l[sav,])
  }
  
  # Initialize output 
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  entrpy_subs <- weights
  
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    curr_subset <- subsets[[i]]
    weights[i] = length(curr_subset$data_col)/length(data_col)
  }
  
  
  # Compute entropy of each unique observation
  
  for(i in 1:length(entrpy_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    
    # Find number of each unique label within each subset and calcualte entropy
    for(j in 1:length(uq_labs)){
      num_uq_labs[j] <- sum(curr_subset$d_labels==uq_labs[j])
      
      entrpy_subs[i] <- entrpy_subs[i] - (num_uq_labs[j]/nrow(curr_subset))*log((num_uq_labs[j]/nrow(curr_subset)),2)
      
      if(is.na(entrpy_subs[i])){
        entrpy_subs[i] <- 0
      }
    }

  }
  
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  entrpy_all <- 0
  
  # Find total entropy of attribute
  for(i in 1:length(uq_labs)){
    num_uq_labs[i] <- sum(d_labels==uq_labs[i])
    
    entrpy_all <- entrpy_all - (num_uq_labs[i]/length(d_labels))*log((num_uq_labs[i]/length(d_labels)),2)
    
    if(is.na(entrpy_all)){
      entrpy_all <- 0
    }
  }
  
  
  
  
  return(list("pur_all"=entrpy_all,"weights"=weights,"pur_subs"=entrpy_subs))
  
}




Mj_Error <- function(data_col,d_labels){
  
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
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = as.data.frame(comb_d_l[sav,])
  }
  
  # Initialize output 
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  maj_err_subs <- weights
  
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    curr_subset <- subsets[[i]]
    weights[i] = length(curr_subset$data_col)/length(data_col)
  }
  
  
  # Compute majority error of each unique observation
  
  for(i in 1:length(maj_err_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    
    # Find number of each unique label within each subset
    for(j in 1:length(uq_labs)){
      num_uq_labs[j] <- sum(curr_subset$d_labels==uq_labs[j])
    }
    
    # Calculate majority error
    maj_err_subs[i] <- (nrow(curr_subset) - max(num_uq_labs))/nrow(curr_subset)
    
  }
  
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  # Find majority error of attribute
  for(i in 1:length(uq_labs)){
    num_uq_labs[i] <- sum(d_labels==uq_labs[i])
  }
  
  maj_err_all <- (length(d_labels) - max(num_uq_labs))/length(d_labels)
  
  
  
  return(list("pur_all"=maj_err_all,"weights"=weights,"pur_subs"=maj_err_subs))

}




gini <- function(data_col,d_labels){
  
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
  
  for(i in 1:length(subsets)){
    sav <- which(data_col==uq_obs[i])
    
    subsets[[i]] = as.data.frame(comb_d_l[sav,])
  }
  
  # Initialize output 
  
  weights <- vector(mode="numeric",length=length(uq_obs))
  
  gini_subs <- weights
  
  # Determine weights of each attribute
  
  for(i in 1:length(weights)){
    curr_subset <- subsets[[i]]
    weights[i] = length(curr_subset[,1])/length(data_col)
  }
  
  
  # Compute GINI Index of each unique observation
  
  for(i in 1:length(gini_subs)){
    
    num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
    
    curr_subset <- subsets[[i]]
    
    # Find number of each unique label within each subset
    for(j in 1:length(uq_labs)){
      num_uq_labs[j] <- sum(curr_subset$d_labels==uq_labs[j])
    }
    
    # Calculate GINI
    
    gini_sum <- sum((num_uq_labs/nrow(curr_subset))^2)
    
    gini_subs[i] <- 1 - gini_sum
    
  }
  
  # Find GINI of attribute
  num_uq_labs <- vector(mode="numeric",length=length(uq_labs))
  
  for(i in 1:length(uq_labs)){
    num_uq_labs[i] <- sum(d_labels==uq_labs[i])
  }
  
  gini_sum <- sum((num_uq_labs/length(d_labels))^2)
  
  gini_all <- 1 - gini_sum
  
  
  
  return(list("pur_all"=gini_all,"weights"=weights,"pur_subs"=gini_subs))
  
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




# *** ----




# # Read In Tennis Data ----
# 
# 
# tennis_NoHd_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/tennis_noHeader.csv",header=F,stringsAsFactors = F)
# 
# tennis_Hd_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/tennis_Header.csv",header=T,stringsAsFactors = F)
# 
# 
# 
# # *** ----
# 
# 
# 
# 
# # Train Tennis Tree ----
# 
# TennisTree <- Node$new("Tennis")
# 
# trn_ID3(treeVar=TennisTree,data=tennis_Hd_data,depth=6,PurityMethod = "Entropy")
# 
# print(TennisTree,"splitVar","isLeaf")
# 
# 
# # *** ----
# 
# 
# 
# 
# # Test Model of Tennis Tree ----
# 
# test_Tennis_Hd <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/tennis_Header_test.csv",header=T,stringsAsFactors = F)
# 
# test_ID3(TennisTree,test_Tennis_Hd,numericOut = F)
# 
# # *** ----
# 
# 
# 

# # *** ----
# 
# 
# 
#
# Car Problem ----

# Read in Training & Test Data ----

car_trn_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/car/train.csv",header=F,stringsAsFactors=F)

car_tst_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/car/test.csv",header=F,stringsAsFactors=F)

# *** ----




# Initialize trees for each depth of car model, and IG method ----

car_d1_ent <- Node$new("Car_Depth1_Ent")
car_d2_ent <- Node$new("Car_Depth2_Ent")
car_d3_ent <- Node$new("Car_Depth3_Ent")
car_d4_ent <- Node$new("Car_Depth4_Ent")
car_d5_ent <- Node$new("Car_Depth5_Ent")
car_d6_ent <- Node$new("Car_Depth6_Ent")

car_d1_MJe <- Node$new("Car_Depth1_MJerr")
car_d2_MJe <- Node$new("Car_Depth2_MJerr")
car_d3_MJe <- Node$new("Car_Depth3_MJerr")
car_d4_MJe <- Node$new("Car_Depth4_MJerr")
car_d5_MJe <- Node$new("Car_Depth5_MJerr")
car_d6_MJe <- Node$new("Car_Depth6_MJerr")

car_d1_GIN <- Node$new("Car_Depth1_GINI")
car_d2_GIN <- Node$new("Car_Depth2_GINI")
car_d3_GIN <- Node$new("Car_Depth3_GINI")
car_d4_GIN <- Node$new("Car_Depth4_GINI")
car_d5_GIN <- Node$new("Car_Depth5_GINI")
car_d6_GIN <- Node$new("Car_Depth6_GINI")

# *** ----




# Train each tree ----

trn_ID3(car_d1_ent,car_trn_data,depth=1,PurityMethod="Entropy")
trn_ID3(car_d2_ent,car_trn_data,depth=2,PurityMethod="Entropy")
trn_ID3(car_d3_ent,car_trn_data,depth=3,PurityMethod="Entropy")
trn_ID3(car_d4_ent,car_trn_data,depth=4,PurityMethod="Entropy")
trn_ID3(car_d5_ent,car_trn_data,depth=5,PurityMethod="Entropy")
trn_ID3(car_d6_ent,car_trn_data,depth=6,PurityMethod="Entropy")

trn_ID3(car_d1_MJe,car_trn_data,depth=1,PurityMethod="Majority Error")
trn_ID3(car_d2_MJe,car_trn_data,depth=2,PurityMethod="Majority Error")
trn_ID3(car_d3_MJe,car_trn_data,depth=3,PurityMethod="Majority Error")
trn_ID3(car_d4_MJe,car_trn_data,depth=4,PurityMethod="Majority Error")
trn_ID3(car_d5_MJe,car_trn_data,depth=5,PurityMethod="Majority Error")
trn_ID3(car_d6_MJe,car_trn_data,depth=6,PurityMethod="Majority Error")

trn_ID3(car_d1_GIN,car_trn_data,depth=1,PurityMethod="GINI")
trn_ID3(car_d2_GIN,car_trn_data,depth=2,PurityMethod="GINI")
trn_ID3(car_d3_GIN,car_trn_data,depth=3,PurityMethod="GINI")
trn_ID3(car_d4_GIN,car_trn_data,depth=4,PurityMethod="GINI")
trn_ID3(car_d5_GIN,car_trn_data,depth=5,PurityMethod="GINI")
trn_ID3(car_d6_GIN,car_trn_data,depth=6,PurityMethod="GINI")

# *** ----




# Train & Test error of each tree ----


# Entropy
car_d1_ent_trnerr <- test_ID3(car_d1_ent,car_trn_data,numericOut=F)
car_d2_ent_trnerr <- test_ID3(car_d2_ent,car_trn_data,numericOut=F)
car_d3_ent_trnerr <- test_ID3(car_d3_ent,car_trn_data,numericOut=F)
car_d4_ent_trnerr <- test_ID3(car_d4_ent,car_trn_data,numericOut=F)
car_d5_ent_trnerr <- test_ID3(car_d5_ent,car_trn_data,numericOut=F)
car_d6_ent_trnerr <- test_ID3(car_d6_ent,car_trn_data,numericOut=F)

car_d1_ent_tsterr <- test_ID3(car_d1_ent,car_tst_data,numericOut=F)
car_d2_ent_tsterr <- test_ID3(car_d2_ent,car_tst_data,numericOut=F)
car_d3_ent_tsterr <- test_ID3(car_d3_ent,car_tst_data,numericOut=F)
car_d4_ent_tsterr <- test_ID3(car_d4_ent,car_tst_data,numericOut=F)
car_d5_ent_tsterr <- test_ID3(car_d5_ent,car_tst_data,numericOut=F)
car_d6_ent_tsterr <- test_ID3(car_d6_ent,car_tst_data,numericOut=F)


# Majority Error
car_d1_MJe_trnerr <- test_ID3(car_d1_MJe,car_trn_data,numericOut=F)
car_d2_MJe_trnerr <- test_ID3(car_d2_MJe,car_trn_data,numericOut=F)
car_d3_MJe_trnerr <- test_ID3(car_d3_MJe,car_trn_data,numericOut=F)
car_d4_MJe_trnerr <- test_ID3(car_d4_MJe,car_trn_data,numericOut=F)
car_d5_MJe_trnerr <- test_ID3(car_d5_MJe,car_trn_data,numericOut=F)
car_d6_MJe_trnerr <- test_ID3(car_d6_MJe,car_trn_data,numericOut=F)

car_d1_MJe_tsterr <- test_ID3(car_d1_MJe,car_tst_data,numericOut=F)
car_d2_MJe_tsterr <- test_ID3(car_d2_MJe,car_tst_data,numericOut=F)
car_d3_MJe_tsterr <- test_ID3(car_d3_MJe,car_tst_data,numericOut=F)
car_d4_MJe_tsterr <- test_ID3(car_d4_MJe,car_tst_data,numericOut=F)
car_d5_MJe_tsterr <- test_ID3(car_d5_MJe,car_tst_data,numericOut=F)
car_d6_MJe_tsterr <- test_ID3(car_d6_MJe,car_tst_data,numericOut=F)


# GINI2
car_d1_GIN_trnerr <- test_ID3(car_d1_GIN,car_trn_data,numericOut=F)
car_d2_GIN_trnerr <- test_ID3(car_d2_GIN,car_trn_data,numericOut=F)
car_d3_GIN_trnerr <- test_ID3(car_d3_GIN,car_trn_data,numericOut=F)
car_d4_GIN_trnerr <- test_ID3(car_d4_GIN,car_trn_data,numericOut=F)
car_d5_GIN_trnerr <- test_ID3(car_d5_GIN,car_trn_data,numericOut=F)
car_d6_GIN_trnerr <- test_ID3(car_d6_GIN,car_trn_data,numericOut=F)

car_d1_GIN_tsterr <- test_ID3(car_d1_GIN,car_tst_data,numericOut=F)
car_d2_GIN_tsterr <- test_ID3(car_d2_GIN,car_tst_data,numericOut=F)
car_d3_GIN_tsterr <- test_ID3(car_d3_GIN,car_tst_data,numericOut=F)
car_d4_GIN_tsterr <- test_ID3(car_d4_GIN,car_tst_data,numericOut=F)
car_d5_GIN_tsterr <- test_ID3(car_d5_GIN,car_tst_data,numericOut=F)
car_d6_GIN_tsterr <- test_ID3(car_d6_GIN,car_tst_data,numericOut=F)

# *** ----




# Organize Error Results for Easy Viewing ----

Car_Err_Tbl <- data.frame("Entropy Train Error"=round(c(car_d1_ent_trnerr
                                                       ,car_d2_ent_trnerr
                                                       ,car_d3_ent_trnerr
                                                       ,car_d4_ent_trnerr
                                                       ,car_d5_ent_trnerr
                                                       ,car_d6_ent_trnerr),3),
                          "Entropy Test Error"=round(c(car_d1_ent_tsterr
                                                      ,car_d2_ent_tsterr
                                                      ,car_d3_ent_tsterr
                                                      ,car_d4_ent_tsterr
                                                      ,car_d5_ent_tsterr
                                                      ,car_d6_ent_tsterr),3),
                          "Majority Error Train Error"=c(car_d1_MJe_trnerr
                                                        ,car_d2_MJe_trnerr
                                                        ,car_d3_MJe_trnerr
                                                        ,car_d4_MJe_trnerr
                                                        ,car_d5_MJe_trnerr
                                                        ,car_d6_MJe_trnerr),
                          "Majority Error Test Error"=round(c(car_d1_MJe_tsterr
                                                             ,car_d2_MJe_tsterr
                                                             ,car_d3_MJe_tsterr
                                                             ,car_d4_MJe_tsterr
                                                             ,car_d5_MJe_tsterr
                                                             ,car_d6_MJe_tsterr),3),
                          "GINI Train Error"=round(c(car_d1_GIN_trnerr
                                                    ,car_d2_GIN_trnerr
                                                    ,car_d3_GIN_trnerr
                                                    ,car_d4_GIN_trnerr
                                                    ,car_d5_GIN_trnerr
                                                    ,car_d6_GIN_trnerr),3),
                          "GINI Test Error"=round(c(car_d1_GIN_tsterr
                                                   ,car_d2_GIN_tsterr
                                                   ,car_d3_GIN_tsterr
                                                   ,car_d4_GIN_tsterr
                                                   ,car_d5_GIN_tsterr
                                                   ,car_d6_GIN_tsterr),3))

Car_Err_Tbl

# *** ----




# *** ----
# 
# 
# 
#
# Bank Problem ----

# Read in Training & Test Data ----

bnk_trn_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/train.csv",header=F,stringsAsFactors=F)

bnk_tst_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/test.csv",header=F,stringsAsFactors=F)

# *** ----




# Initialize trees for each depth of bank model with no missing data, and IG method ----

bnk_d1_ent_nMs  <- Node$new("Bank_nMs_Depth1_Ent")
bnk_d2_ent_nMs  <- Node$new("Bank_nMs_Depth2_Ent")
bnk_d3_ent_nMs  <- Node$new("Bank_nMs_Depth3_Ent")
bnk_d4_ent_nMs  <- Node$new("Bank_nMs_Depth4_Ent")
bnk_d5_ent_nMs  <- Node$new("Bank_nMs_Depth5_Ent")
bnk_d6_ent_nMs  <- Node$new("Bank_nMs_Depth6_Ent")
bnk_d7_ent_nMs  <- Node$new("Bank_nMs_Depth7_Ent")
bnk_d8_ent_nMs  <- Node$new("Bank_nMs_Depth8_Ent")
bnk_d9_ent_nMs  <- Node$new("Bank_nMs_Depth9_Ent")
bnk_d10_ent_nMs <- Node$new("Bank_nMs_Depth10_Ent")
bnk_d11_ent_nMs <- Node$new("Bank_nMs_Depth11_Ent")
bnk_d12_ent_nMs <- Node$new("Bank_nMs_Depth12_Ent")
bnk_d13_ent_nMs <- Node$new("Bank_nMs_Depth13_Ent")
bnk_d14_ent_nMs <- Node$new("Bank_nMs_Depth14_Ent")
bnk_d15_ent_nMs <- Node$new("Bank_nMs_Depth15_Ent")
bnk_d16_ent_nMs <- Node$new("Bank_nMs_Depth16_Ent")


bnk_d1_MJe_nMs  <- Node$new("Bank_nMs_Depth1_MJerr")
bnk_d2_MJe_nMs  <- Node$new("Bank_nMs_Depth2_MJerr")
bnk_d3_MJe_nMs  <- Node$new("Bank_nMs_Depth3_MJerr")
bnk_d4_MJe_nMs  <- Node$new("Bank_nMs_Depth4_MJerr")
bnk_d5_MJe_nMs  <- Node$new("Bank_nMs_Depth5_MJerr")
bnk_d6_MJe_nMs  <- Node$new("Bank_nMs_Depth6_MJerr")
bnk_d7_MJe_nMs  <- Node$new("Bank_nMs_Depth7_MJerr")
bnk_d8_MJe_nMs  <- Node$new("Bank_nMs_Depth8_MJerr")
bnk_d9_MJe_nMs  <- Node$new("Bank_nMs_Depth9_MJerr")
bnk_d10_MJe_nMs <- Node$new("Bank_nMs_Depth10_MJerr")
bnk_d11_MJe_nMs <- Node$new("Bank_nMs_Depth11_MJerr")
bnk_d12_MJe_nMs <- Node$new("Bank_nMs_Depth12_MJerr")
bnk_d13_MJe_nMs <- Node$new("Bank_nMs_Depth13_MJerr")
bnk_d14_MJe_nMs <- Node$new("Bank_nMs_Depth14_MJerr")
bnk_d15_MJe_nMs <- Node$new("Bank_nMs_Depth15_MJerr")
bnk_d16_MJe_nMs <- Node$new("Bank_nMs_Depth16_MJerr")


bnk_d1_GIN_nMs  <- Node$new("Bank_nMs_Depth1_GINI")
bnk_d2_GIN_nMs  <- Node$new("Bank_nMs_Depth2_GINI")
bnk_d3_GIN_nMs  <- Node$new("Bank_nMs_Depth3_GINI")
bnk_d4_GIN_nMs  <- Node$new("Bank_nMs_Depth4_GINI")
bnk_d5_GIN_nMs  <- Node$new("Bank_nMs_Depth5_GINI")
bnk_d6_GIN_nMs  <- Node$new("Bank_nMs_Depth6_GINI")
bnk_d7_GIN_nMs  <- Node$new("Bank_nMs_Depth7_GINI")
bnk_d8_GIN_nMs  <- Node$new("Bank_nMs_Depth8_GINI")
bnk_d9_GIN_nMs  <- Node$new("Bank_nMs_Depth9_GINI")
bnk_d10_GIN_nMs <- Node$new("Bank_nMs_Depth10_GINI")
bnk_d11_GIN_nMs <- Node$new("Bank_nMs_Depth11_GINI")
bnk_d12_GIN_nMs <- Node$new("Bank_nMs_Depth12_GINI")
bnk_d13_GIN_nMs <- Node$new("Bank_nMs_Depth13_GINI")
bnk_d14_GIN_nMs <- Node$new("Bank_nMs_Depth14_GINI")
bnk_d15_GIN_nMs <- Node$new("Bank_nMs_Depth15_GINI")
bnk_d16_GIN_nMs <- Node$new("Bank_nMs_Depth16_GINI")


# *** ----




# Train each tree ----

trn_ID3(bnk_d1_ent_nMs,bnk_trn_data,depth=1,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_ent_nMs,bnk_trn_data,depth=2,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_ent_nMs,bnk_trn_data,depth=3,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_ent_nMs,bnk_trn_data,depth=4,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_ent_nMs,bnk_trn_data,depth=5,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_ent_nMs,bnk_trn_data,depth=6,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_ent_nMs,bnk_trn_data,depth=7,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_ent_nMs,bnk_trn_data,depth=8,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_ent_nMs,bnk_trn_data,depth=9,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_ent_nMs,bnk_trn_data,depth=10,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_ent_nMs,bnk_trn_data,depth=11,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_ent_nMs,bnk_trn_data,depth=12,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_ent_nMs,bnk_trn_data,depth=13,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_ent_nMs,bnk_trn_data,depth=14,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_ent_nMs,bnk_trn_data,depth=15,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_ent_nMs,bnk_trn_data,depth=16,PurityMethod="Entropy",numericColInds=c(1,6,10,12,13,14,15))


trn_ID3(bnk_d1_MJe_nMs,bnk_trn_data,depth=1,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_MJe_nMs,bnk_trn_data,depth=2,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_MJe_nMs,bnk_trn_data,depth=3,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_MJe_nMs,bnk_trn_data,depth=4,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_MJe_nMs,bnk_trn_data,depth=5,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_MJe_nMs,bnk_trn_data,depth=6,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_MJe_nMs,bnk_trn_data,depth=7,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_MJe_nMs,bnk_trn_data,depth=8,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_MJe_nMs,bnk_trn_data,depth=9,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_MJe_nMs,bnk_trn_data,depth=10,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_MJe_nMs,bnk_trn_data,depth=11,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_MJe_nMs,bnk_trn_data,depth=12,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_MJe_nMs,bnk_trn_data,depth=13,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_MJe_nMs,bnk_trn_data,depth=14,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_MJe_nMs,bnk_trn_data,depth=15,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_MJe_nMs,bnk_trn_data,depth=16,PurityMethod="Majority Error",numericColInds=c(1,6,10,12,13,14,15))


trn_ID3(bnk_d1_GIN_nMs,bnk_trn_data,depth=1,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_GIN_nMs,bnk_trn_data,depth=2,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_GIN_nMs,bnk_trn_data,depth=3,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_GIN_nMs,bnk_trn_data,depth=4,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_GIN_nMs,bnk_trn_data,depth=5,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_GIN_nMs,bnk_trn_data,depth=6,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_GIN_nMs,bnk_trn_data,depth=7,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_GIN_nMs,bnk_trn_data,depth=8,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_GIN_nMs,bnk_trn_data,depth=9,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_GIN_nMs,bnk_trn_data,depth=10,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_GIN_nMs,bnk_trn_data,depth=11,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_GIN_nMs,bnk_trn_data,depth=12,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_GIN_nMs,bnk_trn_data,depth=13,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_GIN_nMs,bnk_trn_data,depth=14,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_GIN_nMs,bnk_trn_data,depth=15,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_GIN_nMs,bnk_trn_data,depth=16,PurityMethod="GINI",numericColInds=c(1,6,10,12,13,14,15))


# *** ----




# Train & Test error of each tree ----


# Entropy
bnk_d1_ent_nMs_trnerr  <- test_ID3(bnk_d1_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_ent_nMs_trnerr  <- test_ID3(bnk_d2_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_ent_nMs_trnerr  <- test_ID3(bnk_d3_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_ent_nMs_trnerr  <- test_ID3(bnk_d4_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_ent_nMs_trnerr  <- test_ID3(bnk_d5_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_ent_nMs_trnerr  <- test_ID3(bnk_d6_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_ent_nMs_trnerr  <- test_ID3(bnk_d7_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_ent_nMs_trnerr  <- test_ID3(bnk_d8_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_ent_nMs_trnerr  <- test_ID3(bnk_d9_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_ent_nMs_trnerr <- test_ID3(bnk_d10_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_ent_nMs_trnerr <- test_ID3(bnk_d11_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_ent_nMs_trnerr <- test_ID3(bnk_d12_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_ent_nMs_trnerr <- test_ID3(bnk_d13_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_ent_nMs_trnerr <- test_ID3(bnk_d14_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_ent_nMs_trnerr <- test_ID3(bnk_d15_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_ent_nMs_trnerr <- test_ID3(bnk_d16_ent_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_ent_nMs_tsterr  <- test_ID3(bnk_d1_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_ent_nMs_tsterr  <- test_ID3(bnk_d2_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_ent_nMs_tsterr  <- test_ID3(bnk_d3_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_ent_nMs_tsterr  <- test_ID3(bnk_d4_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_ent_nMs_tsterr  <- test_ID3(bnk_d5_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_ent_nMs_tsterr  <- test_ID3(bnk_d6_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_ent_nMs_tsterr  <- test_ID3(bnk_d7_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_ent_nMs_tsterr  <- test_ID3(bnk_d8_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_ent_nMs_tsterr  <- test_ID3(bnk_d9_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_ent_nMs_tsterr <- test_ID3(bnk_d10_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_ent_nMs_tsterr <- test_ID3(bnk_d11_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_ent_nMs_tsterr <- test_ID3(bnk_d12_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_ent_nMs_tsterr <- test_ID3(bnk_d13_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_ent_nMs_tsterr <- test_ID3(bnk_d14_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_ent_nMs_tsterr <- test_ID3(bnk_d15_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_ent_nMs_tsterr <- test_ID3(bnk_d16_ent_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))



# Majority Error
bnk_d1_MJe_nMs_trnerr  <- test_ID3(bnk_d1_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_MJe_nMs_trnerr  <- test_ID3(bnk_d2_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_MJe_nMs_trnerr  <- test_ID3(bnk_d3_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_MJe_nMs_trnerr  <- test_ID3(bnk_d4_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_MJe_nMs_trnerr  <- test_ID3(bnk_d5_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_MJe_nMs_trnerr  <- test_ID3(bnk_d6_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_MJe_nMs_trnerr  <- test_ID3(bnk_d7_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_MJe_nMs_trnerr  <- test_ID3(bnk_d8_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_MJe_nMs_trnerr  <- test_ID3(bnk_d9_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_MJe_nMs_trnerr <- test_ID3(bnk_d10_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_MJe_nMs_trnerr <- test_ID3(bnk_d11_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_MJe_nMs_trnerr <- test_ID3(bnk_d12_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_MJe_nMs_trnerr <- test_ID3(bnk_d13_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_MJe_nMs_trnerr <- test_ID3(bnk_d14_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_MJe_nMs_trnerr <- test_ID3(bnk_d15_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_MJe_nMs_trnerr <- test_ID3(bnk_d16_MJe_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_MJe_nMs_tsterr  <- test_ID3(bnk_d1_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_MJe_nMs_tsterr  <- test_ID3(bnk_d2_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_MJe_nMs_tsterr  <- test_ID3(bnk_d3_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_MJe_nMs_tsterr  <- test_ID3(bnk_d4_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_MJe_nMs_tsterr  <- test_ID3(bnk_d5_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_MJe_nMs_tsterr  <- test_ID3(bnk_d6_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_MJe_nMs_tsterr  <- test_ID3(bnk_d7_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_MJe_nMs_tsterr  <- test_ID3(bnk_d8_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_MJe_nMs_tsterr  <- test_ID3(bnk_d9_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_MJe_nMs_tsterr <- test_ID3(bnk_d10_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_MJe_nMs_tsterr <- test_ID3(bnk_d11_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_MJe_nMs_tsterr <- test_ID3(bnk_d12_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_MJe_nMs_tsterr <- test_ID3(bnk_d13_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_MJe_nMs_tsterr <- test_ID3(bnk_d14_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_MJe_nMs_tsterr <- test_ID3(bnk_d15_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_MJe_nMs_tsterr <- test_ID3(bnk_d16_MJe_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))



# GINI
bnk_d1_GIN_nMs_trnerr  <- test_ID3(bnk_d1_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_GIN_nMs_trnerr  <- test_ID3(bnk_d2_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_GIN_nMs_trnerr  <- test_ID3(bnk_d3_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_GIN_nMs_trnerr  <- test_ID3(bnk_d4_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_GIN_nMs_trnerr  <- test_ID3(bnk_d5_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_GIN_nMs_trnerr  <- test_ID3(bnk_d6_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_GIN_nMs_trnerr  <- test_ID3(bnk_d7_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_GIN_nMs_trnerr  <- test_ID3(bnk_d8_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_GIN_nMs_trnerr  <- test_ID3(bnk_d9_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_GIN_nMs_trnerr <- test_ID3(bnk_d10_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_GIN_nMs_trnerr <- test_ID3(bnk_d11_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_GIN_nMs_trnerr <- test_ID3(bnk_d12_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_GIN_nMs_trnerr <- test_ID3(bnk_d13_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_GIN_nMs_trnerr <- test_ID3(bnk_d14_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_GIN_nMs_trnerr <- test_ID3(bnk_d15_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_GIN_nMs_trnerr <- test_ID3(bnk_d16_GIN_nMs,bnk_trn_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_GIN_nMs_tsterr  <- test_ID3(bnk_d1_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_GIN_nMs_tsterr  <- test_ID3(bnk_d2_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_GIN_nMs_tsterr  <- test_ID3(bnk_d3_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_GIN_nMs_tsterr  <- test_ID3(bnk_d4_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_GIN_nMs_tsterr  <- test_ID3(bnk_d5_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_GIN_nMs_tsterr  <- test_ID3(bnk_d6_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_GIN_nMs_tsterr  <- test_ID3(bnk_d7_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_GIN_nMs_tsterr  <- test_ID3(bnk_d8_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_GIN_nMs_tsterr  <- test_ID3(bnk_d9_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_GIN_nMs_tsterr <- test_ID3(bnk_d10_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_GIN_nMs_tsterr <- test_ID3(bnk_d11_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_GIN_nMs_tsterr <- test_ID3(bnk_d12_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_GIN_nMs_tsterr <- test_ID3(bnk_d13_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_GIN_nMs_tsterr <- test_ID3(bnk_d14_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_GIN_nMs_tsterr <- test_ID3(bnk_d15_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_GIN_nMs_tsterr <- test_ID3(bnk_d16_GIN_nMs,bnk_tst_data,numericOut=F,numericColInds=c(1,6,10,12,13,14,15))


# *** ----




# Organize Error Results for Easy Viewing ----

Bnk_Err_Tbl_nMs <- data.frame("Entropy Train Error"=round(c(bnk_d1_ent_nMs_trnerr
                                                       ,bnk_d2_ent_nMs_trnerr
                                                       ,bnk_d3_ent_nMs_trnerr
                                                       ,bnk_d4_ent_nMs_trnerr
                                                       ,bnk_d5_ent_nMs_trnerr
                                                       ,bnk_d6_ent_nMs_trnerr
                                                       ,bnk_d7_ent_nMs_trnerr
                                                       ,bnk_d8_ent_nMs_trnerr
                                                       ,bnk_d9_ent_nMs_trnerr
                                                       ,bnk_d10_ent_nMs_trnerr
                                                       ,bnk_d11_ent_nMs_trnerr
                                                       ,bnk_d12_ent_nMs_trnerr
                                                       ,bnk_d13_ent_nMs_trnerr
                                                       ,bnk_d14_ent_nMs_trnerr
                                                       ,bnk_d15_ent_nMs_trnerr
                                                       ,bnk_d16_ent_nMs_trnerr),3),
                          "Entropy Test Error"=round(c(bnk_d1_ent_nMs_tsterr
                                                      ,bnk_d2_ent_nMs_tsterr
                                                      ,bnk_d3_ent_nMs_tsterr
                                                      ,bnk_d4_ent_nMs_tsterr
                                                      ,bnk_d5_ent_nMs_tsterr
                                                      ,bnk_d6_ent_nMs_tsterr
                                                      ,bnk_d7_ent_nMs_tsterr
                                                      ,bnk_d8_ent_nMs_tsterr
                                                      ,bnk_d9_ent_nMs_tsterr
                                                      ,bnk_d10_ent_nMs_tsterr
                                                      ,bnk_d11_ent_nMs_tsterr
                                                      ,bnk_d12_ent_nMs_tsterr
                                                      ,bnk_d13_ent_nMs_tsterr
                                                      ,bnk_d14_ent_nMs_tsterr
                                                      ,bnk_d15_ent_nMs_tsterr
                                                      ,bnk_d16_ent_nMs_tsterr),3),
                          "Majority Error Train Error"=c(bnk_d1_MJe_nMs_trnerr
                                                        ,bnk_d2_MJe_nMs_trnerr
                                                        ,bnk_d3_MJe_nMs_trnerr
                                                        ,bnk_d4_MJe_nMs_trnerr
                                                        ,bnk_d5_MJe_nMs_trnerr
                                                        ,bnk_d6_MJe_nMs_trnerr
                                                        ,bnk_d7_MJe_nMs_trnerr
                                                        ,bnk_d8_MJe_nMs_trnerr
                                                        ,bnk_d9_MJe_nMs_trnerr
                                                        ,bnk_d10_MJe_nMs_trnerr
                                                        ,bnk_d11_MJe_nMs_trnerr
                                                        ,bnk_d12_MJe_nMs_trnerr
                                                        ,bnk_d13_MJe_nMs_trnerr
                                                        ,bnk_d14_MJe_nMs_trnerr
                                                        ,bnk_d15_MJe_nMs_trnerr
                                                        ,bnk_d16_MJe_nMs_trnerr),
                          "Majority Error Test Error"=round(c(bnk_d1_MJe_nMs_tsterr
                                                             ,bnk_d2_MJe_nMs_tsterr
                                                             ,bnk_d3_MJe_nMs_tsterr
                                                             ,bnk_d4_MJe_nMs_tsterr
                                                             ,bnk_d5_MJe_nMs_tsterr
                                                             ,bnk_d6_MJe_nMs_tsterr
                                                             ,bnk_d7_MJe_nMs_tsterr
                                                             ,bnk_d8_MJe_nMs_tsterr
                                                             ,bnk_d9_MJe_nMs_tsterr
                                                             ,bnk_d10_MJe_nMs_tsterr
                                                             ,bnk_d11_MJe_nMs_tsterr
                                                             ,bnk_d12_MJe_nMs_tsterr
                                                             ,bnk_d13_MJe_nMs_tsterr
                                                             ,bnk_d14_MJe_nMs_tsterr
                                                             ,bnk_d15_MJe_nMs_tsterr
                                                             ,bnk_d16_MJe_nMs_tsterr),3),
                          "GINI Train Error"=round(c(bnk_d1_GIN_nMs_trnerr
                                                    ,bnk_d2_GIN_nMs_trnerr
                                                    ,bnk_d3_GIN_nMs_trnerr
                                                    ,bnk_d4_GIN_nMs_trnerr
                                                    ,bnk_d5_GIN_nMs_trnerr
                                                    ,bnk_d6_GIN_nMs_trnerr
                                                    ,bnk_d7_GIN_nMs_trnerr
                                                    ,bnk_d8_GIN_nMs_trnerr
                                                    ,bnk_d9_GIN_nMs_trnerr
                                                    ,bnk_d10_GIN_nMs_trnerr
                                                    ,bnk_d11_GIN_nMs_trnerr
                                                    ,bnk_d12_GIN_nMs_trnerr
                                                    ,bnk_d13_GIN_nMs_trnerr
                                                    ,bnk_d14_GIN_nMs_trnerr
                                                    ,bnk_d15_GIN_nMs_trnerr
                                                    ,bnk_d16_GIN_nMs_trnerr),3),
                          "GINI Test Error"=round(c(bnk_d1_GIN_nMs_tsterr
                                                   ,bnk_d2_GIN_nMs_tsterr
                                                   ,bnk_d3_GIN_nMs_tsterr
                                                   ,bnk_d4_GIN_nMs_tsterr
                                                   ,bnk_d5_GIN_nMs_tsterr
                                                   ,bnk_d6_GIN_nMs_tsterr
                                                   ,bnk_d7_GIN_nMs_tsterr
                                                   ,bnk_d8_GIN_nMs_tsterr
                                                   ,bnk_d9_GIN_nMs_tsterr
                                                   ,bnk_d10_GIN_nMs_tsterr
                                                   ,bnk_d11_GIN_nMs_tsterr
                                                   ,bnk_d12_GIN_nMs_tsterr
                                                   ,bnk_d13_GIN_nMs_tsterr
                                                   ,bnk_d14_GIN_nMs_tsterr
                                                   ,bnk_d15_GIN_nMs_tsterr
                                                   ,bnk_d16_GIN_nMs_tsterr),3))

row.names(Bnk_Err_Tbl_nMs) <- as.character(seq(1,16,1))

Bnk_Err_Tbl_nMs


# *** ----




# *** ----




# Initialize trees for each depth of bank model with missing data, and IG method ----

bnk_d1_ent_MsD  <- Node$new("Bank_MsD_Depth1_Ent")
bnk_d2_ent_MsD  <- Node$new("Bank_MsD_Depth2_Ent")
bnk_d3_ent_MsD  <- Node$new("Bank_MsD_Depth3_Ent")
bnk_d4_ent_MsD  <- Node$new("Bank_MsD_Depth4_Ent")
bnk_d5_ent_MsD  <- Node$new("Bank_MsD_Depth5_Ent")
bnk_d6_ent_MsD  <- Node$new("Bank_MsD_Depth6_Ent")
bnk_d7_ent_MsD  <- Node$new("Bank_MsD_Depth7_Ent")
bnk_d8_ent_MsD  <- Node$new("Bank_MsD_Depth8_Ent")
bnk_d9_ent_MsD  <- Node$new("Bank_MsD_Depth9_Ent")
bnk_d10_ent_MsD <- Node$new("Bank_MsD_Depth10_Ent")
bnk_d11_ent_MsD <- Node$new("Bank_MsD_Depth11_Ent")
bnk_d12_ent_MsD <- Node$new("Bank_MsD_Depth12_Ent")
bnk_d13_ent_MsD <- Node$new("Bank_MsD_Depth13_Ent")
bnk_d14_ent_MsD <- Node$new("Bank_MsD_Depth14_Ent")
bnk_d15_ent_MsD <- Node$new("Bank_MsD_Depth15_Ent")
bnk_d16_ent_MsD <- Node$new("Bank_MsD_Depth16_Ent")


bnk_d1_MJe_MsD  <- Node$new("Bank_MsD_Depth1_MJerr")
bnk_d2_MJe_MsD  <- Node$new("Bank_MsD_Depth2_MJerr")
bnk_d3_MJe_MsD  <- Node$new("Bank_MsD_Depth3_MJerr")
bnk_d4_MJe_MsD  <- Node$new("Bank_MsD_Depth4_MJerr")
bnk_d5_MJe_MsD  <- Node$new("Bank_MsD_Depth5_MJerr")
bnk_d6_MJe_MsD  <- Node$new("Bank_MsD_Depth6_MJerr")
bnk_d7_MJe_MsD  <- Node$new("Bank_MsD_Depth7_MJerr")
bnk_d8_MJe_MsD  <- Node$new("Bank_MsD_Depth8_MJerr")
bnk_d9_MJe_MsD  <- Node$new("Bank_MsD_Depth9_MJerr")
bnk_d10_MJe_MsD <- Node$new("Bank_MsD_Depth10_MJerr")
bnk_d11_MJe_MsD <- Node$new("Bank_MsD_Depth11_MJerr")
bnk_d12_MJe_MsD <- Node$new("Bank_MsD_Depth12_MJerr")
bnk_d13_MJe_MsD <- Node$new("Bank_MsD_Depth13_MJerr")
bnk_d14_MJe_MsD <- Node$new("Bank_MsD_Depth14_MJerr")
bnk_d15_MJe_MsD <- Node$new("Bank_MsD_Depth15_MJerr")
bnk_d16_MJe_MsD <- Node$new("Bank_MsD_Depth16_MJerr")


bnk_d1_GIN_MsD  <- Node$new("Bank_MsD_Depth1_GINI")
bnk_d2_GIN_MsD  <- Node$new("Bank_MsD_Depth2_GINI")
bnk_d3_GIN_MsD  <- Node$new("Bank_MsD_Depth3_GINI")
bnk_d4_GIN_MsD  <- Node$new("Bank_MsD_Depth4_GINI")
bnk_d5_GIN_MsD  <- Node$new("Bank_MsD_Depth5_GINI")
bnk_d6_GIN_MsD  <- Node$new("Bank_MsD_Depth6_GINI")
bnk_d7_GIN_MsD  <- Node$new("Bank_MsD_Depth7_GINI")
bnk_d8_GIN_MsD  <- Node$new("Bank_MsD_Depth8_GINI")
bnk_d9_GIN_MsD  <- Node$new("Bank_MsD_Depth9_GINI")
bnk_d10_GIN_MsD <- Node$new("Bank_MsD_Depth10_GINI")
bnk_d11_GIN_MsD <- Node$new("Bank_MsD_Depth11_GINI")
bnk_d12_GIN_MsD <- Node$new("Bank_MsD_Depth12_GINI")
bnk_d13_GIN_MsD <- Node$new("Bank_MsD_Depth13_GINI")
bnk_d14_GIN_MsD <- Node$new("Bank_MsD_Depth14_GINI")
bnk_d15_GIN_MsD <- Node$new("Bank_MsD_Depth15_GINI")
bnk_d16_GIN_MsD <- Node$new("Bank_MsD_Depth16_GINI")


# *** ----




# Train each tree ----

trn_ID3(bnk_d1_ent_MsD,bnk_trn_data,depth=1,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_ent_MsD,bnk_trn_data,depth=2,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_ent_MsD,bnk_trn_data,depth=3,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_ent_MsD,bnk_trn_data,depth=4,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_ent_MsD,bnk_trn_data,depth=5,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_ent_MsD,bnk_trn_data,depth=6,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_ent_MsD,bnk_trn_data,depth=7,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_ent_MsD,bnk_trn_data,depth=8,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_ent_MsD,bnk_trn_data,depth=9,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_ent_MsD,bnk_trn_data,depth=10,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_ent_MsD,bnk_trn_data,depth=11,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_ent_MsD,bnk_trn_data,depth=12,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_ent_MsD,bnk_trn_data,depth=13,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_ent_MsD,bnk_trn_data,depth=14,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_ent_MsD,bnk_trn_data,depth=15,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_ent_MsD,bnk_trn_data,depth=16,PurityMethod="Entropy",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


trn_ID3(bnk_d1_MJe_MsD,bnk_trn_data,depth=1,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_MJe_MsD,bnk_trn_data,depth=2,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_MJe_MsD,bnk_trn_data,depth=3,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_MJe_MsD,bnk_trn_data,depth=4,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_MJe_MsD,bnk_trn_data,depth=5,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_MJe_MsD,bnk_trn_data,depth=6,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_MJe_MsD,bnk_trn_data,depth=7,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_MJe_MsD,bnk_trn_data,depth=8,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_MJe_MsD,bnk_trn_data,depth=9,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_MJe_MsD,bnk_trn_data,depth=10,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_MJe_MsD,bnk_trn_data,depth=11,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_MJe_MsD,bnk_trn_data,depth=12,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_MJe_MsD,bnk_trn_data,depth=13,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_MJe_MsD,bnk_trn_data,depth=14,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_MJe_MsD,bnk_trn_data,depth=15,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_MJe_MsD,bnk_trn_data,depth=16,PurityMethod="Majority Error",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


trn_ID3(bnk_d1_GIN_MsD,bnk_trn_data,depth=1,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d2_GIN_MsD,bnk_trn_data,depth=2,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d3_GIN_MsD,bnk_trn_data,depth=3,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d4_GIN_MsD,bnk_trn_data,depth=4,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d5_GIN_MsD,bnk_trn_data,depth=5,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d6_GIN_MsD,bnk_trn_data,depth=6,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d7_GIN_MsD,bnk_trn_data,depth=7,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d8_GIN_MsD,bnk_trn_data,depth=8,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d9_GIN_MsD,bnk_trn_data,depth=9,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d10_GIN_MsD,bnk_trn_data,depth=10,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d11_GIN_MsD,bnk_trn_data,depth=11,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d12_GIN_MsD,bnk_trn_data,depth=12,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d13_GIN_MsD,bnk_trn_data,depth=13,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d14_GIN_MsD,bnk_trn_data,depth=14,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d15_GIN_MsD,bnk_trn_data,depth=15,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
trn_ID3(bnk_d16_GIN_MsD,bnk_trn_data,depth=16,PurityMethod="GINI",missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


# *** ----




# Train & Test error of each tree ----


# Entropy
bnk_d1_ent_MsD_trnerr  <- test_ID3(bnk_d1_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_ent_MsD_trnerr  <- test_ID3(bnk_d2_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_ent_MsD_trnerr  <- test_ID3(bnk_d3_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_ent_MsD_trnerr  <- test_ID3(bnk_d4_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_ent_MsD_trnerr  <- test_ID3(bnk_d5_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_ent_MsD_trnerr  <- test_ID3(bnk_d6_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_ent_MsD_trnerr  <- test_ID3(bnk_d7_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_ent_MsD_trnerr  <- test_ID3(bnk_d8_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_ent_MsD_trnerr  <- test_ID3(bnk_d9_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_ent_MsD_trnerr <- test_ID3(bnk_d10_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_ent_MsD_trnerr <- test_ID3(bnk_d11_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_ent_MsD_trnerr <- test_ID3(bnk_d12_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_ent_MsD_trnerr <- test_ID3(bnk_d13_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_ent_MsD_trnerr <- test_ID3(bnk_d14_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_ent_MsD_trnerr <- test_ID3(bnk_d15_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_ent_MsD_trnerr <- test_ID3(bnk_d16_ent_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_ent_MsD_tsterr  <- test_ID3(bnk_d1_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_ent_MsD_tsterr  <- test_ID3(bnk_d2_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_ent_MsD_tsterr  <- test_ID3(bnk_d3_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_ent_MsD_tsterr  <- test_ID3(bnk_d4_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_ent_MsD_tsterr  <- test_ID3(bnk_d5_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_ent_MsD_tsterr  <- test_ID3(bnk_d6_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_ent_MsD_tsterr  <- test_ID3(bnk_d7_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_ent_MsD_tsterr  <- test_ID3(bnk_d8_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_ent_MsD_tsterr  <- test_ID3(bnk_d9_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_ent_MsD_tsterr <- test_ID3(bnk_d10_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_ent_MsD_tsterr <- test_ID3(bnk_d11_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_ent_MsD_tsterr <- test_ID3(bnk_d12_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_ent_MsD_tsterr <- test_ID3(bnk_d13_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_ent_MsD_tsterr <- test_ID3(bnk_d14_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_ent_MsD_tsterr <- test_ID3(bnk_d15_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_ent_MsD_tsterr <- test_ID3(bnk_d16_ent_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))



# Majority Error
bnk_d1_MJe_MsD_trnerr  <- test_ID3(bnk_d1_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_MJe_MsD_trnerr  <- test_ID3(bnk_d2_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_MJe_MsD_trnerr  <- test_ID3(bnk_d3_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_MJe_MsD_trnerr  <- test_ID3(bnk_d4_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_MJe_MsD_trnerr  <- test_ID3(bnk_d5_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_MJe_MsD_trnerr  <- test_ID3(bnk_d6_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_MJe_MsD_trnerr  <- test_ID3(bnk_d7_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_MJe_MsD_trnerr  <- test_ID3(bnk_d8_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_MJe_MsD_trnerr  <- test_ID3(bnk_d9_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_MJe_MsD_trnerr <- test_ID3(bnk_d10_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_MJe_MsD_trnerr <- test_ID3(bnk_d11_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_MJe_MsD_trnerr <- test_ID3(bnk_d12_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_MJe_MsD_trnerr <- test_ID3(bnk_d13_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_MJe_MsD_trnerr <- test_ID3(bnk_d14_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_MJe_MsD_trnerr <- test_ID3(bnk_d15_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_MJe_MsD_trnerr <- test_ID3(bnk_d16_MJe_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_MJe_MsD_tsterr  <- test_ID3(bnk_d1_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_MJe_MsD_tsterr  <- test_ID3(bnk_d2_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_MJe_MsD_tsterr  <- test_ID3(bnk_d3_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_MJe_MsD_tsterr  <- test_ID3(bnk_d4_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_MJe_MsD_tsterr  <- test_ID3(bnk_d5_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_MJe_MsD_tsterr  <- test_ID3(bnk_d6_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_MJe_MsD_tsterr  <- test_ID3(bnk_d7_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_MJe_MsD_tsterr  <- test_ID3(bnk_d8_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_MJe_MsD_tsterr  <- test_ID3(bnk_d9_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_MJe_MsD_tsterr <- test_ID3(bnk_d10_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_MJe_MsD_tsterr <- test_ID3(bnk_d11_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_MJe_MsD_tsterr <- test_ID3(bnk_d12_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_MJe_MsD_tsterr <- test_ID3(bnk_d13_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_MJe_MsD_tsterr <- test_ID3(bnk_d14_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_MJe_MsD_tsterr <- test_ID3(bnk_d15_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_MJe_MsD_tsterr <- test_ID3(bnk_d16_MJe_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))



# GINI
bnk_d1_GIN_MsD_trnerr  <- test_ID3(bnk_d1_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_GIN_MsD_trnerr  <- test_ID3(bnk_d2_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_GIN_MsD_trnerr  <- test_ID3(bnk_d3_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_GIN_MsD_trnerr  <- test_ID3(bnk_d4_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_GIN_MsD_trnerr  <- test_ID3(bnk_d5_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_GIN_MsD_trnerr  <- test_ID3(bnk_d6_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_GIN_MsD_trnerr  <- test_ID3(bnk_d7_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_GIN_MsD_trnerr  <- test_ID3(bnk_d8_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_GIN_MsD_trnerr  <- test_ID3(bnk_d9_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_GIN_MsD_trnerr <- test_ID3(bnk_d10_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_GIN_MsD_trnerr <- test_ID3(bnk_d11_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_GIN_MsD_trnerr <- test_ID3(bnk_d12_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_GIN_MsD_trnerr <- test_ID3(bnk_d13_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_GIN_MsD_trnerr <- test_ID3(bnk_d14_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_GIN_MsD_trnerr <- test_ID3(bnk_d15_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_GIN_MsD_trnerr <- test_ID3(bnk_d16_GIN_MsD,bnk_trn_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


bnk_d1_GIN_MsD_tsterr  <- test_ID3(bnk_d1_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d2_GIN_MsD_tsterr  <- test_ID3(bnk_d2_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d3_GIN_MsD_tsterr  <- test_ID3(bnk_d3_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d4_GIN_MsD_tsterr  <- test_ID3(bnk_d4_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d5_GIN_MsD_tsterr  <- test_ID3(bnk_d5_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d6_GIN_MsD_tsterr  <- test_ID3(bnk_d6_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d7_GIN_MsD_tsterr  <- test_ID3(bnk_d7_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d8_GIN_MsD_tsterr  <- test_ID3(bnk_d8_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d9_GIN_MsD_tsterr  <- test_ID3(bnk_d9_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d10_GIN_MsD_tsterr <- test_ID3(bnk_d10_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d11_GIN_MsD_tsterr <- test_ID3(bnk_d11_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d12_GIN_MsD_tsterr <- test_ID3(bnk_d12_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d13_GIN_MsD_tsterr <- test_ID3(bnk_d13_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d14_GIN_MsD_tsterr <- test_ID3(bnk_d14_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d15_GIN_MsD_tsterr <- test_ID3(bnk_d15_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))
bnk_d16_GIN_MsD_tsterr <- test_ID3(bnk_d16_GIN_MsD,bnk_tst_data,numericOut=F,missingData_ID=c(NA,"unknown"),numericColInds=c(1,6,10,12,13,14,15))


# *** ----




# Organize Error Results for Easy Viewing ----

Bnk_Err_Tbl_MsD <- data.frame("Entropy Train Error"=round(c(bnk_d1_ent_MsD_trnerr
                                                       ,bnk_d2_ent_MsD_trnerr
                                                       ,bnk_d3_ent_MsD_trnerr
                                                       ,bnk_d4_ent_MsD_trnerr
                                                       ,bnk_d5_ent_MsD_trnerr
                                                       ,bnk_d6_ent_MsD_trnerr
                                                       ,bnk_d7_ent_MsD_trnerr
                                                       ,bnk_d8_ent_MsD_trnerr
                                                       ,bnk_d9_ent_MsD_trnerr
                                                       ,bnk_d10_ent_MsD_trnerr
                                                       ,bnk_d11_ent_MsD_trnerr
                                                       ,bnk_d12_ent_MsD_trnerr
                                                       ,bnk_d13_ent_MsD_trnerr
                                                       ,bnk_d14_ent_MsD_trnerr
                                                       ,bnk_d15_ent_MsD_trnerr
                                                       ,bnk_d16_ent_MsD_trnerr),3),
                          "Entropy Test Error"=round(c(bnk_d1_ent_MsD_tsterr
                                                      ,bnk_d2_ent_MsD_tsterr
                                                      ,bnk_d3_ent_MsD_tsterr
                                                      ,bnk_d4_ent_MsD_tsterr
                                                      ,bnk_d5_ent_MsD_tsterr
                                                      ,bnk_d6_ent_MsD_tsterr
                                                      ,bnk_d7_ent_MsD_tsterr
                                                      ,bnk_d8_ent_MsD_tsterr
                                                      ,bnk_d9_ent_MsD_tsterr
                                                      ,bnk_d10_ent_MsD_tsterr
                                                      ,bnk_d11_ent_MsD_tsterr
                                                      ,bnk_d12_ent_MsD_tsterr
                                                      ,bnk_d13_ent_MsD_tsterr
                                                      ,bnk_d14_ent_MsD_tsterr
                                                      ,bnk_d15_ent_MsD_tsterr
                                                      ,bnk_d16_ent_MsD_tsterr),3),
                          "Majority Error Train Error"=c(bnk_d1_MJe_MsD_trnerr
                                                        ,bnk_d2_MJe_MsD_trnerr
                                                        ,bnk_d3_MJe_MsD_trnerr
                                                        ,bnk_d4_MJe_MsD_trnerr
                                                        ,bnk_d5_MJe_MsD_trnerr
                                                        ,bnk_d6_MJe_MsD_trnerr
                                                        ,bnk_d7_MJe_MsD_trnerr
                                                        ,bnk_d8_MJe_MsD_trnerr
                                                        ,bnk_d9_MJe_MsD_trnerr
                                                        ,bnk_d10_MJe_MsD_trnerr
                                                        ,bnk_d11_MJe_MsD_trnerr
                                                        ,bnk_d12_MJe_MsD_trnerr
                                                        ,bnk_d13_MJe_MsD_trnerr
                                                        ,bnk_d14_MJe_MsD_trnerr
                                                        ,bnk_d15_MJe_MsD_trnerr
                                                        ,bnk_d16_MJe_MsD_trnerr),
                          "Majority Error Test Error"=round(c(bnk_d1_MJe_MsD_tsterr
                                                             ,bnk_d2_MJe_MsD_tsterr
                                                             ,bnk_d3_MJe_MsD_tsterr
                                                             ,bnk_d4_MJe_MsD_tsterr
                                                             ,bnk_d5_MJe_MsD_tsterr
                                                             ,bnk_d6_MJe_MsD_tsterr
                                                             ,bnk_d7_MJe_MsD_tsterr
                                                             ,bnk_d8_MJe_MsD_tsterr
                                                             ,bnk_d9_MJe_MsD_tsterr
                                                             ,bnk_d10_MJe_MsD_tsterr
                                                             ,bnk_d11_MJe_MsD_tsterr
                                                             ,bnk_d12_MJe_MsD_tsterr
                                                             ,bnk_d13_MJe_MsD_tsterr
                                                             ,bnk_d14_MJe_MsD_tsterr
                                                             ,bnk_d15_MJe_MsD_tsterr
                                                             ,bnk_d16_MJe_MsD_tsterr),3),
                          "GINI Train Error"=round(c(bnk_d1_GIN_MsD_trnerr
                                                    ,bnk_d2_GIN_MsD_trnerr
                                                    ,bnk_d3_GIN_MsD_trnerr
                                                    ,bnk_d4_GIN_MsD_trnerr
                                                    ,bnk_d5_GIN_MsD_trnerr
                                                    ,bnk_d6_GIN_MsD_trnerr
                                                    ,bnk_d7_GIN_MsD_trnerr
                                                    ,bnk_d8_GIN_MsD_trnerr
                                                    ,bnk_d9_GIN_MsD_trnerr
                                                    ,bnk_d10_GIN_MsD_trnerr
                                                    ,bnk_d11_GIN_MsD_trnerr
                                                    ,bnk_d12_GIN_MsD_trnerr
                                                    ,bnk_d13_GIN_MsD_trnerr
                                                    ,bnk_d14_GIN_MsD_trnerr
                                                    ,bnk_d15_GIN_MsD_trnerr
                                                    ,bnk_d16_GIN_MsD_trnerr),3),
                          "GINI Test Error"=round(c(bnk_d1_GIN_MsD_tsterr
                                                   ,bnk_d2_GIN_MsD_tsterr
                                                   ,bnk_d3_GIN_MsD_tsterr
                                                   ,bnk_d4_GIN_MsD_tsterr
                                                   ,bnk_d5_GIN_MsD_tsterr
                                                   ,bnk_d6_GIN_MsD_tsterr
                                                   ,bnk_d7_GIN_MsD_tsterr
                                                   ,bnk_d8_GIN_MsD_tsterr
                                                   ,bnk_d9_GIN_MsD_tsterr
                                                   ,bnk_d10_GIN_MsD_tsterr
                                                   ,bnk_d11_GIN_MsD_tsterr
                                                   ,bnk_d12_GIN_MsD_tsterr
                                                   ,bnk_d13_GIN_MsD_tsterr
                                                   ,bnk_d14_GIN_MsD_tsterr
                                                   ,bnk_d15_GIN_MsD_tsterr
                                                   ,bnk_d16_GIN_MsD_tsterr),3))

row.names(Bnk_Err_Tbl_MsD) <- as.character(seq(1,16,1))

Bnk_Err_Tbl_MsD


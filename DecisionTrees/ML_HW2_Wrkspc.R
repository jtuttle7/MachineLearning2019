# ML_HW2 Workspace



# Librarys ----

source("MachineLearning_JFT.R")


# *** ----




# Functions ----


adaBoost_trn <- function(trn_data,iters,numColInds,PurityMethod="Entropy",firstCall=1){
  
  if(firstCall){
    wgts <- rep(1,nrow(trn_data))
    wgts <- wgts/nrow(trn_data)
    alph <- vector(mode="numeric",length=iters)
  }



}
  



# *** ----




# Read In Bank Datasets ----

# Numeric Cols = c(1,6,10,12,13,14,15)
# Assume no missing data

bnk_trn_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/train.csv",header=F,stringsAsFactors=F)

bnk_tst_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/test.csv",header=F,stringsAsFactors=F)

# *** ----




# Read In Tennis Data ----


tennis_NoHd_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/tennis_noHeader.csv",header=F,stringsAsFactors = F)

tennis_Hd_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/tennis_Header.csv",header=T,stringsAsFactors = F)



# *** ----




# Train Tennis Tree ----

TennisTree <- Node$new("Tennis")

trn_ID3(treeVar=TennisTree,data=tennis_Hd_data,depth=6,PurityMethod = "GINI")

print(TennisTree,"splitVar","isLeaf","ig")


# *** ----




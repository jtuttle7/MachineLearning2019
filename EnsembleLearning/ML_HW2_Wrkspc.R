# ML_HW2 Workspace



# Librarys ----

ggplot2_Flag <- require(ggplot2)
RColorBrewer_Flag <- require(RColorBrewer)

if(!ggplot2_Flag){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!RColorBrewer_Flag){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

source("MachineLearning_JFT.R")


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
# ada_tennis <- adaBoost_trn(trn_data=tennis_Hd_data,iters=5)
# 
# 
# # *** ----
# 
# 
# 
# 
# # Read In Bank Datasets ----
# 
# # Numeric Cols = c(1,6,10,12,13,14,15)
# # Assume no missing data
# 
# bnk_trn_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/train.csv",header=F,stringsAsFactors=F)
# 
# bnk_tst_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW1/bank/test.csv",header=F,stringsAsFactors=F)
# 
# # *** ----
# 
# 
# 
# 
# # Adaboost Bank Model ----
# 
# # Create Boosted Tree
# ada_BnkModel <- adaBoost_trn(trn_data=bnk_trn_data,iters=1000,numericColInds = c(1,6,10,12,13,14,15))
# 
# # Initialize variables to store boosted tree and stump errors
# T_adaTrnErr <- vector(mode="numeric",length=length(ada_BnkModel$Trees))
# T_adaTstErr <- T_adaTrnErr
# stumpTrnErr <- T_adaTrnErr
# stumpTstErr <- T_adaTrnErr
# stumpTrnPreds <- vector(mode="list",length=length(ada_BnkModel$Trees))
# stumpTstPreds <- stumpTrnPreds
# 
# # Go through all iterations of boosted tree and determine the error at each iteration
# # Also determine individual stump error
# # for(i in 1:length(T_adaTrnErr)){
#   # curr_list <- list("Trees" = ada_BnkModel$Trees[1:i],"alphas"=ada_BnkModel$alphas[1:i])
#   #
#   # trnRes <- adaBoost_tst(curr_list,bnk_trn_data,numericColInds=c(1,6,10,12,13,14,15))
#   # T_adaTrnErr[i] = trnRes$Error
#   #
#   # tstRes <- adaBoost_tst(curr_list,bnk_tst_data,numericColInds=c(1,6,10,12,13,14,15))
#   # T_adaTstErr[i] = tstRes$Error
# #
# #   stmpTrnRes <- test_ID3(ada_BnkModel$Trees[[i]],bnk_trn_data,numericColInds=c(1,6,10,12,13,14,15))
# #   stumpTrnErr[i] <- stmpTrnRes$Error
# #   stumpTrnPreds[i] <- stmpTrnRes$Preds
# #
# #   stmpTstRes <- test_ID3(ada_BnkModel$Trees[[i]],bnk_tst_data,numericColInds=c(1,6,10,12,13,14,15))
# #   stumpTstErr[i] <- stmpTstRes$Error
# #   stumpTstPreds[i] <- stmpTstRes$Preds
# #
# # }
# 
# # Identify Unique Labels
# uq_labs_bnkTrn <- unique(bnk_trn_data[,ncol(bnk_trn_data)])
# uq_labs_bnkTst <- unique(bnk_tst_data[,ncol(bnk_tst_data)])
# 
# # Create vector to track accumulatives "votes" of labels for boosted tree
# uq_labs_curSum <- vector(mode="numeric",length=length(uq_labs_bnkTrn))
# 
# # *** ----
# 
# 
# 
# 
# # Ad hoc boosted results of training set ----
# 
# # List to store vectors for each row of training set weighted answer votes
# list_TrnSums <- vector(mode="list",length=nrow(bnk_trn_data))
# 
# curStumpTrnPreds <- vector(length=nrow(bnk_trn_data))
# curBoostTrnPreds <- curStumpTrnPreds
# 
# # Vectors in each space of list initialized to 0
# for(i in 1:length(list_TrnSums)){
#   list_TrnSums[[i]] = uq_labs_curSum
# }
# 
# 
# # Move each tree through all dataset rows and store preds
# for(i in 1:length(ada_BnkModel$Trees)){
#   for(j in 1:nrow(bnk_trn_data)){
# 
#     uq_labs_curSum = list_TrnSums[[j]]
# 
#     # Determine prediction of each row from each tree
#     curResult = test_ID3(ada_BnkModel$Trees[[i]],bnk_trn_data[j,],numericColInds=c(1,6,10,12,13,14,15))
# 
#     # Store the current prediction value
#     curPred <- curResult$Preds
# 
#     # Identify which index the prediction is for weighted vote tracking
#     labID <- which(uq_labs_bnkTrn==curPred)
# 
#     # Store this stumps prediction pure prediction (no weight)
#     curStumpTrnPreds[j] = curPred
#     # Increase weighted vote of current winner by respective weight
#     uq_labs_curSum[labID] = uq_labs_curSum[labID] + ada_BnkModel$alphas[i]
#     # Update this rows weighted votes
#     list_TrnSums[[j]] = uq_labs_curSum
#     # Store boosted tree prediction for this iteration
#     curBoostTrnPreds[j] = uq_labs_bnkTrn[which.max(uq_labs_curSum)]
# 
#   }
# 
#   # Now moved through all rows with one tree, determine error of this stump on whole dataset, and error of boosted tree to this point
# 
# stumpTrnErr[i] = sum(curStumpTrnPreds!=bnk_trn_data[,ncol(bnk_trn_data)])/nrow(bnk_trn_data)
# 
# T_adaTrnErr[i] = sum(curBoostTrnPreds!=bnk_trn_data[,ncol(bnk_trn_data)])/nrow(bnk_trn_data)
# 
# }
# 
# 
# # Ad hoc boosted results of test set
# 
# # List to store vectors for each row of test set weighted answer votes
# list_TstSums <- vector(mode="list",length=nrow(bnk_tst_data))
# 
# curStumpTstPreds <- vector(length=nrow(bnk_tst_data))
# curBoostTstPreds <- curStumpTstPreds
# 
# # Create vector to track accumulatives "votes" of labels for boosted tree
# uq_labs_curSum <- vector(mode="numeric",length=length(uq_labs_bnkTst))
# 
# 
# # Vectors in each space of list initialized to 0
# for(i in 1:length(list_TstSums)){
#   list_TstSums[[i]] = uq_labs_curSum
# }
# 
# # Move each tree through all dataset rows and store preds
# for(i in 1:length(ada_BnkModel$Trees)){
#   for(j in 1:nrow(bnk_tst_data)){
# 
#     uq_labs_curSum = list_TstSums[[j]]
# 
#     # Determine prediction of each row from each tree
#     curResult = test_ID3(ada_BnkModel$Trees[[i]],bnk_tst_data[j,],numericColInds=c(1,6,10,12,13,14,15))
# 
#     # Store the current prediction value
#     curPred <- curResult$Preds
# 
#     # Identify which index the prediction is for weighted vote tracking
#     labID <- which(uq_labs_bnkTst==curPred)
# 
#     # Store this stumps prediction pure prediction (no weight)
#     curStumpTstPreds[j] = curPred
#     # Increase weighted vote of current winner by respective weight
#     uq_labs_curSum[labID] = uq_labs_curSum[labID] + ada_BnkModel$alphas[i]
#     # Update this rows weighted votes
#     list_TstSums[[j]] = uq_labs_curSum
#     # Store boosted tree prediction for this iteration
#     curBoostTstPreds[j] = uq_labs_bnkTst[which.max(uq_labs_curSum)]
# 
#   }
# 
#   # Now moved through all rows with one tree, determine error of this stump on whole dataset, and error of boosted tree to this point
# 
#   stumpTstErr[i] = sum(curStumpTstPreds!=bnk_tst_data[,ncol(bnk_tst_data)])/nrow(bnk_tst_data)
# 
#   T_adaTstErr[i] = sum(curBoostTstPreds!=bnk_tst_data[,ncol(bnk_tst_data)])/nrow(bnk_tst_data)
# 
# }
# 
# 
# # *** ----
# 
# 
# 
# 
# # Plot Boosted Results ----
# 
# # Train and Test Errors of individual stumps and the boosted tree along each iteration
# 
# ggplot() +
#   geom_line(aes(x=1:length(T_adaTrnErr),y=T_adaTrnErr,color="Boosted Train Error")) +
#   geom_line(aes(x=1:length(T_adaTstErr),y=T_adaTstErr,color="Boosted Test Error")) +
#   geom_hline(aes(yintercept=0.013,color="Full Single Tree Train Error"),linetype=2) +
#   geom_hline(aes(yintercept=0.156,color="Full Single Tree Test Error"),linetype=2) +
#   scale_color_manual("",values=c("Boosted Train Error"="#F8766D","Full Single Tree Train Error"="#F8766D","Boosted Test Error"="#00BFC4","Full Single Tree Test Error"="#00BFC4"),breaks=c("Boosted Train Error","Full Single Tree Train Error","Boosted Test Error","Full Single Tree Test Error")) +
#   guides(colour=guide_legend(byrow=T,override.aes = list(size=0.65,linetype=c("Boosted Train Error"=1,"Full Single Tree Train Error"=2,"Boosted Test Error"=1,"Full Single Tree Test Error"=2)))) +
#   theme_grey() +
#   theme(legend.title = element_blank()) +
#   # coord_cartesian(ylim=c(0.09,0.11)) +
#   labs(x="Boosting Iterations",y="Boosted Tree Error")
# 
# ggplot() +
#   geom_line(aes(x=1:length(stumpTrnErr),y=stumpTrnErr,color='Train Error')) +
#   geom_line(aes(x=1:length(stumpTstErr),y=stumpTstErr,color='Test Error')) +
#   theme_grey() +
#   theme(legend.title = element_blank()) +
#   labs(x="Boosting Iterations",y="Individual Stump Error")
# 
# # *** ----
# 
# 
# 
# 
# # Bagging Evaluation ----
# 
# bnkBaggModel <- bagged_ID3_trn(trn_data=bnk_trn_data,iters=1000,numericColInds=c(1,6,10,12,13,14,15))
# 
# 
# bnkBagg_TrnErr <- ErrAlong_Trees(bnk_trn_data,models=bnkBaggModel,numericColInds=c(1,6,10,12,13,14,15))
# bnkBagg_TstErr <- ErrAlong_Trees(bnk_tst_data,models=bnkBaggModel,numericColInds=c(1,6,10,12,13,14,15))
# 
# 
# 
# 
# # *** ----
# 
# 
# 
# 
# # Plot Bagging Results ----
# 
# ggplot() +
#   geom_line(aes(x=1:length(bnkBagg_TrnErr),y=bnkBagg_TrnErr,color="Bagging Train Error")) +
#   geom_line(aes(x=1:length(bnkBagg_TstErr),y=bnkBagg_TstErr,color='Bagging Test Error')) +
#   geom_hline(aes(yintercept=0.013,color="Full Single Tree Train Error"),linetype=2) +
#   geom_hline(aes(yintercept=0.156,color="Full Single Tree Test Error"),linetype=2) +
#   geom_hline(aes(yintercept=0.103,color="Best Boosted Tree Train Error"),linetype=4) +
#   geom_hline(aes(yintercept=0.106,color="Best Boosted Tree Test Error"),linetype=4) +
#   scale_color_manual("",values=c("Bagging Train Error"="#F8766D","Full Single Tree Train Error"="#F8766D","Best Boosted Tree Train Error"="#F8766D","Bagging Test Error"="#00BFC4","Full Single Tree Test Error"="#00BFC4","Best Boosted Tree Test Error"="#00BFC4"),breaks=c("Bagging Train Error","Full Single Tree Train Error","Best Boosted Tree Train Error","Bagging Test Error","Full Single Tree Test Error","Best Boosted Tree Test Error")) +
#   guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Bagging Train Error"=1,"Full Single Tree Train Error"=2,"Best Boosted Tree Train Error"=4,"Bagging Test Error"=1,"Full Single Tree Test Error"=2,"Best Boosted Tree Test Error"=4)))) +
#   theme_grey() +
#   theme(legend.title = element_blank()) +
#   labs(x="Bagging Trees",y="Bagging Error")
# 
# # *** ----
# 
# 
# 
# 
# # Compute Bias and Error ----
# 
# # Bias and Error for Single Trees
# 
# list_singleTrees <- vector(mode="list",length=length(bag_of_bags))
# 
# for(i in 1:length(list_singleTrees)){
# 
#   list_singleTrees[[i]] = bag_of_bags[[i]][[1]]
# 
# }
# 
# 
# list_singTreePreds <- vector(mode="list",length=length(list_singleTrees))
# 
# for(i in 1:length(list_singleTrees)){
# 
#   cursingTreeRes <- test_ID3(list_singleTrees[[i]],testdata=bnk_tst_data,numericColInds=c(1,6,10,12,13,14,15))
# 
#   list_singTreePreds[[i]] = cursingTreeRes$Preds
# 
# }
# 
# singRecBias <- vector(mode="numeric",length=length(list_singTreePreds[[1]]))
# singRecVar  <- singRecBias
# 
# for(i in 1:length(list_singTreePreds[[i]])){
# 
#   # Track total of predictions for this row
#   curRowPreds <- vector(mode="numeric",length=length(list_singTreePreds))
# 
#   for(j in 1:length(list_singTreePreds)){
# 
#     if(list_singTreePreds[[j]][i] == "yes"){
#       curRowPreds[j] = 1
#     }else{
#       curRowPreds[j] = 0
#     }
#   }
# 
#   # What is the numeric value of actual label?
#   if(bnk_tst_data[i,ncol(bnk_tst_data)] == "yes"){
#     actLab <- 1
#   }else{
#     actLab <- 0
#   }
# 
#   singRecBias[i] = (sum(curRowPreds)/length(curRowPreds) - actLab)^2
#   singRecVar[i]  = var(curRowPreds)
# 
# }
# 
# 
# 
# 
# # Bias and Error for Bagged Trees
# 
# 
# list_BaggPreds <- vector(mode="list",length=length(bag_of_bags))
# 
# for(i in 1:length(bag_of_bags)){
# 
#   curBaggRes <- bagged_ID3_tst(bag_of_bags[[i]],testdata=bnk_tst_data,numericColInds=c(1,6,10,12,13,14,15))
# 
#   list_BaggPreds[[i]] = curBaggRes$Preds
# 
# }
# 
# BaggRecBias <- vector(mode="numeric",length=length(list_BaggPreds[[1]]))
# BaggRecVar  <- BaggRecBias
# 
# for(i in 1:length(list_BaggPreds[[i]])){
# 
#   # Track total of predictions for this row
#   curRowPreds <- vector(mode="numeric",length=length(list_BaggPreds))
# 
#   for(j in 1:length(list_BaggPreds)){
# 
#     if(list_BaggPreds[[j]][i] == "yes"){
#       curRowPreds[j] = 1
#     }else{
#       curRowPreds[j] = 0
#     }
#   }
# 
#   # What is the numeric value of actual label?
#   if(bnk_tst_data[i,ncol(bnk_tst_data)] == "yes"){
#     actLab <- 1
#   }else{
#     actLab <- 0
#   }
# 
#   BaggRecBias[i] = (sum(curRowPreds)/length(curRowPreds) - actLab)^2
#   BaggRecVar[i]  = var(curRowPreds)
# 
# }
# 
# 
# # Compute averages of whole sets to get single tree and bagged tree terms
# 
# single_avgBias <- mean(singRecBias)
# single_avgVar  <- mean(singRecVar)
# 
# bagg_avgBias   <- mean(BaggRecBias)
# bagg_avgVar    <- mean(BaggRecVar)
# 
# 
# # *** ----
# 
# 
# 
# 
# # Random Forest Evaluation, Create forests ----
# 
# forst_2fts <- RandForst_ID3_trn(trn_data=bnk_trn_data,iters=100,numericColInds=c(1,6,10,12,13,14,15),ftr_size=2)
# forst_4fts <- RandForst_ID3_trn(trn_data=bnk_trn_data,iters=100,numericColInds=c(1,6,10,12,13,14,15),ftr_size=4)
# forst_6fts <- RandForst_ID3_trn(trn_data=bnk_trn_data,iters=100,numericColInds=c(1,6,10,12,13,14,15),ftr_size=6)
# 
# 
# # *** ----
# 
# 
# 
# 
# # Determine Random Forest Error Vals ----
# 
# frstTreeTrnErr_2fts <- ErrAlongRndFrst(bnk_trn_data,forst_2fts,numericColInds=c(1,6,10,12,13,14,15))
# frstTreeTstErr_2fts <- ErrAlongRndFrst(bnk_tst_data,forst_2fts,numericColInds=c(1,6,10,12,13,14,15))
# 
# frstTreeTrnErr_4fts <- ErrAlongRndFrst(bnk_trn_data,forst_4fts,numericColInds=c(1,6,10,12,13,14,15))
# frstTreeTstErr_4fts <- ErrAlongRndFrst(bnk_tst_data,forst_4fts,numericColInds=c(1,6,10,12,13,14,15))
# 
# frstTreeTrnErr_6fts <- ErrAlongRndFrst(bnk_trn_data,forst_6fts,numericColInds=c(1,6,10,12,13,14,15))
# frstTreeTstErr_6fts <- ErrAlongRndFrst(bnk_tst_data,forst_6fts,numericColInds=c(1,6,10,12,13,14,15))
# 
# # *** ----
# 
# 
# 
# 
# # Plot Random Forest Results ----
# 
# ggplot() +
#     geom_line(aes(x=1:length(frstTreeTrnErr_2fts),y=frstTreeTrnErr_2fts,color="Random Forest Train Error")) +
#     geom_line(aes(x=1:length(frstTreeTstErr_2fts),y=frstTreeTstErr_2fts,color='Random Forest Test Error')) +
#     geom_hline(aes(yintercept=0.064,color="Best Bagged Tree Train Error"),linetype=4) +
#     geom_hline(aes(yintercept=0.106,color="Best Bagged Tree Test Error"),linetype=4) +
#     scale_color_manual("",values=c("Random Forest Train Error"="#F8766D","Best Bagged Tree Train Error"="#F8766D","Random Forest Test Error"="#00BFC4","Best Bagged Tree Test Error"="#00BFC4"),breaks=c("Random Forest Train Error","Best Bagged Tree Train Error","Random Forest Test Error","Best Bagged Tree Test Error")) +
#     guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Random Forest Train Error"=1,"Best Bagged Tree Train Error"=4,"Random Forest Test Error"=1,"Best Bagged Tree Test Error"=4)))) +
#     theme_grey() +
#     theme(legend.title = element_blank()) +
#     labs(x="Random Forest Trees",y="Random Forest of 2 Features Error")
# 
# ggplot() +
#   geom_line(aes(x=1:length(frstTreeTrnErr_4fts),y=frstTreeTrnErr_4fts,color="Random Forest Train Error")) +
#   geom_line(aes(x=1:length(frstTreeTstErr_4fts),y=frstTreeTstErr_4fts,color='Random Forest Test Error')) +
#   geom_hline(aes(yintercept=0.064,color="Best Bagged Tree Train Error"),linetype=4) +
#   geom_hline(aes(yintercept=0.106,color="Best Bagged Tree Test Error"),linetype=4) +
#   scale_color_manual("",values=c("Random Forest Train Error"="#F8766D","Best Bagged Tree Train Error"="#F8766D","Random Forest Test Error"="#00BFC4","Best Bagged Tree Test Error"="#00BFC4"),breaks=c("Random Forest Train Error","Best Bagged Tree Train Error","Random Forest Test Error","Best Bagged Tree Test Error")) +
#   guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Random Forest Train Error"=1,"Best Bagged Tree Train Error"=4,"Random Forest Test Error"=1,"Best Bagged Tree Test Error"=4)))) +
#   theme_grey() +
#   theme(legend.title = element_blank()) +
#   labs(x="Random Forest Trees",y="Random Forest of 4 Features Error")
# 
# 
# ggplot() +
#   geom_line(aes(x=1:length(frstTreeTrnErr_6fts),y=frstTreeTrnErr_6fts,color="Random Forest Train Error")) +
#   geom_line(aes(x=1:length(frstTreeTstErr_6fts),y=frstTreeTstErr_6fts,color='Random Forest Test Error')) +
#   geom_hline(aes(yintercept=0.064,color="Best Bagged Tree Train Error"),linetype=4) +
#   geom_hline(aes(yintercept=0.106,color="Best Bagged Tree Test Error"),linetype=4) +
#   scale_color_manual("",values=c("Random Forest Train Error"="#F8766D","Best Bagged Tree Train Error"="#F8766D","Random Forest Test Error"="#00BFC4","Best Bagged Tree Test Error"="#00BFC4"),breaks=c("Random Forest Train Error","Best Bagged Tree Train Error","Random Forest Test Error","Best Bagged Tree Test Error")) +
#   guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Random Forest Train Error"=1,"Best Bagged Tree Train Error"=4,"Random Forest Test Error"=1,"Best Bagged Tree Test Error"=4)))) +
#   theme_grey() +
#   theme(legend.title = element_blank()) +
#   labs(x="Random Forest Trees",y="Random Forest of 6 Features Error")
# 
# 
# 
# 
# # *** ----
# 
# 
# 
# 
# # *** ----
# 
# 
# 
# 
# # *** ----
# 
# 
# 
# 
# # Bonus - Credit Card Client ----
# 
# 
# 
# # # *** ----
# #
# #
# #
# #
# # # Create Train and Test Data ----
# #
# # full_CCC_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/default_creditCardClients.csv",header=T,stringsAsFactors=F)
# #
# # # Of 30,000 data records, 24,000 to train, 6,000 to test
# #
# # trnInds <- sample(1:nrow(full_CCC_data),24000,replace=F)
# #
# # trn_CCC_data <- full_CCC_data[trnInds,]
# #
# # tst_CCC_data <- full_CCC_data[-trnInds,]
# #
# # write.csv(trn_CCC_data,"G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/train_default_creditCardClients.csv")
# #
# # write.csv(tst_CCC_data,"G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/test_default_creditCardClients.csv")
# #
# 
# # *** ----
# 
# 
# 
# 
# # Read In Credit Card Data ----
# 
# # trn_CCC_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/train_default_creditCardClients.csv",header=T,stringsAsFactors=F)
# #
# # tst_CCC_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/test_default_creditCardClients.csv",header=T,stringsAsFactors=F)
# 
# # Remote Computer
# trn_CCC_data <- read.csv("train_default_creditCardClients.csv",header=T,stringsAsFactors=F)
# 
# tst_CCC_data <- read.csv("test_default_creditCardClients.csv",header=T,stringsAsFactors=F)
# 
# numCols_CCC <- c(1,5,12,13,14,15,16,17,18,19,20,21,22,23)
# 
# # *** ----
# 
# 
# 
# 
# # Create all the credit card default models ----
# 
# # Create single full tree
# sing_CCCTree <- Node$new("Single_CCC_Tree")
# trn_ID3(treeVar=sing_CCCTree,data=trn_CCC_data,numericColInds=numCols_CCC,depth=ncol(trn_CCC_data)-1)
# 
# # Create Boosted Tree
# ada_CCCModel <- adaBoost_trn(trn_data=trn_CCC_data,iters=100,numericColInds=numCols_CCC)
# 
# # Create Bagged Tree
# bag_CCCModel <- bagged_ID3_trn(trn_data=trn_CCC_data,iters=100,numericColInds=numCols_CCC)
# 
# # Create Random Forest
# fst_CCCModel <- RandForst_ID3_trn(trn_data=trn_CCC_data,iters=100,numericColInds=numCols_CCC)
# 
# 
# # *** ----
# 
# 
# 
# 
# # Evaluate Error along Tree Creations ----
# 
# sing_TrnErr_alngTree <- test_ID3(tree=sing_CCCTree,testdata=trn_CCC_data,numericColInds=numCols_CCC)
# sing_TstErr_alngTree <- test_ID3(tree=sing_CCCTree,testdata=tst_CCC_data,numericColInds=numCols_CCC)
# 
# 
# ada_TrnErr_alngTree <- ErrAlong_Trees(testdata=trn_CCC_data,models=ada_CCCModel$Trees,numericColInds=numCols_CCC)
# ada_TstErr_alngTree <- ErrAlong_Trees(testdata=tst_CCC_data,models=ada_CCCModel$Trees,numericColInds=numCols_CCC)
# 
# 
# bag_TrnErr_alngTree <- ErrAlong_Trees(testdata=trn_CCC_data,models=bag_CCCModel,numericColInds=numCols_CCC)
# bag_TstErr_alngTree <- ErrAlong_Trees(testdata=tst_CCC_data,models=bag_CCCModel,numericColInds=numCols_CCC)
# 
# 
# fst_TrnErr_alngTree <- ErrAlongRndFrst(testdata=trn_CCC_data,models=fst_CCCModel,numericColInds=numCols_CCC)
# fst_TstErr_alngTree <- ErrAlongRndFrst(testdata=tst_CCC_data,models=fst_CCCModel,numericColInds=numCols_CCC)
# 
# # *** ----
# 
# 
# 
# 
# # Plot Credit Card Train and Test Error ----

# Train Error
ggplot() +
  geom_hline(aes(yintercept=sing_TrnErr_alngTree$Error,color="Full Single Tree Train Error"),linetype=2) +
  geom_line(aes(x=1:length(ada_TrnErr_alngTree),y=ada_TrnErr_alngTree,color="Boosting Train Error")) +
  geom_line(aes(x=1:length(bag_TrnErr_alngTree),y=bag_TrnErr_alngTree,color="Bagging Train Error")) +
  geom_line(aes(x=1:length(fst_TrnErr_alngTree),y=fst_TrnErr_alngTree,color="Random Forest Train Error")) +
  scale_color_manual("",values=brewer.pal(4,"Dark2"),breaks=c("Full Single Tree Train Error","Boosting Train Error","Bagging Train Error","Random Forest Train Error")) +
  guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Full Single Tree Train Error"=2,"Boosting Train Error"=1,"Bagging Train Error"=4,"Random Forest Train Error"=1)))) +
  theme_grey() +
  coord_cartesian(ylim=c(0.2,0.55)) +
  theme(legend.title = element_blank()) +
  labs(x="Number of Trees",y="Train Error")


# Test Error
ggplot() +
  geom_hline(aes(yintercept=sing_TstErr_alngTree$Error,color="Full Single Tree Test Error"),linetype=2) +
  geom_line(aes(x=1:length(ada_TstErr_alngTree),y=ada_TstErr_alngTree,color="Boosting Test Error")) +
  geom_line(aes(x=1:length(bag_TstErr_alngTree),y=bag_TstErr_alngTree,color="Bagging Test Error")) +
  geom_line(aes(x=1:length(fst_TstErr_alngTree),y=fst_TstErr_alngTree,color="Random Forest Test Error")) +
  scale_color_manual("",values=brewer.pal(4,"Dark2"),breaks=c("Full Single Tree Test Error","Boosting Test Error","Bagging Test Error","Random Forest Test Error")) +
  guides(colour = guide_legend(byrow=T,override.aes = list(size=0.65,linetype = c("Full Single Tree Test Error"=2,"Boosting Test Error"=1,"Bagging Test Error"=4,"Random Forest Test Error"=1)))) +
  theme_grey() +
  coord_cartesian(ylim=c(0.2,0.55)) +
  theme(legend.title = element_blank()) +
  labs(x="Number of Trees",y="Test Error")















# *** ----




# *** ----




# Read in Concrete Data ----


# "G:/My Drive/R_Scripts/19_Spring_ML_Course"

trn_conc_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/concrete/concrete/train.csv",header=F,stringsAsFactors=F)

tst_conc_data <- read.csv("G:/My Drive/U_of_U/19_Spring/MachineLearning/HW/HW2/concrete/concrete/test.csv",header=F,stringsAsFactors=F)

# Batch Gradient Descent ----


conc_trnBatchRes <- trn_batch_GradDsct(trndata=trn_conc_data,lrnRate=0.005)

ggplot() + geom_line(aes(x=1:length(conc_trnBatchRes$TrnCst),y=conc_trnBatchRes$TrnCst)) +
  labs(x="Training Steps",y="Cost Function Value")

conc_tstBatchRes <- eval_batch_GradDsct(testdata=tst_conc_data,gradDsct_wgts=conc_trnBatchRes$Weights)

print(conc_tstBatchRes$SSE)


# *** ----




# Stochastic Gradient Descent ----

conc_trnStochRes <- trn_stoch_GradDsct(trndata=trn_conc_data,lrnRate=0.004,tol=1e-5)

ggplot() + geom_line(aes(x=1:length(conc_trnStochRes$TrnCst),y=conc_trnStochRes$TrnCst))+
  labs(x="Training Steps",y="Cost Function Value")

conc_tstStochRes <- eval_batch_GradDsct(testdata=tst_conc_data,gradDsct_wgts=conc_trnStochRes$Weights)

print(conc_tstStochRes$SSE)


optimalWeightVector <- (t(as.matrix(cbind(rep(1,nrow(trn_conc_data)),trn_conc_data[,-ncol(trn_conc_data)]))) %*% (as.matrix(cbind(rep(1,nrow(trn_conc_data)),trn_conc_data[,-ncol(trn_conc_data)]))))^(-1) %*% (t(as.matrix(cbind(rep(1,nrow(trn_conc_data)),trn_conc_data[,-ncol(trn_conc_data)]))) %*% as.matrix(trn_conc_data[,ncol(trn_conc_data)],nrow=nrow(trn_conc_data)))

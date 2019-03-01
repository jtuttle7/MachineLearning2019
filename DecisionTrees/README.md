The first is ML_HW1_explore.R

This file is self contained, and essentially a calculator for determining the 
answers to the boolean classifier and parts of the Tennis problems (the rest 
this problem is solved using the excel file "Tennis_Dataset.xlsx")

The second is trn_decTree.R

This script file contains the function definitions used to train and test 
decision trees from .csv files.

The important functions are trn_ID3 and test_ID3

As the names suggest, trn_ID3 is used to train a decision tree model, and test_ID3 is used
to test that decision tree against known labels.

trn_ID3 has no direct output, but trains a pre-existing data.tree node (which 
must be passed to the function as input). It is called as follows:

trn_ID3(treeVar,data,depth,PurityMethod="Entropy",missingData_ID=NA,numericColInds=0,labelInd=ncol(data),levelName="Base",firstCall=1)

Input :
treeVar - data.tree variable
data - data.frame containing training data and assocaited label
depth - the maximum depth allowed for the tree
PurityMethod - the method of computing information gain. Available choices are "Entropy",
"Majority Error", and "GINI"
missingData_ID - allows the use to define what missing data looks like in the dataset.
blank cells (NA) are used as default. Other identifies must be identified within a character vector
numericColInds - used to identify columns of numeric data within the training set. The column indicies
are input as a column vector {c(2,5,6)}. This allows the numeric values to be transformed into 
binary decisions based on the median of the data.
labelInd - lets the user define where the label is located in the dataset. It is the last column by default
levelName - assign a name to the data tree level
firstCall - used internally for preprocessing purposes

Results :
The input data.tree variable is now a trained decision tree model


The second important function is test_ID3, which is called as follows:

test_ID3(tree,testdata,missingData_ID=NA,numericColInds=0,numericOut=F,labelInd=ncol(testdata))

Input :
tree - the name of the trained decision tree model
testdata - a data.frame containing the attributes to be predicted on
missingData_ID - allows the use to define what missing data looks like in the dataset.
blank cells (NA) are used as default. Other identifies must be identified within a character vector
numericColInds - used to identify columns of numeric data within the training set. The column indicies
are input as a column vector {c(2,5,6)}. This allows the numeric values to be transformed into 
binary decisions based on the median of the data.
numericOut - Allows the user to define if the output of each prediction should be a numeric 
labelInd - lets the user define where the label is located in the dataset. It is the last column by default

Output : 
The overall error between the model's predictions and the labels within the test dataset

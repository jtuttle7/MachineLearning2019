To address the problems associated with HW2 of CS6350, a number of functions were created

These functions are all contained within the script, MachineLearning_JFT.R 

Each modeling method is constructed in essentially the same way. 

There exists a training function, a predicting function ,and a total testing function.

The training function for each method requires a training dataset to be given to it, as 
well as the algorithm specific features (such as tree depth, feature size splitting, 
bag size percentage, etc.) as well as the the column the label resides in in the dataset 
to be identified, and the column indexes of columns which contain numeric data to be identified.
Numeric data is transformed to binary classification using the median of the training data
in that column.
The training function handles all the data pre-processing that must occur to handle missing
features and the numeric transformation.

The general inputs to these functions are as follows:

Inputs : 

trn_data       	- The training dataset (as data.frame, directly from read_csv)
iters          	- The number of training iterations to perform, or number of bagged/boosted/random trees to build
numericColInds 	- Column indexes of numeric (continous) data, to be processed using median of column (base 1)
PurityMethod 	- Which method of information gain to use. Options are "Entropy", "Majority Error", or "GINI"
missingData_ID 	- Vector containing ID of missiing data, by default = NA
labelInd 	- Column index of labels in dataset (base 1)

Returns : 

Each training function returns a list containing the trained trees. These are used directly by the test functions

Inputs unique to bagged training

bagPct 		- The percentage of the train data to use for training each bagged tree
w_replace 	- Should the model replace data for sampling within each bag? (T or F)

Inputs unique to random forest training

ftr_splt_sz 	- Specifies how many features to use for each random tree


The prediction function takes in a single data attribute, and predicts a label from the 
trained model. This function cannot stand alone, and must be utilized within context of the
test function in order to properly handle numerics and missing data. In essence, it is a helper function.



The test function requires a model and testdata to be input. There are defaults for missing
data, numeric columns, and label index, but these basicaly assume no missing data, no numeric 
columns, and the label at the end of the dataset. If these assumptions are not the case, they 
must be specified.

The general inputs to these functions are as follows:

Inputs : 

model 		- Model of the relevant form, this will be the model attribute from the output list of the train function
testdata 	- a data.frame (directly from read_csv) containing the test data
numericColInds 	- Column indexes of numeric (continous) data, to be processed using median of column (base 1)
missingData_ID 	- Vector containing ID of missiing data, by default = NA. Must correspond with that used during training
numericOut 	- Identify if output predictions should be of class "numeric" (T or F)
labelInd 	- Column index of labels in dataset (base 1)



All functions are constructed following the same basic format. As such, the general description 
above should be enough to become familiar with their usage.
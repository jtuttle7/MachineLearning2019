To address the problems associated with HW2 of CS6350, a number of functions were created

These functions are all contained within the script, MachineLearning_JFT.R 

Each modeling method is constructed in essentially the same way. 

There exists a training function, and a testing function.

Each modeling method is constructed in essentially the same way. 

There exists a training function and a testing (eval) function.

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

trn_data    - The training dataset (as data.frame, directly from read_csv)
wgts        - A vector containing the weights to be updated during training. This is automatically 		
			  initialized for the first training instance.
lrnRate 	- A hyper parameter for tuning the learning process
tol 		- The tolerance of change in weight vectors to determine when training is complete
totCst 		- Vector containing the cost vector value for each epoch of training
labelInd 	- Column index of labels in dataset (base 1)

Returns : 

Each training function returns a vector containing the learned weights for later prediction ("Weights"), and the total cost of each epoch ("TrnCst".




The test function requires a model and testdata to be input. There are defaults for missing
data, numeric columns, and label index, but these basicaly assume no missing data, no numeric 
columns, and the label at the end of the dataset. If these assumptions are not the case, they 
must be specified.

The general inputs to these functions are as follows:

Inputs : 

testdata 	- a data.frame (directly from read_csv) containing the test data
model_wgts  - Weight vector determined from training 
labelInd 	- Column index of labels in dataset (base 1)


Returns :

SSE 		- The total cost of the test data set
Preds 		- The prediction of each feature in the test data


All functions are constructed following the same basic format. As such, the general description 
above should be enough to become familiar with their usage.
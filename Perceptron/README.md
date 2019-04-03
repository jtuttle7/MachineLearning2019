README.md for Perceptron Functions

In order to create Perceptron machine learnign models, 3 training functions and 3 evaluation functions are utilized. The three function correspond to standard, voted, and averaged perceptrons. THe functions for each type follow the same structure and methods, so only descriptions for using a training function and an evaluation function will be provided.

Training :

Inputs  : trndata - dataframe object containing the training data (features and labels)
		  wgts    - previous weight matrix (automatically defined, user should not define)
		  lrnRate - Learning rate of the perceptron algorithm (recommend 0 to 1)
		  epoch   - number of times to train through entire dataset
		  labelInd- the column index in the training data where the label resides (by default the last column)
		 
 Outputs: wgts    - a vector containing the respective weights which define the learned perceptron. This vector is used directly by the evluation function.
 
 
 Evaluation : 
 
 Inputs  : testdata       - dataframe object containing the test data to be evaluated by the Perceptron models
		   stnd_wgt_Prcpt - the weights vector representing the learned Perceptron model (output of the training function)
		   labelInd 	  - the column index in the training data where the label resides (by default the last column)
		   
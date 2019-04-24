README.md - Neural Networks

There are 4 separate functions that work together to train and predict using neural network models.
Of these, only two are directly called by the user (one for training, and the other for predicting). 
The other two functions are used to calculate the value of the sigmoid function, and to perform back 
propagation to tune the weights within the neural network. 

The training function must be given a training dataset, the structure of the hidden nodes (passed as a 
vector, with each input representing the number of nodes in a layer). As well, the method of weight 
initialization can be designated as "StdNorm" or "Zeros". The maximum number of epochs to complete,
and two hyperparameters to tune the learning rate, gamma and d can also be passed. All other inputs have
 pecified defaults that are satisfactory for most situations.

The training function utilizes stochastic gradient descent to solve the objective minimization problem. 

To predict a classificaton using the trained neural network results, the training results are directly
passed to the prediction function. The only other necessary item is a test dataset. The prediciton function will return the average classification error on the dataset, and the individual predictions for each
set of features in the dataset.
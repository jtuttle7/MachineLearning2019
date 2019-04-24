README.md - SVM Training and Predicting

There are 5 separate functions that work together to train, predict, and evaluate SVM models.
Of these, there are two training functions, two evaluation functions, and one prediction function.

The two training and evaluation function correspond to training through the prime or dual 
objective function formulation. The prime problem utilizes stochastic gradient descent to 
iterate toward the optimal result. The dual problem utilizes convex optimization to determine the 
support vectors and weights which best represent the training set.

Use of the two training functions are similar. They require a training dataset to be specified,
and the desired number of epochs, the objective function hyperparameter C, and two hyperparameters 
to tune the learning rate, gamma and d. All other inputs have specified defaults that are satisfactory
for most situations.

The dual problem requires that the type of kernel to be used be specified. Currenlty, only a "linear" and
a "Gaussian" kernel are available to the user.

To evaluate the trained SVM models (or predict the results of a dataset), the evaluation functions
must be used. For both the prime and dual problem, these require that the output of the training
function be passed directly to the evaluation function, and a test dataset be specified. For the dual problem,
a kernel type must be specified. This kernel must match that which ws used to train the model to 
realize reliable results.
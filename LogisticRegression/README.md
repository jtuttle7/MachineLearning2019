README.md - Logistic Regression

There are 4 separate functions that work together to train and evaluate logistic regression models.
Of these, there are two training functions and two evaluation functions.

The two training and evaluation function correspond to training using the maximum likelihood estimator
(MLE) objective, and the maximum a posteriori (MAP) objective. The MAP objective assumes the weight
vector belons to the standard normal distribution.

Use of the two training functions are similar. They require a training dataset to be specified,
and the desired number of epochs, the objective function hyperparameter variance, and two hyperparameters 
to tune the learning rate, gamma and d. All other inputs have specified defaults that are satisfactory
for most situations.

The MLE formulation does not require the variance parameter, as it does not exist in that definition.

To evaluate the trained MLE or MAP models (or predict the results of a dataset), the evaluation functions
must be used. For both model types, the weight vector outpu as part of the training results must be passed
to the evaluation function, and a test dataset be specified. The evaluation function will return the 
average error on the full test data set, as well as provide a vector corresponding to the model predictions
for the test dataset.
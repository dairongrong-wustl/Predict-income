# Set the level of the test dataset to be the same as that of the training dataset

levels(adult_test_complet$native_country) = levels(adult_data_complet$native_country)

library(e1071)

# The following code is to tune the parameter "cost" according to cross validation, tune.out is
# for radial basis function kernal,  tune.out_polynomial is for polynomial kernal. 
# tune.out$best.model stores the best model using the RBF kernel, which has the lowest 
# cross-validation error rate. tune.out_polynomial$best.model stores the best model using the 
# polynomial kernel. Then these models were used by predict function to predict the incomes 
# for all of the imputed test cases in the census dataset. Moreover, tune.out$best.model$coefs 
# and tune.out_polynomial$best.model$coefs can provide the resulting alpha's for the best model.
# Table and mean function can be used to calcuate the accuracy.

tune.out = tune(svm, 
                income~.,
                data = adult_data_complet, 
                kernel = "radial", 
                ranges=list(cost=c(0.1,1,10,100)))
radial_predict = predict(tune.out$best.model, newdata = adult_test_complet)
table(test.income,radial_predict)
mean(test.income == radial_predict)
tune.out$best.model$coefs
tune.out$best.model$labels
for (i in 5424:10596){ tune.out$best.model$coefs[i] = tune.out$best.model$coefs[i]/2 }
plot(tune.out$best.model$coefs)


tune.out_polynomial = tune(svm,
                           income~.,
                           data = adult_data_complet,
                           kernel = "polynomial",
                           ranges=list(cost=c(0.1,1,10,100)))
polynomial_predict = predict(tune.out_polynomial$best.model, newdata = adult_test_complet)
table(test.income,polynomial_predict)
mean(test.income == polynomial_predict)
for (i in 5717:11225){ tune.out_polynomial$best.model$coefs[i] = tune.out_polynomial$best.model$coefs[i]/2 }
plot(tune.out_polynomial$best.model$coefs)


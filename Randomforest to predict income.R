# Inorder to compute the accuracy, I created test.income to represent true classifiers. It then will be
# Compared with the predicted classifiers. Because true classifiers(test.income) has different levels
# than the predicted one. So I changed them to have the same level. 

adult_test_complet$income = as.character(adult_test_complet$income)
adult_test_complet$income[adult_test_complet$income == " <=50K."] = " <=50K"
adult_test_complet$income[adult_test_complet$income == " >50K."] = " >50K"
adult_test_complet$income = as.factor(adult_test_complet$income)
test.income = adult_test_complet$income
levels(test.income) = c(" <=50K", " >50K")

library(boot)

# 1000 Boosttrapping datasets, sampled with replacement from the imputed training data set (adult_data_complet) were used to build decision trees 
# and predict the income for the test dataset. Average accuracy for the test dataset was calculated by mean(test.income == prune.predict)
# where test.income is the true classifier and prune.predict is the predicted classifier.

boot_fn_test = function(data, index){
  tree.income <- tree(income~.-native_country, data =data, subset=index)
  prune.income = prune.misclass(tree.income, best =5)
  prune.predict = predict(prune.income,adult_test_complet,type = "class")
  return(mean(test.income == prune.predict))}

boot(adult_data_complet, boot_fn_test, R=1000)


# Random Forests were used to de-correlate the trees. Build 1000 decision trees on bootstrapped training sample, but when building these trees, 
# each time a split in a tree is considered, a random sample of 7 predictors is chosen as split candidates from the full set of 53 predictors. 
# I choose 7 because 7 is close to the square root of 53. 

randomtree = randomForest::randomForest(income~.-native_country, data = adult_data_complet,
                           ntree = 1000, mtry = 7)

# apply Random Forest method to predict the incomes for all of the test cases in the census dataset.
# The average accuracy over all test cases is computed using mean(trandomtree.predict == test.income),
# where test.income is the true classifier and randomtree.predict is the predicted classifier from random Forest

randomtree.predict = predict(randomtree, newdata = adult_test_complet)
mean(randomtree.predict == test.income)

# Check the influence of number of trees to the accuracy of bootstrapping and randomforest. 
print("Bootstrape 100 dataset")
boot(adult_data_complet, boot_fn_test, R=100)
print("random Forest 100 dataset")
randomtree = randomForest::randomForest(income~.-native_country, data = adult_data_complet,
                                        ntree = 100, mtry = 7)
randomtree.predict = predict(randomtree, newdata = adult_test_complet)
mean(randomtree.predict == test.income)


print("Bootstrape 500 dataset")
boot(adult_data_complet, boot_fn_test, R=500)
print("random Forest 500 dataset")
randomtree = randomForest::randomForest(income~.-native_country, data = adult_data_complet,
                                        ntree = 500, mtry = 7)
randomtree.predict = predict(randomtree, newdata = adult_test_complet)
mean(randomtree.predict == test.income)


print("Bootstrape 1500 dataset")
boot(adult_data_complet, boot_fn_test, R=1500)
print("random Forest 1500 dataset")
randomtree = randomForest::randomForest(income~.-native_country, data = adult_data_complet,
                                        ntree = 1500, mtry = 7)
randomtree.predict = predict(randomtree, newdata = adult_test_complet)
mean(randomtree.predict == test.income)

print("Bootstrape 2000 dataset")
boot(adult_data_complet, boot_fn_test, R=2000)
print("random Forest 2000 dataset")
randomtree = randomForest::randomForest(income~.-native_country, data = adult_data_complet,
                                        ntree = 2000, mtry = 7)
randomtree.predict = predict(randomtree, newdata = adult_test_complet)
mean(randomtree.predict == test.income)


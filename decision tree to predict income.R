library(tree)

#create dummy values for training data because to implement decision tree in R, factor predictors must have at most 32 levels.

library(dummies)
adult_data_complet <- cbind(adult_data_complet,dummy(adult_data_complet$native_country))

# tree creation for training data without prunning 

for (i in 15:55){
  names(adult_data_complet)[i]= paste("is_",unlist(strsplit(names(adult_data_complet)[i]," "))[2],sep = "")}

names(adult_data_complet)[c(20,22,29,42,47,52,53)]=c("is_Dominican_Republic","is_El_Salvador","is_Holand_Netherlands", 
                                                     "is_Outlying_US","is_Puerto_Rico","is_Trinadad_and_Tobago","is_United_States")

tree.income <- tree(income~.-native_country, data = adult_data_complet)
summary(tree.income)
plot(tree.income)
text(tree.income,pretty=0)


# Create dummy values for test data, add In the test data, none the observations' native_country is Holand_Netherlands while 
# there are some training observations come from Holand_Netherlands. 
# So I add one column "Is Holand_Netherlands" to the test data and set its value to be 0 for all test observations.

adult_test_complet <- cbind(adult_test_complet,dummy(adult_test_complet$native_country))
for (i in 15:54){
  names(adult_test_complet)[i]= paste("is_",unlist(strsplit(names(adult_test_complet)[i]," "))[2],sep = "")}

names(adult_test_complet)[c(20,22,41,46,51,52)]=c("is_Dominican_Republic","is_El_Salvador","is_Outlying_US","is_Puerto_Rico",
                                                  "is_Trinadad_and_Tobago","is_United_States")
adult_test_complet[55]=0
names(adult_test_complet)[55] = "is_Holand_Netherlands"

# Using tree model(without prunning) to predict test data

tree.predict = predict(tree.income,adult_test_complet,type = "class")
test.income =  adult_test_complet$income 
table(test.income, tree.predict)


# prunning the tree according to the classification error rate

cv.income = cv.tree(tree.income,FUN=prune.misclass)
par(mfrow=c(1,2))

# Plot tree size vs error rate to decide the tree size for the pruned tree

plot(cv.income$size,cv.income$dev,type="b", xlab = "The number of terminal nodes(tree size)", ylab = "Corresponding error rate")

plot(cv.income$k,cv.income$dev,type="b")

# Prune the tree to a 5 node tree. 

prune.income = prune.misclass(tree.income, best =5)

# Caculate the accuracy of decision tree for the training data 

summary(prune.income)
train.income = adult_data_complet$income
prune.predict = predict(prune.income,adult_data_complet,type = "class")
table(train.income, prune.predict)


# using prunned tree to make prediction for the test data and calculate the accuracy for the test data 

prune.predict = predict(prune.income,adult_test_complet,type = "class")
table(test.income, prune.predict)
plot(prune.income)
text(prune.income)



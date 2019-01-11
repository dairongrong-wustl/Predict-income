# read in the adult_data, remove education level column, create set_A to be the subset without missing value
# and set_B to be the subset with missing value

library(class)
adult_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", 
                         sep = ",", header = FALSE, na.strings = " ?")
adult_data <- adult_data[,-4]
set_A <- adult_data[complete.cases(adult_data),]
set_B <- adult_data[!complete.cases(adult_data),]


# The function I have used is called Fill in NA values with the values of the nearest neighbours (knnImputation {DMwR}).
# This function will automatically create dummy variables for the categorical variable. I set the scale = T 
# to normalize each dimension. 

library("DMwR")
impute_missing_valuables_training <- knnImputation(set_B, k = 5, scale = T, meth = "median",
                                          distData = set_A)
adult_data_complet <- rbind(set_A,impute_missing_valuables_training)
colnames(adult_data_complet) <- c("age", "workclass","fnlwgt","education_num","marital_status",
                         "occupation","relationship","race","sex","capital_gain","capital_loss",
                         "hours_per_week","native_country","income")
colnames(impute_missing_valuables_training) <- c("age", "workclass","fnlwgt","education_num","marital_status",
                                  "occupation","relationship","race","sex","capital_gain","capital_loss",
                                  "hours_per_week","native_country","income")
write.table(adult_data_complet,file = "adult_data_complet_meth_median.txt", sep = ",", row.names = FALSE,
            col.names = TRUE )
write.table(impute_missing_valuables_training,file = "impute_missing_valuables_meth_median_training.txt", sep = ",", row.names = FALSE,
            col.names = TRUE )

# The following code is to fill in NA values with KNN for adult_test. The above method applied here

library(class)
adult_test <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", 
                         sep = ",", header = FALSE, na.strings = " ?", skip =1)
adult_test <- adult_test[,-4]
set_C <- adult_test[complete.cases(adult_test),]
set_D <- adult_test[!complete.cases(adult_test),]


# The function I have used is called Fill in NA values with the values of the nearest neighbours (knnImputation {DMwR}).
# This function will automatically create dummy variables for the categorical variable. I set the scale = T 
# to normalize each dimension. 

library("DMwR")
impute_missing_valuables_test <- knnImputation(set_D, k = 5, scale = T, meth = "median",
                                          distData = set_C)
adult_test_complet <- rbind(set_C,impute_missing_valuables_test)
colnames(adult_test_complet) <- c("age", "workclass","fnlwgt","education_num","marital_status",
                                  "occupation","relationship","race","sex","capital_gain","capital_loss",
                                  "hours_per_week","native_country","income")
colnames(impute_missing_valuables_test) <- c("age", "workclass","fnlwgt","education_num","marital_status",
                                        "occupation","relationship","race","sex","capital_gain","capital_loss",
                                        "hours_per_week","native_country","income")
write.table(adult_test_complet,file = "adult_test_complet_meth_median.txt", sep = ",", row.names = FALSE,
            col.names = TRUE )
write.table(impute_missing_valuables_test,file = "impute_missing_valuables_meth_median_test.txt", sep = ",", row.names = FALSE,
            col.names = TRUE )
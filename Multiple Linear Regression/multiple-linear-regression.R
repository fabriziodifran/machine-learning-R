# Multiple linear regression

# In this example I want to find out which features are involved in the profits of
# 50 differents startups and how this features are statistically significative.

# Importing dataset
dataset = read.csv('data/50_Startups.csv')

# Encoding categorical data, in this case the name of the state where the
# startup is

dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting data into training and test data
# install.packages('caTools')
library('caTools')
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Linear regression to the training data
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

# Building the optimal model using Backward Elimination
# p value threshold chosen to be 0.06

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

# State variable removed
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)

# Administration variable removed
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)



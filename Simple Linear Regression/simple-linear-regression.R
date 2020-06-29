# Simple linear regression

# In this example I want to find the relation between the salary and the years of experience
# of a person, in order to know more about job offers and salary pretensions.

# Importing dataset
dataset = read.csv('data/Salary_data.csv')

# Splitting data into training and test data
# install.packages('caTools')
library('caTools')
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Linear regression to the training data
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

# Visualizing the training set results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour='red' ) +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
                colour='blue ') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience')+
  ylab('Salary')


# Visualizing the test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour='red' ) +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour='blue ') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience')+
  ylab('Salary')



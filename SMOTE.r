# Install required packages
install.packages(c("DMwR", "randomForest"))

# Load libraries
library(DMwR)
library(randomForest)

# Assume the last column 'Fraud' is the target variable
data <- read.csv("credit_card_transactions.csv")
data$Fraud <- as.factor(data$Fraud)  # Ensure the target is a factor

# Handle missing values, normalize data, and encode categorical variables as before...

# Split the data into training and test set
set.seed(123)
split <- sample.split(data$Fraud, SplitRatio = 0.7)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Use SMOTE to oversample the minority class in the training data
set.seed(123)
train_set_balanced <- SMOTE(Fraud ~ ., data  = train_set, perc.over = 600, k = 5)

# Build the random forest model
set.seed(123)
fraud_model <- randomForest(Fraud ~ ., data = train_set_balanced, ntree = 100)

# Predict the test set results
fraud_pred <- predict(fraud_model, newdata = test_set)

# Create the confusion matrix
cm <- table(test_set$Fraud, fraud_pred)
print(cm)

# Calculate accuracy
accuracy <- sum(diag(cm))/sum(cm)
print(paste('Accuracy:', round(accuracy*100, 2), '%'))

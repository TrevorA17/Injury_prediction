# Set seed for reproducibility
set.seed(123)

# 1. Data Splitting (80% Training, 20% Testing)
train_index <- sample(1:nrow(InjuryData), 0.8 * nrow(InjuryData))
train_data <- InjuryData[train_index, ]
test_data <- InjuryData[-train_index, ]
cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# 2. Bootstrapping (Sampling with replacement)
n_bootstrap <- 1000
bootstrap_samples <- matrix(NA, nrow = n_bootstrap, ncol = nrow(InjuryData))
for(i in 1:n_bootstrap) {
  bootstrap_samples[i, ] <- sample(1:nrow(InjuryData), nrow(InjuryData), replace = TRUE)
}
head(bootstrap_samples)

# 3. Cross-Validation (Caret package)
install.packages("caret")
library(caret)

# a. Basic k-Fold Cross-Validation (10-fold)
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(Likelihood_of_Injury ~ ., data = train_data, method = "glm", family = "binomial", trControl = train_control)
print(model_cv)

# b. Repeated k-Fold Cross-Validation (10-fold, repeating 3 times)
train_control_repeated <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_repeated_cv <- train(Likelihood_of_Injury ~ ., data = train_data, method = "glm", family = "binomial", trControl = train_control_repeated)
print(model_repeated_cv)

# c. Leave-One-Out Cross-Validation (LOOCV)
train_control_loocv <- trainControl(method = "LOOCV")
model_loocv <- train(Likelihood_of_Injury ~ ., data = train_data, method = "glm", family = "binomial", trControl = train_control_loocv)
print(model_loocv)

# 4. Model Training with Decision Tree (rpart)
model_dt <- train(Likelihood_of_Injury ~ ., data = train_data, method = "rpart", trControl = train_control)
print(model_dt)

# 5. Model Training with Random Forest (rf)
model_rf <- train(Likelihood_of_Injury ~ ., data = train_data, method = "rf", trControl = train_control)
print(model_rf)
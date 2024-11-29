# Set up grid of hyperparameters for tuning
rf_grid <- expand.grid(
  mtry = c(1, 2, 3, 4, 5),  # Number of variables to try at each split
  ntree = c(50, 100, 200)   # Number of trees
)

# Train the Random Forest model with hyperparameter tuning
model_rf_tuned <- train(Likelihood_of_Injury ~ ., data = train_data, method = "rf", 
                        trControl = train_control, tuneGrid = rf_grid)

# Check the best tuning parameters
model_rf_tuned$bestTune

# Decision Tree Hyperparameter Tuning Example
dt_grid <- expand.grid(cp = c(0.01, 0.05, 0.1, 0.15))  # Complexity parameter for pruning

model_dt_tuned <- train(Likelihood_of_Injury ~ ., data = train_data, method = "rpart", 
                        trControl = train_control, tuneGrid = dt_grid)

# Check the best tuning parameter for Decision Tree
model_dt_tuned$bestTune

# Example of using Gradient Boosting Machine (GBM) for classification
model_gbm <- train(Likelihood_of_Injury ~ ., data = train_data, method = "gbm", 
                   trControl = train_control, verbose = FALSE)

# Compare all models using resampling
models_list <- resamples(list(GLM = model_gbm, DecisionTree = model_dt_tuned, RandomForest = model_rf, GBM = model_gbm))
summary(models_list)

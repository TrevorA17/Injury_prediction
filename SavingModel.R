# Save the Random Forest model to an RDS file
saveRDS(model_rf, "./models/saved_rf_model.rds")

# Load the saved Random Forest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Example new data for prediction
new_data <- data.frame(
  Player_Age = 24,
  Player_Weight = 66.25,
  Player_Height = 175.73,
  Previous_Injuries = factor(1, levels = c(0, 1)),
  Training_Intensity = 5,
  Recovery_Time = 0.46,
  Likelihood_of_Injury = factor(0, levels = c(0, 1))
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print the predictions
print(predictions_loaded_model)

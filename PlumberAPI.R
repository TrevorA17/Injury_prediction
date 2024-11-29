# Plumber API
# Load the saved RandomForest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

#* @apiTitle Injury Prediction Model API

#* @apiDescription Used to predict the likelihood of injury for a player.

#* @param Player_Age Age of the player
#* @param Player_Weight Weight of the player
#* @param Player_Height Height of the player
#* @param Previous_Injuries Number of previous injuries (0 or 1)
#* @param Training_Intensity Intensity of training (scale of 1 to 10)
#* @param Recovery_Time Recovery time in days
#* @param Likelihood_of_Injury The target variable: 0 = not likely, 1 = likely

#* @get /predict_injury
predict_injury <- function(Player_Age, Player_Weight, Player_Height, Previous_Injuries, 
                           Training_Intensity, Recovery_Time, Likelihood_of_Injury) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    Player_Age = as.numeric(Player_Age),
    Player_Weight = as.numeric(Player_Weight),
    Player_Height = as.numeric(Player_Height),
    Previous_Injuries = factor(Previous_Injuries, levels = c(0, 1)),
    Training_Intensity = as.numeric(Training_Intensity),
    Recovery_Time = as.numeric(Recovery_Time),
    Likelihood_of_Injury = factor(Likelihood_of_Injury, levels = c(0, 1))
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(list(prediction = prediction))
}

# Run the Plumber API
# pr <- plumb("path_to_this_script.R")  # If this is in an R file, specify the path
# pr$run(port = 8000)

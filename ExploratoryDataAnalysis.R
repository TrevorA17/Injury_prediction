# Load the dataset with appropriate column classes
InjuryData <- read.csv("data/injury_data.csv", colClasses = c(
  Player_Age = "numeric",
  Player_Weight = "numeric",
  Player_Height = "numeric",
  Previous_Injuries = "integer",
  Training_Intensity = "numeric",
  Recovery_Time = "integer",
  Likelihood_of_Injury = "factor"  # Assuming this is the target variable (categorical)
))

# Display structure to verify data types
str(InjuryData)

# Display first few rows to ensure data is loaded correctly
head(InjuryData)

# View the dataset (optional)
View(InjuryData)

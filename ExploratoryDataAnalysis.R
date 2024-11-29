# Load the dataset
InjuryData <- read.csv("data/injury_data.csv", colClasses = c(
  Player_Age = "numeric",
  Player_Weight = "numeric",
  Player_Height = "numeric",
  Previous_Injuries = "factor",
  Training_Intensity = "numeric",
  Recovery_Time = "numeric",
  Likelihood_of_Injury = "factor"  # Assuming this is the target variable (categorical)
))

# Measures of Frequency
# Frequency of categorical variable
freq_table <- table(InjuryData$Likelihood_of_Injury)
print("Frequency Table:")
print(freq_table)

# Proportions of categories
prop_table <- prop.table(freq_table)
print("Proportions Table:")
print(prop_table)

# Summary of numeric variables (count of non-missing values)
print("Summary of Player_Age and Training_Intensity:")
summary(InjuryData$Player_Age)
summary(InjuryData$Training_Intensity)

# Measures of Central Tendency
# Mean
mean_age <- mean(InjuryData$Player_Age, na.rm = TRUE)
mean_weight <- mean(InjuryData$Player_Weight, na.rm = TRUE)
print("Mean of Player_Age and Player_Weight:")
print(mean_age)
print(mean_weight)

# Median
median_age <- median(InjuryData$Player_Age, na.rm = TRUE)
median_weight <- median(InjuryData$Player_Weight, na.rm = TRUE)
print("Median of Player_Age and Player_Weight:")
print(median_age)
print(median_weight)

# Mode (custom function, as R does not have a built-in mode function)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mode_injuries <- get_mode(InjuryData$Previous_Injuries)
print("Mode of Previous_Injuries:")
print(mode_injuries)

# Measures of Distribution
# Range
range_age <- range(InjuryData$Player_Age, na.rm = TRUE)
range_weight <- range(InjuryData$Player_Weight, na.rm = TRUE)
print("Range of Player_Age and Player_Weight:")
print(range_age)
print(range_weight)

# Variance
var_age <- var(InjuryData$Player_Age, na.rm = TRUE)
var_weight <- var(InjuryData$Player_Weight, na.rm = TRUE)
print("Variance of Player_Age and Player_Weight:")
print(var_age)
print(var_weight)

# Standard Deviation
sd_age <- sd(InjuryData$Player_Age, na.rm = TRUE)
sd_weight <- sd(InjuryData$Player_Weight, na.rm = TRUE)
print("Standard Deviation of Player_Age and Player_Weight:")
print(sd_age)
print(sd_weight)

# Skewness and Kurtosis (requires 'e1071' package)
install.packages("e1071")
library(e1071)

skew_age <- skewness(InjuryData$Player_Age, na.rm = TRUE)
kurt_age <- kurtosis(InjuryData$Player_Age, na.rm = TRUE)
print("Skewness and Kurtosis of Player_Age:")
print(skew_age)
print(kurt_age)

# Measures of Relationship
# Correlation between two numeric variables
cor_age_weight <- cor(InjuryData$Player_Age, InjuryData$Player_Weight, use = "complete.obs")
print("Correlation between Player_Age and Player_Weight:")
print(cor_age_weight)

# Covariance between two numeric variables
cov_age_weight <- cov(InjuryData$Player_Age, InjuryData$Player_Weight, use = "complete.obs")
print("Covariance between Player_Age and Player_Weight:")
print(cov_age_weight)

# Scatterplot to visualize relationship
plot(InjuryData$Player_Age, InjuryData$Player_Weight, 
     xlab = "Player Age", ylab = "Player Weight", main = "Age vs Weight")

# Perform ANOVA: Testing if Player_Age differs by Likelihood_of_Injury
anova_age <- aov(Player_Age ~ Likelihood_of_Injury, data = InjuryData)

# Print summary of ANOVA for Player_Age
print("ANOVA for Player_Age by Likelihood_of_Injury:")
summary(anova_age)

# Perform ANOVA: Testing if Player_Weight differs by Likelihood_of_Injury
anova_weight <- aov(Player_Weight ~ Likelihood_of_Injury, data = InjuryData)

# Print summary of ANOVA for Player_Weight
print("ANOVA for Player_Weight by Likelihood_of_Injury:")
summary(anova_weight)

# Optional: Visualizing the results using boxplots
# Boxplot for Player_Age by Likelihood_of_Injury
boxplot(Player_Age ~ Likelihood_of_Injury, data = InjuryData,
        main = "Boxplot of Player_Age by Likelihood_of_Injury",
        xlab = "Likelihood of Injury", ylab = "Player Age")

# Boxplot for Player_Weight by Likelihood_of_Injury
boxplot(Player_Weight ~ Likelihood_of_Injury, data = InjuryData,
        main = "Boxplot of Player_Weight by Likelihood_of_Injury",
        xlab = "Likelihood of Injury", ylab = "Player Weight")

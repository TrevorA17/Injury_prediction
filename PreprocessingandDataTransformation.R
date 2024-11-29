# Check if there are any missing values in the dataset
# This will return TRUE for any column that has missing values
missing_values <- sapply(InjuryData, function(x) sum(is.na(x)))

# Display the number of missing values per column
print("Missing values per column:")
print(missing_values)

# Check if there are any missing values in the entire dataset
total_missing <- sum(is.na(InjuryData))
cat("Total missing values in the dataset:", total_missing, "\n")

# Check if any column has missing values
any_missing <- any(missing_values > 0)
cat("Does the dataset have missing values?", ifelse(any_missing, "Yes", "No"), "\n")

# Visualize missing data (optional, using the 'VIM' package)
library(VIM)
aggr(InjuryData, col = c("navyblue", "yellow"), numbers = TRUE, sortVars = TRUE, labels = names(InjuryData), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))

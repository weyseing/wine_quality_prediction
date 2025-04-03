# library
install.packages("dplyr")
library(dplyr)

# dataset
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
tail(winequality, n=3)

# summary
summary(winequality)

# outlier (verify)
outlier_vars <- c("residual.sugar", "chlorides", "total.sulfur.dioxide")
par(mfrow = c(1, 3))  # Arrange plots in 1 row, 3 columns
for (var in outlier_vars) {
  boxplot(winequality[[var]], 
          main = paste(var, "Distribution"), 
          ylab = var)
}

# IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(list(lower = lower_bound, upper = upper_bound))
}

# winsorize outliers 
winsorize_iqr <- function(x) {
  bounds <- detect_outliers(x)
  x[x < bounds$lower] <- bounds$lower  # Replace lower outliers with Q1 - 1.5*IQR
  x[x > bounds$upper] <- bounds$upper  # Replace upper outliers with Q3 + 1.5*IQR
  return(x)
}

winequality <- winequality %>%
  mutate(across(all_of(outlier_vars), ~ winsorize(.x)))

tail(winequality, n=3)
summary(winequality)

# library
install.packages("dplyr")
install.packages("e1071")
install.packages("gridExtra")
install.packages("ggplot2")
library(dplyr)
library(e1071)
library(gridExtra)
library(ggplot2)

# dataset
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
tail(winequality, n=3)

# summary
summary(winequality)

# === OUTLIER HANDLING ===

# check outlier
png("outlier_check_before.png", width = 1200, height = 400)
par(mfrow = c(1, 3))
outlier_vars <- c("residual.sugar", "chlorides", "total.sulfur.dioxide")
for (var in outlier_vars) {
  boxplot(winequality[[var]], 
          main = paste(var, "Distribution"), 
          ylab = var)
}
dev.off()

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
  mutate(across(all_of(outlier_vars), ~ winsorize_iqr(.x)))

tail(winequality, n=3)
summary(winequality)

# check outlier
png("outlier_check_after.png", width = 1200, height = 400)
par(mfrow = c(1, 3))
outlier_vars <- c("residual.sugar", "chlorides", "total.sulfur.dioxide")
for (var in outlier_vars) {
  boxplot(winequality[[var]], 
          main = paste(var, "Distribution"), 
          ylab = var)
}
dev.off()

# === LOG TRANSFORMATION ===

# calc skewness
numeric_vars <- names(winequality)[sapply(winequality, is.numeric)]
skewness_report <- data.frame(
  Variable = numeric_vars,
  Skewness = round(sapply(winequality[, numeric_vars], skewness), 2)
)

skewness_report <- skewness_report[order(-abs(skewness_report$Skewness)), ]
print("Skewness Report (Before Transformation):")
print(skewness_report)

high_skew_vars <- skewness_report %>% 
  filter(Skewness > 1) %>% 
  pull(Variable)

# check skewness
original_plots <- lapply(high_skew_vars, function(var) {
  skew_val <- round(skewness(winequality[[var]]), 2)
  ggplot(winequality, aes_string(var)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    geom_vline(aes(xintercept = mean(winequality[[var]])), 
               color = "red", linetype = "dashed") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("Skewness:", skew_val),
             hjust = 1.2, vjust = 1.2, size = 4) +
    ggtitle(paste("Original", var)) +
    theme_minimal()
})

# log transformation
winequality <- winequality %>%
  mutate(across(all_of(outlier_vars), ~ log1p(.x)))

# check skewness
transformed_plots <- lapply(outlier_vars, function(var) {
  skew_val <- round(skewness(winequality[[var]]), 2)
  ggplot(winequality, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
    geom_vline(aes(xintercept = mean(.data[[var]])), 
               color = "darkgreen", linetype = "dashed") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("Skewness:", skew_val),
             hjust = 1.2, vjust = 1.2, size = 4) +
    ggtitle(paste("Transformed", var)) +
    theme_minimal()
})

# display plots
ggsave(
  "log_transformation.png",
  arrangeGrob(grobs = combined_plots, ncol = 3, nrow = 2),
  width = 14, height = 8
)



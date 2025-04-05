# library
# install.packages("dplyr")
# install.packages("e1071")
# install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("VIM")
# install.packages("mice")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("GGally")
# install.packages("plotly")
# install.packages("tidyverse")
library(dplyr)
library(e1071)
library(gridExtra)
library(ggplot2)
library(VIM)
library(mice)
library(caret)
library(randomForest)
library(GGally)
library(plotly)
library(tidyr)
library(dplyr)
library(tidyverse)

# seed
set.seed(123)

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
  x[x < bounds$lower] <- bounds$lower 
  x[x > bounds$upper] <- bounds$upper
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

# === MISSING VALUE HANDLING ====

# check missing
sum(is.na(winequality)) 

# introduce missing value
winequality_missing <- winequality
predictor_vars <- setdiff(names(winequality_missing), "quality") 
for (col in predictor_vars) {
  if (is.numeric(winequality_missing[[col]])) {
    winequality_missing[[col]][sample(1:nrow(winequality_missing), 1000)] <- NA
  }
}

# missing report
missing_report <- data.frame(
  Variable = names(winequality_missing),
  Missing = sapply(winequality_missing, function(x) sum(is.na(x))),
  Percent = round(sapply(winequality_missing, function(x) mean(is.na(x)))*100, 2)
)
print("Missing Value Report:")
print(missing_report)

# mean imputation
method1 <- mice(
  winequality_missing,
  method = "mean",  # Correct method name for mean imputation
  m = 1, 
  maxit = 1,
  printFlag = FALSE
)
winequality_mean <- complete(method1)

# KNN imputation
method2 <- mice(
  winequality_missing,
  method = "pmm", 
  m = 1, 
  maxit = 1,
  printFlag = FALSE
)
winequality_knn <- complete(method2)

# compare imputation
compare_imputation <- function(original, method1, method2, var) {
  # Original distribution
  p1 <- ggplot(data.frame(value = original[[var]]), aes(x = value)) +
    geom_density(color = "blue", fill = "blue", alpha = 0.3) +
    ggtitle("Original Distribution")
  
  # mean imputation
  p2 <- ggplot(data.frame(value = method1[[var]]), aes(x = value)) +
    geom_density(color = "red", fill = "red", alpha = 0.3) +
    ggtitle("Mean Imputation")
  
  # KNN imputation
  p3 <- ggplot(data.frame(value = method2[[var]]), aes(x = value)) +
    geom_density(color = "green", fill = "green", alpha = 0.3) +
    ggtitle("K-NN Imputation")
  
  grid.arrange(p1, p2, p3, ncol = 3)
}
png("missing_handling_1.png", width = 1200, height = 800, res = 150)
compare_imputation(winequality, winequality_mean, winequality_knn, "residual.sugar")
dev.off()

compare_violin <- function(original, method1, method2, var) {
  df <- data.frame(
    Value = c(original[[var]], method1[[var]], method2[[var]]),
    Type = c(rep("Original", nrow(original)),
             rep("Imputed", nrow(method1)*2)),
    Method = c(rep("Original", nrow(original)),
               rep("Mean", nrow(method1)),
               rep("K-NN", nrow(method2)))
  )
  ggplot(df, aes(x = Method, y = Value, fill = Type)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_jitter(aes(color = Type), width = 0.2, alpha = 0.3) +
    ggtitle(paste("Distribution Comparison for", var)) +
    theme_minimal()
}
png("missing_handling_2.png", width = 1200, height = 800, res = 150)
compare_violin(winequality, winequality_mean, winequality_knn, "residual.sugar")
dev.off()

# === TRAIN-TEST SPLIT, MIN-MAX SCALING ===
data <- winequality_knn
split_ratios <- list(
  "70-30" = 0.7,
  "80-20" = 0.8,
  "90-10" = 0.9
)
results <- data.frame()

for (split_name in names(split_ratios)) {
  ratio <- split_ratios[[split_name]]
  
  # split data
  trainIndex <- createDataPartition(data$quality, p = ratio, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  # min-max scaling
  preproc <- preProcess(train_data[, -which(names(train_data) == "quality")], method = "range")
  train_scaled <- predict(preproc, train_data)
  test_scaled <- predict(preproc, test_data)
  
  # modeling
  model <- randomForest(
    x = train_scaled[, -which(names(train_scaled) == "quality")],
    y = train_scaled$quality,
    ntree = 100,
    importance = TRUE
  )
  
  # predict
  predictions <- predict(model, test_scaled[, -which(names(test_scaled) == "quality")])
  
  # metrics
  rmse <- sqrt(mean((predictions - test_data$quality)^2))
  mae <- mean(abs(predictions - test_data$quality))
  r_squared <- cor(predictions, test_data$quality)^2
  
  results <- rbind(results, data.frame(
    Split = split_name,  # Use split_name directly
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R_Squared = round(r_squared, 2),
    Train_Size = nrow(train_data),
    Test_Size = nrow(test_data)
  ))
}

# RMSE
print("Model Performance Across Splits:")
print(results)
png("RMSE.png", width = 1200, height = 800, res = 150)
ggplot(results, aes(x = Split, y = RMSE, fill = Split)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = RMSE), vjust = -0.5) +
  labs(title = "Model Performance Comparison (RMSE)",
       y = "Root Mean Squared Error",
       x = "Train-Test Split Ratio") +
  theme_minimal()
dev.off()

# === VISUALIZE MIN-MAX SCALE ===
comparison_all <- predictor_vars %>%
  map_dfr(~{
    tibble(
      Feature = .x,
      Original = train_data[[.x]],   # From original data
      Scaled = train_scaled[[.x]]    # From scaled data
    ) %>% 
      pivot_longer(cols = c(Original, Scaled), 
                   names_to = "Type", 
                   values_to = "Value")
  })

# layout
n_features <- length(predictor_vars)
n_rows <- ceiling(n_features / 2)
height <- max(8, n_rows * 2.5)

ggplot(comparison_all, aes(x = Value, fill = Type)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  
  # min/max lines
  geom_vline(data = summary_all, 
             aes(xintercept = Min, color = Type), 
             linetype = "dashed") +
  geom_vline(data = summary_all, 
             aes(xintercept = Max, color = Type), 
             linetype = "dashed") +
  
  # Facet grid
  facet_wrap(~ Feature, 
             scales = "free", 
             ncol = 2,
             labeller = label_wrap_gen(width = 25)) +  
  
  # color & style
  scale_fill_manual(values = c("Original" = "lightblue", "Scaled" = "lightgreen")) +
  scale_color_manual(values = c("Original" = "blue", "Scaled" = "green")) +
  labs(title = "Min-Max Scaling Effects Across All Features",
       subtitle = "Blue: Original | Green: Scaled [0,1]",
       x = "Value",
       y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 9, face = "bold"),
    panel.spacing = unit(1.5, "lines"),  # Increased spacing between panels
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 11)
  )

# Save with appropriate dimensions
ggsave("min_max_scaling.png", 
       width = 16, 
       height = height, 
       units = "in",
       dpi = 300)





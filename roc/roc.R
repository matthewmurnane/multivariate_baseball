library(readxl)
library(tidyverse)
library(MASS)

data <- read_xlsx("../master_data.xlsx")
model_inputs <- data[,c(3:14, 16)]

# Function to calculate ROC by varying priors
calculate_roc_by_prior <- function(formula, data, prior_seq = seq(0, 1, by = 0.1)) {
  
  # Initialize results dataframe
  roc_results <- data.frame(
    prior = numeric(),
    TPR = numeric(),
    FPR = numeric(),
    TN = numeric(),
    FP = numeric(),
    FN = numeric(),
    TP = numeric()
  )
  
  # Get the actual values
  actual <- data[[all.vars(formula)[1]]]
  
  # Get the unique levels (assuming binary)
  levels <- unique(actual)
  positive_class <- levels[2]  # Adjust if needed
  
  # Loop over prior probabilities
  for (p in prior_seq) {
    
    # Skip exact 0 and 1 as they cause issues
    if (p == 0 | p == 1) next
    
    # Fit LDA with specified prior
    model <- lda(formula, data = data, prior = c(1-p, p))
    
    # Make predictions
    pred <- predict(model, data)
    
    # Create confusion matrix
    conf_mat <- table(Predicted = pred$class, Actual = actual)
    
    # Extract values (adjust indexing based on your data)
    TN <- conf_mat[1, 1]
    FP <- conf_mat[2, 1]
    FN <- conf_mat[1, 2]
    TP <- conf_mat[2, 2]
    
    # Calculate TPR and FPR
    TPR <- TP / (TP + FN)
    FPR <- FP / (FP + TN)
    
    # Store results
    roc_results <- rbind(roc_results, data.frame(
      prior = p,
      TPR = TPR,
      FPR = FPR,
      TN = TN,
      FP = FP,
      FN = FN,
      TP = TP
    ))
  }
  
  # Sort by FPR for proper ROC curve
  roc_results <- roc_results[order(roc_results$FPR), ]
  
  return(roc_results)
}

# Run the function
#roc_data <- calculate_roc_by_prior(Vote_Flag ~ ., model_inputs)
roc_data <- calculate_roc_by_prior(Vote_Flag ~ ., model_inputs, 
                                   prior_seq = seq(0.01, 0.99, by = 0.01))
# View results
print(roc_data)

# Plot ROC curve
ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  #geom_point(color = "red", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curve (varying prior probabilities)",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)"
  ) +
  theme_minimal() +
  coord_equal()

# Calculate AUC (approximate)
library(zoo)
auc <- sum(diff(roc_data$FPR) * rollmean(roc_data$TPR, 2))
print(paste("Approximate AUC:", round(auc, 3)))

# Calculate distance to top-left corner (0, 1)
roc_data$distance <- sqrt(roc_data$FPR^2 + (1 - roc_data$TPR)^2)

# Find the point with minimum distance
optimal_point <- roc_data[which.min(roc_data$distance), ]

print(optimal_point)
print(paste("Optimal prior probability:", optimal_point$prior))
print(paste("TPR:", round(optimal_point$TPR, 3)))
print(paste("FPR:", round(optimal_point$FPR, 3)))

# Plot ROC curve with optimal point highlighted
ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_point(data = optimal_point, aes(x = FPR, y = TPR), 
             color = "green", size = 4, shape = 17) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = optimal_point$FPR + 0.05, y = optimal_point$TPR,
           label = paste("Prior =", optimal_point$prior), 
           color = "green", size = 4) +
  labs(
    title = "ROC Curve (varying prior probabilities)",
    subtitle = "Green triangle = optimal point",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)"
  ) +
  theme_minimal() +
  coord_equal()

# Fit a model to see the level order
model <- lda(Vote_Flag ~ ., data = model_inputs)

# Check the levels - this shows the order
model$lev

# This tells you:
# prior = c(1-p, p) means:
# First value (1-p = 0.48) goes to model$lev[1]
# Second value (p = 0.52) goes to model$lev[2]


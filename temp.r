# Load required libraries
library(ggplot2)
library(nnet)  # For multinomial logistic regression
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Create a simulated dataset
create_example_data <- function(n_samples = 1000) {
  # Create features
  x1 <- rnorm(n_samples)
  x2 <- rnorm(n_samples)
  
  # Create probabilities for 3 classes
  # Class probabilities depend on the features
  linear_pred1 <- 0.5 + 1.2 * x1 - 0.5 * x2
  linear_pred2 <- -0.8 + 0.2 * x1 + 1.5 * x2
  linear_pred3 <- 0.3 - 0.7 * x1 - 0.3 * x2
  
  # Convert to probabilities using softmax
  denom <- exp(linear_pred1) + exp(linear_pred2) + exp(linear_pred3)
  prob1 <- exp(linear_pred1) / denom
  prob2 <- exp(linear_pred2) / denom
  prob3 <- exp(linear_pred3) / denom
  
  # Generate class based on probabilities
  classes <- apply(cbind(prob1, prob2, prob3), 1, function(p) sample(1:3, 1, prob = p))
  class_factor <- factor(classes, levels = 1:3, labels = c("Class1", "Class2", "Class3"))
  
  # Create dataset
  df <- data.frame(
    x1 = x1,
    x2 = x2,
    class = class_factor,
    true_prob1 = prob1,
    true_prob2 = prob2,
    true_prob3 = prob3
  )
  
  return(df)
}

# Function for multinomial calibration plot
plot_multinomial_calibration <- function(predicted_probs, actual_class, n_bins=10) {
  classes <- colnames(predicted_probs)
  plot_data <- data.frame()
  
  for (class_name in classes) {
    # Extract probabilities for this class
    probs <- predicted_probs[, class_name]
    
    # Create bins
    bins <- cut(probs, breaks=seq(0, 1, length.out=n_bins+1), include.lowest=TRUE)
    
    # Calculate observed proportions
    bin_summary <- aggregate(actual_class == class_name, by=list(bin=bins), FUN=mean)
    bin_centers <- aggregate(probs, by=list(bin=bins), FUN=mean)
    
    # Combine data
    class_data <- data.frame(
      class = class_name,
      bin_center = bin_centers$x,
      observed_prop = bin_summary$x,
      bin_size = as.numeric(table(bins)[bin_centers$bin])
    )
    plot_data <- rbind(plot_data, class_data)
  }
  
  # Create plot
  ggplot(plot_data, aes(x=bin_center, y=observed_prop, color=class)) +
    geom_point(aes(size=bin_size)) +
    geom_line() +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    xlim(0, 1) + ylim(0, 1) +
    labs(x="Predicted Probability", y="Observed Proportion", 
         title="Calibration Plot for Multinomial Classes") +
    theme_minimal() +
    scale_size_continuous(name="Bin Size")
}

# Generate data
df <- create_example_data(2000)

# Split into training and testing sets
set.seed(456)
train_idx <- sample(nrow(df), nrow(df) * 0.7)
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

# Train a multinomial logistic regression model
model <- multinom(class ~ x1 + x2, data = train_data)

# Predict probabilities on test data
pred_probs <- predict(model, newdata = test_data, type = "probs")

# Convert to data frame
pred_probs_df <- as.data.frame(pred_probs)
colnames(pred_probs_df) <- levels(test_data$class)

# Create the calibration plot
plot_multinomial_calibration(pred_probs_df, test_data$class, n_bins=10)

# For comparison, we can also plot with perfect calibration (using true probabilities)
true_probs <- test_data %>% select(true_prob1, true_prob2, true_prob3)
colnames(true_probs) <- levels(test_data$class)
plot_multinomial_calibration(true_probs, test_data$class, n_bins=10)

# Calculate calibration metrics (Expected Calibration Error)
calculate_ece <- function(predicted_probs, actual_class, n_bins=10) {
  classes <- colnames(predicted_probs)
  ece_by_class <- numeric(length(classes))
  
  for (i in 1:length(classes)) {
    class_name <- classes[i]
    probs <- predicted_probs[, class_name]
    
    # Create bins
    bins <- cut(probs, breaks=seq(0, 1, length.out=n_bins+1), include.lowest=TRUE)
    
    # Calculate observed proportions
    bin_summary <- aggregate(actual_class == class_name, by=list(bin=bins), FUN=mean)
    bin_centers <- aggregate(probs, by=list(bin=bins), FUN=mean)
    bin_counts <- table(bins)
    
    # Calculate ECE
    ece <- 0
    for (j in 1:length(bin_summary$x)) {
      bin_name <- as.character(bin_summary$bin[j])
      if (bin_name %in% names(bin_counts)) {
        bin_size <- bin_counts[bin_name]
        ece <- ece + (bin_size / length(probs)) * abs(bin_summary$x[j] - bin_centers$x[j])
      }
    }
    
    ece_by_class[i] <- ece
  }
  
  return(list(
    ece_by_class = setNames(ece_by_class, classes),
    mean_ece = mean(ece_by_class)
  ))
}

# Calculate ECE for the model predictions
ece_model <- calculate_ece(pred_probs_df, test_data$class)
print("Expected Calibration Error by class:")
print(ece_model$ece_by_class)
print(paste("Mean ECE:", round(ece_model$mean_ece, 4)))

# Calculate ECE for the true probabilities (should be close to 0)
ece_true <- calculate_ece(true_probs, test_data$class)
print("Expected Calibration Error for true probabilities:")
print(ece_true$ece_by_class)
print(paste("Mean ECE (true probs):", round(ece_true$mean_ece, 4)))
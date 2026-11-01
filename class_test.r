# Mean Formula
calculate_mean <- function(xi) {
  # NA values first ma nikalne 
  xi_clean <- na.omit(xi)
  
  if (length(xi_clean) == 0) return(NA)
  
  sum_xi <- sum(xi_clean)
  n <- length(xi_clean)
  
  mean_val <- sum_xi / n
  return(mean_val)
}

# SD Formula
calculate_sd <- function(xi, mean_val) {
  xi_clean <- na.omit(xi)
  n <- length(xi_clean)
  
  if (n <= 1 || is.na(mean_val)) return(NA)
  
  sd_val <- sqrt(sum((xi_clean - mean_val)^2) / (n - 1))
  return(sd_val)
}

#  CV Formula (Checks for Negative/Zero Means)
calculate_cv <- function(sd_val, mean_val) {
  if (is.na(mean_val) || is.na(sd_val)) return(NA)
  if (mean_val <= 0) return(NA) # mean 0 or neg nagey NA return garne 
  
  total_cv <- sd_val / mean_val
  return(total_cv)
}

# Convert to percentage
convert_to_percent <- function(total_cv) {
  if (is.na(total_cv)) return(NA)
  
  cv_percent <- total_cv * 100
  return(cv_percent)
}

# Final Major Pipeline Function
Final_CV_per <- function(xi) {
  
  if (length(xi) == 0) return(NA)
  
  mean_val         <- calculate_mean(xi)
  sd_val           <- calculate_sd(xi, mean_val)
  total_cv         <- calculate_cv(sd_val, mean_val)
  final_percentage <- convert_to_percent(total_cv)
  
  return(final_percentage)
}

# eg
xi <- c(12, 18, 25, 30, 34)
Final_CV_per(xi)

# 0 case
empty_data <- c()
zero_mean_data <- c(-10, 10) # mean is 0

print(Final_CV_per(empty_data))     # Returns NA safely
print(Final_CV_per(zero_mean_data)) # Returns NA safely instead of Inf

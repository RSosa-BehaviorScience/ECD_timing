#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

#Function to calculate the error correction metric
calculate_triplet_comparison <- function(df, col_name) {
  data <- df[[col_name]]
  n <- length(data)
  
  #Store the results
  result <- numeric(n - 2)
  
  #Function to calculate the slope using multiple points, handling NA values
  get_slope_with_fallback <- function(data, i_start, i_end) {
    #Extract the subset of non-NA values
    valid_data <- data[i_start:i_end]
    valid_indices <- which(!is.na(valid_data)) + (i_start - 1)
    
    #Check if we have enough valid points to calculate the slope
    if (length(valid_indices) < 2) {
      return(NA)  #Not enough valid data points
    }
    
    #Calculate slope as (last valid value - first valid value) / (number of lags)
    slope <- (data[max(valid_indices)] - data[min(valid_indices)]) / (max(valid_indices) - min(valid_indices))
    
    #Fallback: Move further back if slope is 0, ensuring slope is non-NA before comparison
    while (!is.na(slope) && slope == 0 && min(valid_indices) > 1) {
      valid_indices <- valid_indices - 1
      if (min(valid_indices) == 0) break
      slope <- (data[max(valid_indices)] - data[min(valid_indices)]) / (max(valid_indices) - min(valid_indices))
    }
    
    #If after fallback we still have a zero slope, return NA
    if (is.na(slope) || slope == 0) {
      return(NA)
    }
    
    return(slope)
  }
  
  #Loop through triplets
  for (i in 3:n) {
    #Skip triplets with NA values
    if (any(is.na(data[(i-2):i]))) {
      result[i-2] <- NA
      next
    }
    
    #Calculate first slope (t1 to t2), applying fallback if needed
    slope1 <- tanh(0.1 * get_slope_with_fallback(data, i-2, i-1))
    
    if (is.na(slope1)) {
      result[i-2] <- NA
      next
    }
    
    #Calculate second slope (t2 to t3) 
    slope2 <- tanh(0.1 * (data[i] - data[i-1]) / 1)
    
    #Error correction metric comes from dividing Slope 2 by Slope 1
    result[i-2] <- slope2 / slope1
  }
  
  return(c(rep(NA, 2), result))  #Fill first two rows with NA to match original length
}

#You can continue with script number 4
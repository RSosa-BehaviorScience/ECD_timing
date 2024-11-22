#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('boot')) install.packages('boot'); library(boot)


#Import .csv file named "dataset_DRL_Sosa_unpub" from the Timing Database
#This line will generate a pop-up window for manually selecting the file!
data_wec <- read.csv (file.choose(),fileEncoding="UTF-8-BOM") 

#Determine best model describing within session trends

#Filter out rows where Class is "burst" and Excluded is TRUE
filtered_data <- data_wec[data_wec$Class != "burst" & data_wec$Excluded != TRUE, ]

#Re-index the data to maintain consecutive indexing by Trial. 
#When removing burst IRTs, we eliminated entire rows, 
#breaking the assumption of consecutive trial values. 
#This ensures proper alignment for subsequent analyses.

filtered_data <- filtered_data %>%
  group_by(Subj_idx, Session) %>%
  mutate(Trial = row_number()) %>%
  ungroup()

#Summarize data from subjects across sessions
subject_summary_data <- filtered_data %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    mean_irt = mean(IRT),
    var_coef = sd(IRT) / mean_irt,
    mean_weight = mean(Weight),
    Rewards = sum(IRT >= 15)
  )

#Bootstrapping function for generating confidence intervals
bootstrap_fn <- function(data, indices) {
  sampled_data <- data[indices, ]
  mean_irt <- mean(sampled_data$mean_irt, na.rm = TRUE)
  var_coef <- mean(sampled_data$var_coef, na.rm = TRUE)
  return(c(mean_irt, var_coef))
}

#Set number of bootstrap iterations
n_boot <- 1000

#Calculate averages for session waiting times' mean and spread
grand_data <- subject_summary_data %>%
  group_by(Session) %>%
  summarise(
    mean_irt = mean(mean_irt, na.rm = TRUE),  # Natural average for the session's mean IRT
    var_coef = mean(var_coef, na.rm = TRUE)   # Natural average for the session's variation coefficient
  ) %>%
  ungroup()

#Apply bootstrapping independently for each session
boot_results_list <- list()

for (session in unique(grand_data$Session)) {
  #Filter data for the current session
  session_data <- subject_summary_data %>% filter(Session == session)
  
  #Only apply bootstrapping if there is more than 1 data point
  if (nrow(session_data) > 1) {
    boot_results <- boot(data = session_data, statistic = bootstrap_fn, R = n_boot)
    
    #Store the bootstrapped confidence intervals for mean and spread
    boot_results_list[[as.character(session)]] <- list(
      lower_ci_mean = quantile(boot_results$t[, 1], 0.16, na.rm = TRUE),
      upper_ci_mean = quantile(boot_results$t[, 1], 0.84, na.rm = TRUE),
      lower_ci_var = quantile(boot_results$t[, 2], 0.16, na.rm = TRUE),
      upper_ci_var = quantile(boot_results$t[, 2], 0.84, na.rm = TRUE)
    )
  } else {
    #If there's only one data point, no bootstrapping: return NAs for confidence intervals
    boot_results_list[[as.character(session)]] <- list(
      lower_ci_mean = NA, upper_ci_mean = NA,
      lower_ci_var = NA, upper_ci_var = NA
    )
  }
}

#Add the bootstrapped confidence intervals to the grand_data data frame
#Please note that this takes a while
set.seed(1401)
grand_data <- grand_data %>%
  rowwise() %>%
  mutate(
    lower_ci_mean = boot_results_list[[as.character(Session)]]$lower_ci_mean,
    upper_ci_mean = boot_results_list[[as.character(Session)]]$upper_ci_mean,
    lower_ci_var = boot_results_list[[as.character(Session)]]$lower_ci_var,
    upper_ci_var = boot_results_list[[as.character(Session)]]$upper_ci_var
  ) %>%
  ungroup()

#Print the final grand_data with bootstrapped confidence intervals
print(grand_data)

#Create Figure 1
ss_summary <- ggplot() +
  #Plot subject-level data in the background as traces
  geom_line(data = subject_summary_data, aes(x = Session, y = mean_irt, group = Subj_idx), color = "#f3d3d8", alpha = 0.4, size = 1) +
  geom_line(data = subject_summary_data, aes(x = Session, y = var_coef * 20, group = Subj_idx), color = "#D3E4D7", alpha = 0.4, size = 1) +  #Rescale var_coef for dual axis alignment
  
  #Plot grand mean with bootstrapped confidence intervals and connected dots
  geom_line(data = grand_data, aes(x = Session, y = mean_irt), color = "#560216", size = 1.5) +  #Connect the points with a line
  geom_point(data = grand_data, aes(x = Session, y = mean_irt), color = "#560216", size = 3) +  
  geom_errorbar(data = grand_data, aes(x = Session, ymin = lower_ci_mean, ymax = upper_ci_mean), color = "#560216", width = 0.7, linewidth = 1) +
  
  geom_line(data = grand_data, aes(x = Session, y = var_coef * 20), color = "#2A7C8C", size = 1.5) +  
  geom_point(data = grand_data, aes(x = Session, y = var_coef * 20), color = "#2A7C8C", size = 3) +  
  geom_errorbar(data = grand_data, aes(x = Session, ymin = lower_ci_var * 20, ymax = upper_ci_var * 20), color = "#2A7C8C", width = 0.7, linewidth = 1) +
  
  #Create dual y-axes
  scale_y_continuous(name = "Mean Waiting Time (s)", sec.axis = sec_axis(~./20, name = "Waiting Times Spread (CV)")) +
  
  scale_x_continuous(name = "Session", breaks = seq(5, 40, by = 5)) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    axis.title.y = element_text(color = "#560216", size = 14, face = "bold"),  
    axis.title.y.right = element_text(color = "#2A7C8C", size = 14, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.text = element_text(size = 12),  
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  
  )

ss_summary
#Optimal visualization: 600 x 600

#You can continue with script number 2

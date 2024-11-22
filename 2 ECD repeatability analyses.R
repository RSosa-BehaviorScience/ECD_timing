#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('rptR')) install.packages('rptR'); library(ggplot2)
if (!require('boot')) install.packages('boot'); library(boot)
if (!require("lmerTest")) install.packages("lmerTest"); library(lmerTest)

#Estimating between-subjects (trait) repeatabilities

#Fit a mixed-effects model with mean waiting times as the outcome variable 
#...and Session as a fixed effect to assess the necessity of including Session 
#...as a random effect in repeatability calculations.
meanwt_session_trend <- lmer(mean_irt ~ Session + (1 | Subj_idx), data = subject_summary_data)

#Summarize the model to check the trend
summary(meanwt_session_trend)
#No significant trend in mean waiting times across sessions; 
#adjustment for Session is not required.

#Do the same for waiting-times spread
wtspread_session_trend <- lmer(var_coef ~ Session + (1 | Subj_idx), data = subject_summary_data)

#Summarize the model to check the trend
summary(wtspread_session_trend)
#In this case, adjusting for Session random effects is warranted 
#...when assessing repeatability of timing accuracy.

#Do the same for obtained rewards
rew_session_trend <- lmer(Rewards ~ Session + (1 | Subj_idx), data = subject_summary_data)

#Summarize the model to check the trend
summary(rew_session_trend)
#Adjusting for Session random effects is warranted when assessing 
#...repeatability of obtained rewards.

#Trait repeatabiity for mean waiting times

#Set seed for reproducibility
set.seed(1401)
#Repeatability estimation
bs_r_mean_wt <- rpt(mean_irt ~ (1 | Subj_idx),
                    grname = "Subj_idx",
                    data = subject_summary_data,
                    datatype = "Gaussian",
                    nboot = 1000, npermut = 1000)

#Results
summary(bs_r_mean_wt)
plot(bs_r_mean_wt)
plot(bs_r_mean_wt, type = "permut")

#Extract repeatability estimate, lower CI, and upper CI for plotting
trait_mwt_rpt <- summary(bs_r_mean_wt)$boot  # 95% CI from bootstrapping
trait_mwt_rpt_df <- data.frame(
  Median = trait_mwt_rpt[[1]]["Median"],
  CI_Lower = trait_mwt_rpt[[1]]["2.5%"],
  CI_Upper = trait_mwt_rpt[[1]]["97.5%"]
)

#Trait repeatability for the spread of waiting times

set.seed(1401)
bs_r_spread_wt <- rpt(var_coef ~ Session + (1 | Subj_idx),
                    grname = "Subj_idx",
                    data = subject_summary_data,
                    datatype = "Gaussian",
                    nboot = 1000, npermut = 1000)

summary(bs_r_spread_wt)
plot(bs_r_spread_wt)
plot(bs_r_spread_wt, type = "permut")

#Extract repeatability estimate, lower CI, and upper CI for plotting
trait_wts_rpt <- summary(bs_r_spread_wt)$boot  
trait_wts_rpt_df <- data.frame(
  Median = trait_wts_rpt[[1]]["Median"],
  CI_Lower = trait_wts_rpt[[1]]["2.5%"],
  CI_Upper = trait_wts_rpt[[1]]["97.5%"]
)

#Trait repeatability for obtained rewards

set.seed(1401)
bs_r_rew_wt <- rpt(Rewards ~ Session + (1 | Subj_idx),
                      grname = "Subj_idx",
                      data = subject_summary_data,
                      datatype = "Gaussian",
                      nboot = 1000, npermut = 1000)

summary(bs_r_rew_wt)
plot(bs_r_rew_wt)
plot(bs_r_rew_wt, type = "permut")

#Extract repeatability estimate, lower CI, and upper CI for plotting
trait_rew_rpt <- summary(bs_r_rew_wt)$boot  
trait_rew_rpt_df <- data.frame(
  Median = trait_rew_rpt[[1]]["Median"],
  CI_Lower = trait_rew_rpt[[1]]["2.5%"],
  CI_Upper = trait_rew_rpt[[1]]["97.5%"]
)


#Estimating within-session (state) repeatabilities

#Data preparation

#Add a column to the source data base conveying information about whether a 
#...particular waiting time occurred in the first or second half of 
#...the session
filtered_data$session_half <- ifelse(filtered_data$Timestamp <= 1200, 1, 2)

#Create a new data frame calculating the main variables of interest by
#...session half
summary_halves <- filtered_data %>%
  group_by(Subj_idx, Session, session_half) %>%
  summarise(
    mean_irt = mean(IRT),
    var_coef = sd(IRT) / mean_irt,
    Rewards = sum(IRT >= 15)
  )

#State repeatabilities analysis begins here

#First, generate the function with which we will be able to obtain confidence intervals from 
#...individual reapitability estimates to generate a global one for each index
bootstrap_icc <- function(icc_values, weights) {
  # Resample ICC values with replacement
  resampled_icc <- sample(icc_values, length(icc_values), replace = TRUE)
  # Calculate pooled ICC for the resample
  pooled_icc <- sum(resampled_icc * weights) / sum(weights)
  return(pooled_icc)
}

#State repeatability for mean waiting times

#Create a list to store repeatability values per subject
rpt_values <- list()

#Set seed for reproducibility
set.seed(1401)
#Loop over each subject to estimate their repeatabilities separately
for (subject in unique(summary_halves$Subj_idx)) {
  #Filter data for the specific subject
  subject_data <- subset(summary_halves, Subj_idx == subject)
  
  #Calculate repeatability within sessions
  rpt_result <- rpt(mean_irt ~ (1 | Session), 
                    grname = "Session", 
                    data = subject_data, 
                    datatype = "Gaussian", 
                    nboot = 100, 
                    npermut = 0)
  
  #Store the repeatability value for the session level
  rpt_values[[as.character(subject)]] <- summary(rpt_result)$boot[[1]][3]
}

#Convert list to a numeric vector, removing any NA values
rpt_values <- unlist(rpt_values)
rpt_values <- rpt_values[!is.na(rpt_values)]

#Calculate the overall repeatability estimate as a mean of individual subject estimates
overall_repeatability <- mean(rpt_values)
overall_repeatability

#Calculate weights based on the number of observations per subject-session
weights <- sapply(unique(summary_halves$Subj_idx), function(subj) {
  nrow(subset(summary_halves, Subj_idx == subj))
})

#Bootstrap for confidence interval
set.seed(1401)
n_boot <- 1000
bootstrapped_iccs <- replicate(n_boot, bootstrap_icc(rpt_values, weights))

#Obtain confidence intervals
median <- quantile(bootstrapped_iccs, 0.5)
ci_lower <- quantile(bootstrapped_iccs, 0.025)
ci_upper <- quantile(bootstrapped_iccs, 0.975)

#Store data for plotting
state_mwt_rpt <- data.frame(
  Median = median,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

#within-session repeatability for waiting times spread

rpt_values <- list()

set.seed(1401)
#Loop over each subject to fit the model separately
for (subject in unique(summary_halves$Subj_idx)) {
  
  subject_data <- subset(summary_halves, Subj_idx == subject)
  
  #Calculate repeatability within sessions, adjusting for session trend
  rpt_result <- rpt(var_coef ~ Session + (1 | Session), 
                    grname = "Session", 
                    data = subject_data, 
                    datatype = "Gaussian", 
                    nboot = 100, 
                    npermut = 0)
  
  #Store the repeatability value for the session level
  rpt_values[[as.character(subject)]] <- summary(rpt_result)$boot[[1]][3]
}

#Convert list to a numeric vector, removing any NA values
rpt_values <- unlist(rpt_values)
rpt_values <- rpt_values[!is.na(rpt_values)]

#Calculate the overall repeatability estimate as a mean of individual subject estimates
overall_repeatability <- mean(rpt_values)
overall_repeatability

#Bootstrap for confidence interval
set.seed(1401)
n_boot <- 1000
bootstrapped_iccs <- replicate(n_boot, bootstrap_icc(rpt_values, weights))

median <- quantile(bootstrapped_iccs, 0.5)
ci_lower <- quantile(bootstrapped_iccs, 0.025)
ci_upper <- quantile(bootstrapped_iccs, 0.975)

state_wts_rpt <- data.frame(
  Median = median,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

#within-session repeatability for obtained rewards

#Initialize list to store repeatability values per subject
rpt_values <- list()
set.seed(1401)

for (subject in unique(summary_halves$Subj_idx)) {
  
  subject_data <- subset(summary_halves, Subj_idx == subject)
  
  rpt_result <- rpt(Rewards ~ Session + (1 | Session), 
                    grname = "Session", 
                    data = subject_data, 
                    datatype = "Gaussian", 
                    nboot = 100, 
                    npermut = 0)
  
  rpt_values[[as.character(subject)]] <- summary(rpt_result)$boot[[1]][3]
}

rpt_values <- unlist(rpt_values)
rpt_values <- rpt_values[!is.na(rpt_values)]

overall_repeatability <- mean(rpt_values)
overall_repeatability

set.seed(1401)
n_boot <- 1000
bootstrapped_iccs <- replicate(n_boot, bootstrap_icc(rpt_values, weights))

median <- quantile(bootstrapped_iccs, 0.5)
ci_lower <- quantile(bootstrapped_iccs, 0.025)
ci_upper <- quantile(bootstrapped_iccs, 0.975)

state_rew_rpt <- data.frame(
  Median = median,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

#Preparing data to create Figure 4

colnames(trait_mwt_rpt_df) <- colnames(state_mwt_rpt)
colnames(trait_wts_rpt_df) <- colnames(state_mwt_rpt)
colnames(trait_rew_rpt_df) <- colnames(state_mwt_rpt)

#Combine the six mini data frames into one
trait_mwt_rpt_df$Label <- "Mean Waiting Time"
trait_wts_rpt_df$Label <- "Waiting Times Spread"
trait_rew_rpt_df$Label <- "Obtained Rewards"
state_mwt_rpt$Label <- "Mean Waiting Time"
state_wts_rpt$Label <- "Waiting Times Spread"
state_rew_rpt$Label <- "Obtained Rewards"

#Adding family labels
trait_mwt_rpt_df$Family <- "Trait Repeatability"
trait_wts_rpt_df$Family <- "Trait Repeatability"
trait_rew_rpt_df$Family <- "Trait Repeatability"
state_mwt_rpt$Family <- "State Repeatability"
state_wts_rpt$Family <- "State Repeatability"
state_rew_rpt$Family <- "State Repeatability"

#Combine all data frames
repeatability_data <- bind_rows(trait_mwt_rpt_df, trait_wts_rpt_df, trait_rew_rpt_df, 
                                state_mwt_rpt, state_wts_rpt, state_rew_rpt)

#Ensure "Trait Repeatability" is on the left 
repeatability_data$Family <- factor(repeatability_data$Family, 
                                    levels = c("Trait Repeatability", "State Repeatability"))

#Create Figure 4
ggplot(repeatability_data, aes(x = Label, y = Median, color = Family)) +
  # Diamond-shaped points
  geom_point(size = 8, shape = 18) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.4, size = 1.5) +
  # Custom order for x-axis labels
  scale_x_discrete(limits = c("Mean Waiting Time", "Waiting Times Spread", "Obtained Rewards")) +
  # Adjust facet with fixed scale and re-ordered Family levels
  facet_wrap(~ Family, scales = "fixed") +
  labs(y = "Repeatability Estimate", x = NULL) +
  # Theme adjustments
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank(),               # Remove background grid
    axis.line.x = element_line(color = "black"), # Black x-axis in both panels
    axis.line.y.left = element_line(color = "black"), # Black y-axis in left panel only
    strip.text.x = element_text(face = "bold"),  # Bold facet titles
    legend.position = "none"                     # Remove legend
  ) +
  # Custom colors for each family
  scale_color_manual(values = c("Trait Repeatability" = "#3D5B75", 
                                "State Repeatability" = "#A3C6C4"))
#Optimal visualization: 360x700

#You can continue with script number 3
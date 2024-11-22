#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require("patchwork")) install.packages("patchwork"); library(patchwork)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('BSDA')) install.packages('BSDA'); library(BSDA)

#Rename columns
ecm_rrp <- ecm_rrp %>%
  rename(ecm_trans = ec_metric)
ecm_rrn <- ecm_rrn %>%
  rename(ecm_trans = ec_metric)
ecm_nrnrp <- ecm_nrnrp %>%
  rename(ecm_trans = ec_metric)
ecm_nrnrn <- ecm_nrnrn %>%
  rename(ecm_trans = ec_metric)
ecm_rnr <- ecm_rnr %>%
  rename(ecm_trans = ec_metric)
ecm_nrr <- ecm_nrr %>%
  rename(ecm_trans = ec_metric)

#Get summaries for each sequence class data set, handling NAs

ecm_rrp_summary <- ecm_rrp %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_rrp_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_rrp_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_rrp_summary$ecm_rrp_null[is.nan(ecm_rrp_summary$ecm_rrp_raw)] <- NA
ecm_rrp_summary$ecm_rrp_raw[is.nan(ecm_rrp_summary$ecm_rrp_raw)] <- NA

ecm_rrn_summary <- ecm_rrn %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_rrn_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_rrn_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_rrn_summary$ecm_rrn_null[is.nan(ecm_rrn_summary$ecm_rrn_raw)] <- NA
ecm_rrn_summary$ecm_rrn_raw[is.nan(ecm_rrn_summary$ecm_rrn_raw)] <- NA

ecm_nrnrp_summary <- ecm_nrnrp %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_nrnrp_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_nrnrp_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_nrnrp_summary$ecm_nrnrp_null[is.nan(ecm_nrnrp_summary$ecm_nrnrp_raw)] <- NA
ecm_nrnrp_summary$ecm_nrnrp_raw[is.nan(ecm_nrnrp_summary$ecm_nrnrp_raw)] <- NA

ecm_nrnrn_summary <- ecm_nrnrn %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_nrnrn_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_nrnrn_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_nrnrn_summary$ecm_nrnrn_null[is.nan(ecm_nrnrn_summary$ecm_nrnrn_raw)] <- NA
ecm_nrnrn_summary$ecm_nrnrn_raw[is.nan(ecm_nrnrn_summary$ecm_nrnrn_raw)] <- NA

ecm_rnr_summary <- ecm_rnr %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_rnr_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_rnr_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_rnr_summary$ecm_rnr_null[is.nan(ecm_rnr_summary$ecm_rnr_raw)] <- NA
ecm_rnr_summary$ecm_rnr_raw[is.nan(ecm_rnr_summary$ecm_rnr_raw)] <- NA

ecm_nrr_summary <- ecm_nrr %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    ecm_nrr_raw = mean(ecm_trans, na.rm = TRUE),
    ecm_nrr_null = mean(null_ecm, na.rm = TRUE)
  )

ecm_nrr_summary$ecm_nrr_null[is.nan(ecm_nrr_summary$ecm_nrr_raw)] <- NA
ecm_nrr_summary$ecm_nrr_raw[is.nan(ecm_nrr_summary$ecm_nrr_raw)] <- NA

#Merge error correction metric summarized data with the subject summary data
subject_summary_data <- subject_summary_data %>%
  left_join(ecm_rrp_summary, by = c("Subj_idx", "Session")) %>%
  left_join(ecm_rrn_summary, by = c("Subj_idx", "Session")) %>%
  left_join(ecm_nrnrp_summary, by = c("Subj_idx", "Session")) %>%
  left_join(ecm_nrnrn_summary, by = c("Subj_idx", "Session")) %>%
  left_join(ecm_rnr_summary, by = c("Subj_idx", "Session")) %>%
  left_join(ecm_nrr_summary, by = c("Subj_idx", "Session"))


#Obtain the trait and state components for the observed error-correction metric

#Set target columns
raw_columns <- grep("raw", colnames(subject_summary_data), value = TRUE)

#Loop through each target column and create trait and state components
for (raw_col in raw_columns) {
  #Customize the column name
  suffix <- sub("ecm_", "", sub("_raw", "", raw_col))
  
  #Calculate the between-subject (trait) and within-subject (state) components
  subject_summary_data[[paste0("trait_", suffix)]] <- ave(subject_summary_data[[raw_col]], 
                                                          subject_summary_data$Subj_idx, 
                                                          FUN = function(x) mean(x, na.rm = TRUE))
  
  subject_summary_data[[paste0("state_", suffix)]] <- subject_summary_data[[raw_col]] - 
    subject_summary_data[[paste0("trait_", suffix)]]
}

#Repeat the procedure with the null error-correction metric values

null_columns <- grep("null", colnames(subject_summary_data), value = TRUE)

for (null_col in null_columns) {
  
  suffix <- sub("ecm_", "", sub("_null", "", null_col))
  
  # Calculate the between-subject (trait) and within-subject (state) components
  subject_summary_data[[paste0("null_trait_", suffix)]] <- ave(subject_summary_data[[null_col]], 
                                                          subject_summary_data$Subj_idx, 
                                                          FUN = function(x) mean(x, na.rm = TRUE))
  
  subject_summary_data[[paste0("null_state_", suffix)]] <- subject_summary_data[[null_col]] - 
    subject_summary_data[[paste0("null_trait_", suffix)]]
}


library(lmerTest)


#Rename columns
subject_summary_data <- subject_summary_data %>%
  rename_with(~ paste0("obs_", .), starts_with("trait_")) %>%
  rename_with(~ paste0("obs_", .), starts_with("state_"))

#Hipothesis testing: error-correction metric predicts performance

#First, evaluate the ir trait component of the error correction metric from the 
#...RRP sequence predicts the mean waiting time

#Get slope for observed data
rrp_mwt_trait_obs <- lm(mean_irt ~ obs_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_mwt_trait_obs)
(slope_obs <- summary(rrp_mwt_trait_obs)$coefficients["obs_trait_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_mwt_trait_obs)$coefficients["obs_trait_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrp) - sum(is.na(subject_summary_data$obs_trait_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

#Get slope for null data
rrp_mwt_trait_null <- lm(mean_irt ~ null_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_mwt_trait_null)
(slope_null <- summary(rrp_mwt_trait_null)$coefficients["null_trait_rrp", "Estimate"])
(se_slope_null <- summary(rrp_mwt_trait_null)$coefficients["null_trait_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrp) - sum(is.na(subject_summary_data$null_trait_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

#Compare slopes
tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

#Store data for plotting
rrp_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)

#Repeat this procedure with other sequences, other performance metrics, both
#...with traits and state components of data


rrn_mwt_trait_obs <- lm(mean_irt ~ obs_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_mwt_trait_obs)
(slope_obs <- summary(rrn_mwt_trait_obs)$coefficients["obs_trait_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_mwt_trait_obs)$coefficients["obs_trait_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrn) - sum(is.na(subject_summary_data$obs_trait_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_mwt_trait_null <- lm(mean_irt ~ null_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_mwt_trait_null)
(slope_null <- summary(rrn_mwt_trait_null)$coefficients["null_trait_rrn", "Estimate"])
(se_slope_null <- summary(rrn_mwt_trait_null)$coefficients["null_trait_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrn) - sum(is.na(subject_summary_data$null_trait_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrn_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)


nrnrp_mwt_trait_obs <- lm(mean_irt ~ obs_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_mwt_trait_obs)
(slope_obs <- summary(nrnrp_mwt_trait_obs)$coefficients["obs_trait_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_mwt_trait_obs)$coefficients["obs_trait_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrp) - sum(is.na(subject_summary_data$obs_trait_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_mwt_trait_null <- lm(mean_irt ~ null_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_mwt_trait_null)
(slope_null <- summary(nrnrp_mwt_trait_null)$coefficients["null_trait_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_mwt_trait_null)$coefficients["null_trait_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrp) - sum(is.na(subject_summary_data$null_trait_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrp_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)



nrnrn_mwt_trait_obs <- lm(mean_irt ~ obs_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_mwt_trait_obs)
(slope_obs <- summary(nrnrn_mwt_trait_obs)$coefficients["obs_trait_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_mwt_trait_obs)$coefficients["obs_trait_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrn) - sum(is.na(subject_summary_data$obs_trait_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_mwt_trait_null <- lm(mean_irt ~ null_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_mwt_trait_null)
(slope_null <- summary(nrnrn_mwt_trait_null)$coefficients["null_trait_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_mwt_trait_null)$coefficients["null_trait_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrn) - sum(is.na(subject_summary_data$null_trait_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrn_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)



rnr_mwt_trait_obs <- lm(mean_irt ~ obs_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_mwt_trait_obs)
(slope_obs <- summary(rnr_mwt_trait_obs)$coefficients["obs_trait_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_mwt_trait_obs)$coefficients["obs_trait_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rnr) - sum(is.na(subject_summary_data$obs_trait_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_mwt_trait_null <- lm(mean_irt ~ null_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_mwt_trait_null)
(slope_null <- summary(rnr_mwt_trait_null)$coefficients["null_trait_rnr", "Estimate"])
(se_slope_null <- summary(rnr_mwt_trait_null)$coefficients["null_trait_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rnr) - sum(is.na(subject_summary_data$null_trait_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rnr_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)


nrr_mwt_trait_obs <- lm(mean_irt ~ obs_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_mwt_trait_obs)
(slope_obs <- summary(nrr_mwt_trait_obs)$coefficients["obs_trait_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_mwt_trait_obs)$coefficients["obs_trait_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrr) - sum(is.na(subject_summary_data$obs_trait_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_mwt_trait_null <- lm(mean_irt ~ null_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_mwt_trait_null)
(slope_null <- summary(nrr_mwt_trait_null)$coefficients["null_trait_nrr", "Estimate"])
(se_slope_null <- summary(nrr_mwt_trait_null)$coefficients["null_trait_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrr) - sum(is.na(subject_summary_data$null_trait_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrr_mwt_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_mwt_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)

#Prepare the data for plotting

mwt_trait <- rbind(rrp_mwt_trait_obs_, 
                   rrn_mwt_trait_obs_, 
                   nrnrp_mwt_trait_obs_,
                   nrnrn_mwt_trait_obs_,
                   rnr_mwt_trait_obs_,
                   nrr_mwt_trait_obs_)

mwt_trait <- as.data.frame(mwt_trait)
colnames(mwt_trait) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

mwt_trait$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

mwt_trait$Slope <- as.numeric(mwt_trait$Slope)
mwt_trait$Lower_CI <- as.numeric(mwt_trait$Lower_CI)
mwt_trait$Upper_CI <- as.numeric(mwt_trait$Upper_CI)
mwt_trait$Position <- as.numeric(mwt_trait$Position)


#This is the plot from the top panel of Figure 6

mwt_trait_ <- ggplot(mwt_trait, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Mean Waiting Time ~ Trait ECM", x = NULL, y = "") +  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold")  
  ) +
  scale_color_identity() +
  xlim(-11, 11) +  # Set x-axis limits
  scale_y_continuous(breaks = mwt_trait$Position, labels = rev(mwt_trait$Source)) +  
  annotate("text", x = -4.033856, y = 5.5 + 0.5, label = "*** | & | #", hjust = 0.5, size = 3)  + 
  annotate("text", x = 9.354500, y = 1.5 + 0.5, label = "*** | & | #", hjust = 0.5, size = 3)



#State component of the error-correction metric predicts performance indices


rrp_mwt_state_obs <- lmer(mean_irt ~ obs_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_mwt_state_obs)
(slope_obs <- summary(rrp_mwt_state_obs)$coefficients["obs_state_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_mwt_state_obs)$coefficients["obs_state_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrp) - sum(is.na(subject_summary_data$obs_state_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrp_mwt_state_null <- lmer(mean_irt ~ null_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_mwt_state_null)
(slope_null <- summary(rrp_mwt_state_null)$coefficients["null_state_rrp", "Estimate"])
(se_slope_null <- summary(rrp_mwt_state_null)$coefficients["null_state_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrp) - sum(is.na(subject_summary_data$null_state_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rrp_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)



rrn_mwt_state_obs <- lmer(mean_irt ~ obs_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_mwt_state_obs)
(slope_obs <- summary(rrn_mwt_state_obs)$coefficients["obs_state_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_mwt_state_obs)$coefficients["obs_state_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrn) - sum(is.na(subject_summary_data$obs_state_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_mwt_state_null <- lmer(mean_irt ~ null_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_mwt_state_null)
(slope_null <- summary(rrn_mwt_state_null)$coefficients["null_state_rrn", "Estimate"])
(se_slope_null <- summary(rrn_mwt_state_null)$coefficients["null_state_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrn) - sum(is.na(subject_summary_data$null_state_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrn_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)



nrnrp_mwt_state_obs <- lmer(mean_irt ~ obs_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_mwt_state_obs)
(slope_obs <- summary(nrnrp_mwt_state_obs)$coefficients["obs_state_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_mwt_state_obs)$coefficients["obs_state_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrp) - sum(is.na(subject_summary_data$obs_state_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_mwt_state_null <- lmer(mean_irt ~ null_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_mwt_state_null)
(slope_null <- summary(nrnrp_mwt_state_null)$coefficients["null_state_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_mwt_state_null)$coefficients["null_state_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrp) - sum(is.na(subject_summary_data$null_state_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrp_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)




nrnrn_mwt_state_obs <- lmer(mean_irt ~ obs_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_mwt_state_obs)
(slope_obs <- summary(nrnrn_mwt_state_obs)$coefficients["obs_state_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_mwt_state_obs)$coefficients["obs_state_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrn) - sum(is.na(subject_summary_data$obs_state_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_mwt_state_null <- lmer(mean_irt ~ null_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_mwt_state_null)
(slope_null <- summary(nrnrn_mwt_state_null)$coefficients["null_state_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_mwt_state_null)$coefficients["null_state_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrn) - sum(is.na(subject_summary_data$null_state_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrn_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)




rnr_mwt_state_obs <- lmer(mean_irt ~ obs_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_mwt_state_obs)
(slope_obs <- summary(rnr_mwt_state_obs)$coefficients["obs_state_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_mwt_state_obs)$coefficients["obs_state_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rnr) - sum(is.na(subject_summary_data$obs_state_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_mwt_state_null <- lmer(mean_irt ~ null_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_mwt_state_null)
(slope_null <- summary(rnr_mwt_state_null)$coefficients["null_state_rnr", "Estimate"])
(se_slope_null <- summary(rnr_mwt_state_null)$coefficients["null_state_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rnr) - sum(is.na(subject_summary_data$null_state_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rnr_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)



nrr_mwt_state_obs <- lmer(mean_irt ~ obs_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_mwt_state_obs)
(slope_obs <- summary(nrr_mwt_state_obs)$coefficients["obs_state_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_mwt_state_obs)$coefficients["obs_state_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrr) - sum(is.na(subject_summary_data$obs_state_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_mwt_state_null <- lmer(mean_irt ~ null_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_mwt_state_null)
(slope_null <- summary(nrr_mwt_state_null)$coefficients["null_state_nrr", "Estimate"])
(se_slope_null <- summary(nrr_mwt_state_null)$coefficients["null_state_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrr) - sum(is.na(subject_summary_data$null_state_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrr_mwt_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_mwt_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)


mwt_state <- rbind(rrp_mwt_state_obs_, 
                   rrn_mwt_state_obs_, 
                   nrnrp_mwt_state_obs_,
                   nrnrn_mwt_state_obs_,
                   rnr_mwt_state_obs_,
                   nrr_mwt_state_obs_)

mwt_state <- as.data.frame(mwt_state)
colnames(mwt_state) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

mwt_state$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

mwt_state$Slope <- as.numeric(mwt_state$Slope)
mwt_state$Lower_CI <- as.numeric(mwt_state$Lower_CI)
mwt_state$Upper_CI <- as.numeric(mwt_state$Upper_CI)
mwt_state$Position <- as.numeric(mwt_state$Position)


library(ggplot2)


mwt_state_ <- ggplot(mwt_state, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Mean Waiting Time ~ State ECM", x = NULL, y = "") +  # Removes x-axis title
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold")  # Bold y-axis tick labels
  ) +
  scale_color_identity() +
  xlim(-11, 11) +  # Set x-axis limits
  scale_y_continuous(breaks = mwt_state$Position, labels = rev(mwt_state$Source)) # Use custom y-axis labels
  




#Waiting times spread analyses

#Trait component



rrp_wts_trait_obs <- lm(var_coef ~ obs_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_wts_trait_obs)
(slope_obs <- summary(rrp_wts_trait_obs)$coefficients["obs_trait_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_wts_trait_obs)$coefficients["obs_trait_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrp) - sum(is.na(subject_summary_data$obs_trait_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrp_wts_trait_null <- lm(var_coef ~ null_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_wts_trait_null)
(slope_null <- summary(rrp_wts_trait_null)$coefficients["null_trait_rrp", "Estimate"])
(se_slope_null <- summary(rrp_wts_trait_null)$coefficients["null_trait_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrp) - sum(is.na(subject_summary_data$null_trait_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrp_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)



rrn_wts_trait_obs <- lm(var_coef ~ obs_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_wts_trait_obs)
(slope_obs <- summary(rrn_wts_trait_obs)$coefficients["obs_trait_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_wts_trait_obs)$coefficients["obs_trait_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrn) - sum(is.na(subject_summary_data$obs_trait_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_wts_trait_null <- lm(var_coef ~ null_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_wts_trait_null)
(slope_null <- summary(rrn_wts_trait_null)$coefficients["null_trait_rrn", "Estimate"])
(se_slope_null <- summary(rrn_wts_trait_null)$coefficients["null_trait_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrn) - sum(is.na(subject_summary_data$null_trait_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rrn_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)



nrnrp_wts_trait_obs <- lm(var_coef ~ obs_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_wts_trait_obs)
(slope_obs <- summary(nrnrp_wts_trait_obs)$coefficients["obs_trait_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_wts_trait_obs)$coefficients["obs_trait_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrp) - sum(is.na(subject_summary_data$obs_trait_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_wts_trait_null <- lm(var_coef ~ null_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_wts_trait_null)
(slope_null <- summary(nrnrp_wts_trait_null)$coefficients["null_trait_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_wts_trait_null)$coefficients["null_trait_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrp) - sum(is.na(subject_summary_data$null_trait_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrp_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)




nrnrn_wts_trait_obs <- lm(var_coef ~ obs_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_wts_trait_obs)
(slope_obs <- summary(nrnrn_wts_trait_obs)$coefficients["obs_trait_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_wts_trait_obs)$coefficients["obs_trait_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrn) - sum(is.na(subject_summary_data$obs_trait_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_wts_trait_null <- lm(var_coef ~ null_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_wts_trait_null)
(slope_null <- summary(nrnrn_wts_trait_null)$coefficients["null_trait_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_wts_trait_null)$coefficients["null_trait_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrn) - sum(is.na(subject_summary_data$null_trait_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrn_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)




rnr_wts_trait_obs <- lm(var_coef ~ obs_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_wts_trait_obs)
(slope_obs <- summary(rnr_wts_trait_obs)$coefficients["obs_trait_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_wts_trait_obs)$coefficients["obs_trait_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rnr) - sum(is.na(subject_summary_data$obs_trait_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_wts_trait_null <- lm(var_coef ~ null_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_wts_trait_null)
(slope_null <- summary(rnr_wts_trait_null)$coefficients["null_trait_rnr", "Estimate"])
(se_slope_null <- summary(rnr_wts_trait_null)$coefficients["null_trait_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rnr) - sum(is.na(subject_summary_data$null_trait_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rnr_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)



nrr_wts_trait_obs <- lm(var_coef ~ obs_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_wts_trait_obs)
(slope_obs <- summary(nrr_wts_trait_obs)$coefficients["obs_trait_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_wts_trait_obs)$coefficients["obs_trait_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrr) - sum(is.na(subject_summary_data$obs_trait_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_wts_trait_null <- lm(var_coef ~ null_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_wts_trait_null)
(slope_null <- summary(nrr_wts_trait_null)$coefficients["null_trait_nrr", "Estimate"])
(se_slope_null <- summary(nrr_wts_trait_null)$coefficients["null_trait_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrr) - sum(is.na(subject_summary_data$null_trait_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrr_wts_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_wts_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)

wts_trait <- rbind(rrp_wts_trait_obs_, 
                   rrn_wts_trait_obs_, 
                   nrnrp_wts_trait_obs_,
                   nrnrn_wts_trait_obs_,
                   rnr_wts_trait_obs_,
                   nrr_wts_trait_obs_)

wts_trait <- as.data.frame(wts_trait)
colnames(wts_trait) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

wts_trait$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

wts_trait$Slope <- as.numeric(wts_trait$Slope)
wts_trait$Lower_CI <- as.numeric(wts_trait$Lower_CI)
wts_trait$Upper_CI <- as.numeric(wts_trait$Upper_CI)
wts_trait$Position <- as.numeric(wts_trait$Position)


library(ggplot2)


wts_trait_ <- ggplot(wts_trait, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Waiting Times Spread ~ Trait ECM", x = NULL, y = "") +  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold")  
  ) +
  scale_color_identity() +
  xlim(-0.25, 0.25) +  
  scale_y_continuous(breaks = wts_trait$Position, labels = rev(wts_trait$Source)) +  
  annotate("text", x = -0.18342783, y = 1.5 + 0.5, label = "*** | & | #", hjust = 0.5, size = 3)  
  



#State component


rrp_wts_state_obs <- lmer(var_coef ~ obs_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_wts_state_obs)
(slope_obs <- summary(rrp_wts_state_obs)$coefficients["obs_state_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_wts_state_obs)$coefficients["obs_state_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrp) - sum(is.na(subject_summary_data$obs_state_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrp_wts_state_null <- lmer(var_coef ~ null_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_wts_state_null)
(slope_null <- summary(rrp_wts_state_null)$coefficients["null_state_rrp", "Estimate"])
(se_slope_null <- summary(rrp_wts_state_null)$coefficients["null_state_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrp) - sum(is.na(subject_summary_data$null_state_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrp_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)



rrn_wts_state_obs <- lmer(var_coef ~ obs_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_wts_state_obs)
(slope_obs <- summary(rrn_wts_state_obs)$coefficients["obs_state_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_wts_state_obs)$coefficients["obs_state_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrn) - sum(is.na(subject_summary_data$obs_state_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_wts_state_null <- lmer(var_coef ~ null_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_wts_state_null)
(slope_null <- summary(rrn_wts_state_null)$coefficients["null_state_rrn", "Estimate"])
(se_slope_null <- summary(rrn_wts_state_null)$coefficients["null_state_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrn) - sum(is.na(subject_summary_data$null_state_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rrn_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)



nrnrp_wts_state_obs <- lmer(var_coef ~ obs_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_wts_state_obs)
(slope_obs <- summary(nrnrp_wts_state_obs)$coefficients["obs_state_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_wts_state_obs)$coefficients["obs_state_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrp) - sum(is.na(subject_summary_data$obs_state_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_wts_state_null <- lmer(var_coef ~ null_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_wts_state_null)
(slope_null <- summary(nrnrp_wts_state_null)$coefficients["null_state_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_wts_state_null)$coefficients["null_state_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrp) - sum(is.na(subject_summary_data$null_state_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrp_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)




nrnrn_wts_state_obs <- lmer(var_coef ~ obs_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_wts_state_obs)
(slope_obs <- summary(nrnrn_wts_state_obs)$coefficients["obs_state_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_wts_state_obs)$coefficients["obs_state_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrn) - sum(is.na(subject_summary_data$obs_state_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_wts_state_null <- lmer(var_coef ~ null_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_wts_state_null)
(slope_null <- summary(nrnrn_wts_state_null)$coefficients["null_state_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_wts_state_null)$coefficients["null_state_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrn) - sum(is.na(subject_summary_data$null_state_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrn_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)




rnr_wts_state_obs <- lmer(var_coef ~ obs_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_wts_state_obs)
(slope_obs <- summary(rnr_wts_state_obs)$coefficients["obs_state_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_wts_state_obs)$coefficients["obs_state_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rnr) - sum(is.na(subject_summary_data$obs_state_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_wts_state_null <- lmer(var_coef ~ null_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_wts_state_null)
(slope_null <- summary(rnr_wts_state_null)$coefficients["null_state_rnr", "Estimate"])
(se_slope_null <- summary(rnr_wts_state_null)$coefficients["null_state_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rnr) - sum(is.na(subject_summary_data$null_state_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rnr_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)



nrr_wts_state_obs <- lmer(var_coef ~ obs_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_wts_state_obs)
(slope_obs <- summary(nrr_wts_state_obs)$coefficients["obs_state_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_wts_state_obs)$coefficients["obs_state_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrr) - sum(is.na(subject_summary_data$obs_state_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_wts_state_null <- lmer(var_coef ~ null_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_wts_state_null)
(slope_null <- summary(nrr_wts_state_null)$coefficients["null_state_nrr", "Estimate"])
(se_slope_null <- summary(nrr_wts_state_null)$coefficients["null_state_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrr) - sum(is.na(subject_summary_data$null_state_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrr_wts_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_wts_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)



wts_state <- rbind(rrp_wts_state_obs_, 
                   rrn_wts_state_obs_, 
                   nrnrp_wts_state_obs_,
                   nrnrn_wts_state_obs_,
                   rnr_wts_state_obs_,
                   nrr_wts_state_obs_)

wts_state <- as.data.frame(wts_state)
colnames(wts_state) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

wts_state$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

wts_state$Slope <- as.numeric(wts_state$Slope)
wts_state$Lower_CI <- as.numeric(wts_state$Lower_CI)
wts_state$Upper_CI <- as.numeric(wts_state$Upper_CI)
wts_state$Position <- as.numeric(wts_state$Position)



wts_state_ <- ggplot(wts_state, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Waiting Times Spread ~ State ECM", x = NULL, y = "") +  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold") 
  ) +
  scale_color_identity() +
  xlim(-0.25, 0.25) +  # Set x-axis limits
  scale_y_continuous(breaks = wts_state$Position, labels = rev(wts_state$Source))   
    




#Rewards

#Trait componnet



rrp_rew_trait_obs <- lm(Rewards ~ obs_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_rew_trait_obs)
(slope_obs <- summary(rrp_rew_trait_obs)$coefficients["obs_trait_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_rew_trait_obs)$coefficients["obs_trait_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrp) - sum(is.na(subject_summary_data$obs_trait_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrp_rew_trait_null <- lm(Rewards ~ null_trait_rrp, data = subject_summary_data, na.action = na.exclude)
summary(rrp_rew_trait_null)
(slope_null <- summary(rrp_rew_trait_null)$coefficients["null_trait_rrp", "Estimate"])
(se_slope_null <- summary(rrp_rew_trait_null)$coefficients["null_trait_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrp) - sum(is.na(subject_summary_data$null_trait_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rrp_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)



rrn_rew_trait_obs <- lm(Rewards ~ obs_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_rew_trait_obs)
(slope_obs <- summary(rrn_rew_trait_obs)$coefficients["obs_trait_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_rew_trait_obs)$coefficients["obs_trait_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rrn) - sum(is.na(subject_summary_data$obs_trait_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_rew_trait_null <- lm(Rewards ~ null_trait_rrn, data = subject_summary_data, na.action = na.exclude)
summary(rrn_rew_trait_null)
(slope_null <- summary(rrn_rew_trait_null)$coefficients["null_trait_rrn", "Estimate"])
(se_slope_null <- summary(rrn_rew_trait_null)$coefficients["null_trait_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rrn) - sum(is.na(subject_summary_data$null_trait_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrn_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)



nrnrp_rew_trait_obs <- lm(Rewards ~ obs_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_rew_trait_obs)
(slope_obs <- summary(nrnrp_rew_trait_obs)$coefficients["obs_trait_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_rew_trait_obs)$coefficients["obs_trait_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrp) - sum(is.na(subject_summary_data$obs_trait_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_rew_trait_null <- lm(Rewards ~ null_trait_nrnrp, data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_rew_trait_null)
(slope_null <- summary(nrnrp_rew_trait_null)$coefficients["null_trait_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_rew_trait_null)$coefficients["null_trait_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrp) - sum(is.na(subject_summary_data$null_trait_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrp_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)




nrnrn_rew_trait_obs <- lm(Rewards ~ obs_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_rew_trait_obs)
(slope_obs <- summary(nrnrn_rew_trait_obs)$coefficients["obs_trait_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_rew_trait_obs)$coefficients["obs_trait_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrnrn) - sum(is.na(subject_summary_data$obs_trait_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_rew_trait_null <- lm(Rewards ~ null_trait_nrnrn, data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_rew_trait_null)
(slope_null <- summary(nrnrn_rew_trait_null)$coefficients["null_trait_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_rew_trait_null)$coefficients["null_trait_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrnrn) - sum(is.na(subject_summary_data$null_trait_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrn_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)




rnr_rew_trait_obs <- lm(Rewards ~ obs_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_rew_trait_obs)
(slope_obs <- summary(rnr_rew_trait_obs)$coefficients["obs_trait_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_rew_trait_obs)$coefficients["obs_trait_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_rnr) - sum(is.na(subject_summary_data$obs_trait_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_rew_trait_null <- lm(Rewards ~ null_trait_rnr, data = subject_summary_data, na.action = na.exclude)
summary(rnr_rew_trait_null)
(slope_null <- summary(rnr_rew_trait_null)$coefficients["null_trait_rnr", "Estimate"])
(se_slope_null <- summary(rnr_rew_trait_null)$coefficients["null_trait_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_rnr) - sum(is.na(subject_summary_data$null_trait_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rnr_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)



nrr_rew_trait_obs <- lm(Rewards ~ obs_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_rew_trait_obs)
(slope_obs <- summary(nrr_rew_trait_obs)$coefficients["obs_trait_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_rew_trait_obs)$coefficients["obs_trait_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_trait_nrr) - sum(is.na(subject_summary_data$obs_trait_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_rew_trait_null <- lm(Rewards ~ null_trait_nrr, data = subject_summary_data, na.action = na.exclude)
summary(nrr_rew_trait_null)
(slope_null <- summary(nrr_rew_trait_null)$coefficients["null_trait_nrr", "Estimate"])
(se_slope_null <- summary(nrr_rew_trait_null)$coefficients["null_trait_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_trait_nrr) - sum(is.na(subject_summary_data$null_trait_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrr_rew_trait_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_rew_trait_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)

rew_trait <- rbind(rrp_rew_trait_obs_, 
                   rrn_rew_trait_obs_, 
                   nrnrp_rew_trait_obs_,
                   nrnrn_rew_trait_obs_,
                   rnr_rew_trait_obs_,
                   nrr_rew_trait_obs_)

rew_trait <- as.data.frame(rew_trait)
colnames(rew_trait) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

rew_trait$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

rew_trait$Slope <- as.numeric(rew_trait$Slope)
rew_trait$Lower_CI <- as.numeric(rew_trait$Lower_CI)
rew_trait$Upper_CI <- as.numeric(rew_trait$Upper_CI)
rew_trait$Position <- as.numeric(rew_trait$Position)



rew_trait_ <- ggplot(rew_trait, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Obtained Rewards ~ Trait ECM", x = NULL, y = "") +  
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold")  
  ) +
  scale_color_identity() +
  xlim(-55, 55) +  
  scale_y_continuous(breaks = rew_trait$Position, labels = rev(rew_trait$Source)) +  
  annotate("text", x = -22.64790, y = 5.5 + 0.5, label = "*** | & | #", hjust = 0.5, size = 3)  + 
  annotate("text", x = 49.36400, y = 1.5 + 0.5, label = "** | & | #", hjust = 0.5, size = 3)



#State component



rrp_rew_state_obs <- lmer(Rewards ~ obs_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_rew_state_obs)
(slope_obs <- summary(rrp_rew_state_obs)$coefficients["obs_state_rrp", "Estimate"])
(se_slope_obs <- summary(rrp_rew_state_obs)$coefficients["obs_state_rrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrp) - sum(is.na(subject_summary_data$obs_state_rrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrp_rew_state_null <- lmer(Rewards ~ null_state_rrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrp_rew_state_null)
(slope_null <- summary(rrp_rew_state_null)$coefficients["null_state_rrp", "Estimate"])
(se_slope_null <- summary(rrp_rew_state_null)$coefficients["null_state_rrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrp) - sum(is.na(subject_summary_data$null_state_rrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

rrp_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)
rrp_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#b43052", 1)



rrn_rew_state_obs <- lmer(Rewards ~ obs_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_rew_state_obs)
(slope_obs <- summary(rrn_rew_state_obs)$coefficients["obs_state_rrn", "Estimate"])
(se_slope_obs <- summary(rrn_rew_state_obs)$coefficients["obs_state_rrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rrn) - sum(is.na(subject_summary_data$obs_state_rrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rrn_rew_state_null <- lmer(Rewards ~ null_state_rrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rrn_rew_state_null)
(slope_null <- summary(rrn_rew_state_null)$coefficients["null_state_rrn", "Estimate"])
(se_slope_null <- summary(rrn_rew_state_null)$coefficients["null_state_rrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rrn) - sum(is.na(subject_summary_data$null_state_rrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rrn_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)
rrn_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#eaac8b", 2)



nrnrp_rew_state_obs <- lmer(Rewards ~ obs_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_rew_state_obs)
(slope_obs <- summary(nrnrp_rew_state_obs)$coefficients["obs_state_nrnrp", "Estimate"])
(se_slope_obs <- summary(nrnrp_rew_state_obs)$coefficients["obs_state_nrnrp", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrp) - sum(is.na(subject_summary_data$obs_state_nrnrp)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrp_rew_state_null <- lmer(Rewards ~ null_state_nrnrp + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrp_rew_state_null)
(slope_null <- summary(nrnrp_rew_state_null)$coefficients["null_state_nrnrp", "Estimate"])
(se_slope_null <- summary(nrnrp_rew_state_null)$coefficients["null_state_nrnrp", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrp) - sum(is.na(subject_summary_data$null_state_nrnrp)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrnrp_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)
nrnrp_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#BD9745", 3)




nrnrn_rew_state_obs <- lmer(Rewards ~ obs_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_rew_state_obs)
(slope_obs <- summary(nrnrn_rew_state_obs)$coefficients["obs_state_nrnrn", "Estimate"])
(se_slope_obs <- summary(nrnrn_rew_state_obs)$coefficients["obs_state_nrnrn", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrnrn) - sum(is.na(subject_summary_data$obs_state_nrnrn)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrnrn_rew_state_null <- lmer(Rewards ~ null_state_nrnrn + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrnrn_rew_state_null)
(slope_null <- summary(nrnrn_rew_state_null)$coefficients["null_state_nrnrn", "Estimate"])
(se_slope_null <- summary(nrnrn_rew_state_null)$coefficients["null_state_nrnrn", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrnrn) - sum(is.na(subject_summary_data$null_state_nrnrn)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

nrnrn_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)
nrnrn_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#6d597a", 4)




rnr_rew_state_obs <- lmer(Rewards ~ obs_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_rew_state_obs)
(slope_obs <- summary(rnr_rew_state_obs)$coefficients["obs_state_rnr", "Estimate"])
(se_slope_obs <- summary(rnr_rew_state_obs)$coefficients["obs_state_rnr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_rnr) - sum(is.na(subject_summary_data$obs_state_rnr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

rnr_rew_state_null <- lmer(Rewards ~ null_state_rnr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(rnr_rew_state_null)
(slope_null <- summary(rnr_rew_state_null)$coefficients["null_state_rnr", "Estimate"])
(se_slope_null <- summary(rnr_rew_state_null)$coefficients["null_state_rnr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_rnr) - sum(is.na(subject_summary_data$null_state_rnr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "less")

rnr_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)
rnr_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#4b824e", 5)



nrr_rew_state_obs <- lmer(Rewards ~ obs_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_rew_state_obs)
(slope_obs <- summary(nrr_rew_state_obs)$coefficients["obs_state_nrr", "Estimate"])
(se_slope_obs <- summary(nrr_rew_state_obs)$coefficients["obs_state_nrr", "Std. Error"])
(n_obs <- length(subject_summary_data$obs_state_nrr) - sum(is.na(subject_summary_data$obs_state_nrr)))
(sd_slope_obs <- se_slope_obs * sqrt(n_obs))

nrr_rew_state_null <- lmer(Rewards ~ null_state_nrr + (1 | Subj_idx), data = subject_summary_data, na.action = na.exclude)
summary(nrr_rew_state_null)
(slope_null <- summary(nrr_rew_state_null)$coefficients["null_state_nrr", "Estimate"])
(se_slope_null <- summary(nrr_rew_state_null)$coefficients["null_state_nrr", "Std. Error"])
(n_null <- length(subject_summary_data$null_state_nrr) - sum(is.na(subject_summary_data$null_state_nrr)))
(sd_slope_null <- se_slope_null * sqrt(n_null))

tsum.test(mean.x = slope_obs, s.x = sd_slope_obs, n.x = n_obs,
          mean.y = slope_null, s.y = sd_slope_null, n.y = n_null, alternative = "greater")

nrr_rew_state_obs_ <- c("observed", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)
nrr_rew_state_null_ <- c("null", slope_obs, slope_obs - (se_slope_obs*1.96), slope_obs + (se_slope_obs*1.96), "#8c510a", 6)

rew_state <- rbind(rrp_rew_state_obs_, 
                   rrn_rew_state_obs_, 
                   nrnrp_rew_state_obs_,
                   nrnrn_rew_state_obs_,
                   rnr_rew_state_obs_,
                   nrr_rew_state_obs_)

rew_state <- as.data.frame(rew_state)
colnames(rew_state) <- c("Source", "Slope", "Lower_CI", "Upper_CI", "Color", "Position")

rew_state$Source <- c("RRP", "RRN", "NRNRP", "NRNRN", "RNR", "NRR")

rew_state$Slope <- as.numeric(rew_state$Slope)
rew_state$Lower_CI <- as.numeric(rew_state$Lower_CI)
rew_state$Upper_CI <- as.numeric(rew_state$Upper_CI)
rew_state$Position <- as.numeric(rew_state$Position)


rew_state_ <- ggplot(rew_state, aes(x = Slope, y = rev(Position), color = Color)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.6, linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Obtained Rewards ~ State ECM", x = NULL, y = "") +  # Removes x-axis title
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(face = "bold")  # Bold y-axis tick labels
  ) +
  scale_color_identity() +
  xlim(-55, 55) +  # Set x-axis limits
  scale_y_continuous(breaks = rew_state$Position, labels = rev(rew_state$Source)) 




perf_pred <- mwt_trait_ / mwt_state_ / wts_trait_ / wts_state_ / rew_trait_ / rew_state_
perf_pred
#Ideal dimensions for visualization 750x1000

#Done!!

#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('boot')) install.packages('boot'); library(boot)

#Set progress bar
n_iterations <- 1000
pb <- txtProgressBar(min = 0, max = n_iterations, style = 3, width = 100, char = "=")

#Create a storage structure to store null results
null_results_list <- list()

#Bootstrap shuffled data to obtain null error-correction metric for each
#...sequence

#WARNING: this iterative routine takes several minutes long

set.seed(1401)
for (i in 1:n_iterations) {
  setTxtProgressBar(pb, i)
  
  #Shuffle the IRT column within a Subj_idx-Session unit
  filtered_data <- filtered_data %>%
    group_by(Subj_idx, Session) %>%
    mutate(IRT_shuffled = sample(IRT)) %>%
    ungroup()
  
  filtered_data <- filtered_data %>%
    group_by(Subj_idx, Session) %>%
    mutate(ec_metric = calculate_triplet_comparison(cur_data(), 'IRT_shuffled')) %>%
    ungroup()
  
  subject_session_null_ecm <- filtered_data %>%
    group_by(Subj_idx, Session) %>%
    summarise(
      #Calculate the bounds for each sequence class (rrp, rrn, etc.) and remove outliers
      
      #RRP calculation (removing the mean-based outlier check)
      mean_rrp = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 & 
                                  lag(IRT_shuffled, n = 2, default = NA) >= 15 & 
                                  lag(IRT_shuffled, n = 1, default = NA) > lag(IRT_shuffled, n = 2, default = NA)], na.rm = TRUE),
      lower_bound_rrp = mean_rrp - 5,
      upper_bound_rrp = mean_rrp + 5,
      mean_ecm_rrp = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 &
                                      lag(IRT_shuffled, n = 2, default = NA) >= 15 &
                                      lag(IRT_shuffled, n = 1, default = NA) > lag(IRT_shuffled, n = 2, default = NA) &
                                      ec_metric >= lower_bound_rrp & ec_metric <= upper_bound_rrp], na.rm = TRUE),
      
      #RRN calculation (removing the mean-based outlier check)
      mean_rrn = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 & 
                                  lag(IRT_shuffled, n = 2, default = NA) >= 15 & 
                                  lag(IRT_shuffled, n = 1, default = NA) < lag(IRT_shuffled, n = 2, default = NA)], na.rm = TRUE),
      lower_bound_rrn = mean_rrn - 5,
      upper_bound_rrn = mean_rrn + 5,
      mean_ecm_rrn = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 &
                                      lag(IRT_shuffled, n = 2, default = NA) >= 15 &
                                      lag(IRT_shuffled, n = 1, default = NA) < lag(IRT_shuffled, n = 2, default = NA) &
                                      ec_metric >= lower_bound_rrn & ec_metric <= upper_bound_rrn], na.rm = TRUE),
      
      #NRNRP calculation (removing the mean-based outlier check)
      mean_nrnrp = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 &
                                    lag(IRT_shuffled, n = 2, default = NA) < 15 & 
                                    lag(IRT_shuffled, n = 1, default = NA) > lag(IRT_shuffled, n = 2, default = NA)], na.rm = TRUE),
      lower_bound_nrnrp = mean_nrnrp - 5,
      upper_bound_nrnrp = mean_nrnrp + 5,
      mean_ecm_nrnrp = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 &
                                        lag(IRT_shuffled, n = 2, default = NA) < 15 &
                                        lag(IRT_shuffled, n = 1, default = NA) > lag(IRT_shuffled, n = 2, default = NA) &
                                        ec_metric >= lower_bound_nrnrp & ec_metric <= upper_bound_nrnrp], na.rm = TRUE),
      
      #NRNRN calculation (removing the mean-based outlier check)
      mean_nrnrn = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 & 
                                    lag(IRT_shuffled, n = 2, default = NA) < 15 & 
                                    lag(IRT_shuffled, n = 1, default = NA) < lag(IRT_shuffled, n = 2, default = NA)], na.rm = TRUE),
      lower_bound_nrnrn = mean_nrnrn - 5,
      upper_bound_nrnrn = mean_nrnrn + 5,
      mean_ecm_nrnrn = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 &
                                        lag(IRT_shuffled, n = 2, default = NA) < 15 &
                                        lag(IRT_shuffled, n = 1, default = NA) < lag(IRT_shuffled, n = 2, default = NA) &
                                        ec_metric >= lower_bound_nrnrn & ec_metric <= upper_bound_nrnrn], na.rm = TRUE),
      
      #RNR calculation (removing the mean-based outlier check)
      mean_rnr = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 & 
                                  lag(IRT_shuffled, n = 2, default = NA) >= 15], na.rm = TRUE),
      lower_bound_rnr = mean_rnr - 5,
      upper_bound_rnr = mean_rnr + 5,
      mean_ecm_rnr = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) < 15 &
                                      lag(IRT_shuffled, n = 2, default = NA) >= 15 &
                                      ec_metric >= lower_bound_rnr & ec_metric <= upper_bound_rnr], na.rm = TRUE),
      
      #NRR calculation (removing the mean-based outlier check)
      mean_nrr = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 & 
                                  lag(IRT_shuffled, n = 2, default = NA) < 15], na.rm = TRUE),
      lower_bound_nrr = mean_nrr - 5,
      upper_bound_nrr = mean_nrr + 5,
      mean_ecm_nrr = mean(ec_metric[lag(IRT_shuffled, n = 1, default = NA) >= 15 &
                                      lag(IRT_shuffled, n = 2, default = NA) < 15 &
                                      ec_metric >= lower_bound_nrr & ec_metric <= upper_bound_nrr], na.rm = TRUE)
    )
  
  #Store results for this iteration
  null_results_list[[i]] <- subject_session_null_ecm
}
close(pb)

#Combine all iterations into a single data frame
combined_results <- bind_rows(null_results_list)

colnames(combined_results)

#Apply the second criterion to remove outliers
combined_results <- combined_results %>%
  mutate(
    mean_ecm_rrp = ifelse(mean_ecm_rrp > 5 | mean_ecm_rrp < -5, NA, mean_ecm_rrp),
    mean_ecm_rrn = ifelse(mean_ecm_rrn > 5 | mean_ecm_rrn < -5, NA, mean_ecm_rrn),
    mean_ecm_nrnrp = ifelse(mean_ecm_nrnrp > 5 | mean_ecm_nrnrp < -5, NA, mean_ecm_nrnrp),
    mean_ecm_nrnrn = ifelse(mean_ecm_nrnrn > 5 | mean_ecm_nrnrn < -5, NA, mean_ecm_nrnrn),
    mean_ecm_rnr = ifelse(mean_ecm_rnr > 5 | mean_ecm_rnr < -5, NA, mean_ecm_rnr),
    mean_ecm_nrr = ifelse(mean_ecm_nrr > 5 | mean_ecm_nrr < -5, NA, mean_ecm_nrr)
  )

#Now, group by Subj_idx and Session, and calculate the mean for each metric
mean_per_coupling <- combined_results %>%
  group_by(Subj_idx, Session) %>%
  summarise(
    mean_rrp = mean(mean_ecm_rrp, na.rm = TRUE),
    mean_rrn = mean(mean_ecm_rrn, na.rm = TRUE),
    mean_nrnrp = mean(mean_ecm_nrnrp, na.rm = TRUE),
    mean_nrnrn = mean(mean_ecm_nrnrn, na.rm = TRUE),
    mean_rnr = mean(mean_ecm_rnr, na.rm = TRUE),
    mean_nrr = mean(mean_ecm_nrr, na.rm = TRUE)
  )

#Finally, we have our null error correction metric from the shuffling
#maneuver; we have one mean per metric per Subj_idx-Session unit
print(mean_per_coupling)

 
#We are now ready to calculate the error correction metric from the
#...observed data
filtered_data <- filtered_data %>%
  group_by(Subj_idx, Session) %>%
  mutate(ec_metric = calculate_triplet_comparison(cur_data(), 'IRT')) %>%
  ungroup()

#Outlier removal 
filtered_data <- filtered_data %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    #Bounds are calculated group-wise for each Subj_idx-Session unit
    lower_bound_rrp = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15 & 
                                       lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA)], na.rm = TRUE) - 5,
    upper_bound_rrp = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15 & 
                                       lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA)], na.rm = TRUE) + 5,
    lower_bound_rrn = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15 & 
                                       lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA)], na.rm = TRUE) - 5,
    upper_bound_rrn = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15 & 
                                       lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA)], na.rm = TRUE) + 5,
    lower_bound_nrnrp = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                         lag(IRT, n = 2, default = NA) < 15 & 
                                         lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA)], na.rm = TRUE) - 5,
    upper_bound_nrnrp = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                         lag(IRT, n = 2, default = NA) < 15 & 
                                         lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA)], na.rm = TRUE) + 5,
    lower_bound_nrnrn = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                         lag(IRT, n = 2, default = NA) < 15 & 
                                         lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA)], na.rm = TRUE) - 5,
    upper_bound_nrnrn = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                         lag(IRT, n = 2, default = NA) < 15 & 
                                         lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA)], na.rm = TRUE) + 5,
    lower_bound_rnr = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15], na.rm = TRUE) - 5,
    upper_bound_rnr = mean(ec_metric[lag(IRT, n = 1, default = NA) < 15 & 
                                       lag(IRT, n = 2, default = NA) >= 15], na.rm = TRUE) + 5,
    lower_bound_nrr = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) < 15], na.rm = TRUE) - 5,
    upper_bound_nrr = mean(ec_metric[lag(IRT, n = 1, default = NA) >= 15 & 
                                       lag(IRT, n = 2, default = NA) < 15], na.rm = TRUE) + 5
  ) %>%
  mutate(
    #Remove outliers per classification
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) >= 15 & 
                          lag(IRT, n = 2, default = NA) >= 15 & 
                          lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA) &
                          (ec_metric < lower_bound_rrp | ec_metric > upper_bound_rrp)), NA, ec_metric),
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) >= 15 & 
                          lag(IRT, n = 2, default = NA) >= 15 & 
                          lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA) &
                          (ec_metric < lower_bound_rrn | ec_metric > upper_bound_rrn)), NA, ec_metric),
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) < 15 & 
                          lag(IRT, n = 2, default = NA) < 15 & 
                          lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA) &
                          (ec_metric < lower_bound_nrnrp | ec_metric > upper_bound_nrnrp)), NA, ec_metric),
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) < 15 & 
                          lag(IRT, n = 2, default = NA) < 15 & 
                          lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA) &
                          (ec_metric < lower_bound_nrnrn | ec_metric > upper_bound_nrnrn)), NA, ec_metric),
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) < 15 & 
                          lag(IRT, n = 2, default = NA) >= 15 &
                          (ec_metric < lower_bound_rnr | ec_metric > upper_bound_rnr)), NA, ec_metric),
    ec_metric = ifelse((lag(IRT, n = 1, default = NA) >= 15 & 
                          lag(IRT, n = 2, default = NA) < 15 &
                          (ec_metric < lower_bound_nrr | ec_metric > upper_bound_nrr)), NA, ec_metric)
  ) %>%
  ungroup() %>%
  #Remove the unnecessary boundary columns
  select(-lower_bound_rrp, -upper_bound_rrp, -lower_bound_rrn, -upper_bound_rrn, 
         -lower_bound_nrnrp, -upper_bound_nrnrp, -lower_bound_nrnrn, -upper_bound_nrnrn, 
         -lower_bound_rnr, -upper_bound_rnr, -lower_bound_nrr, -upper_bound_nrr)


#Filter the observed metrics for each sequence class

ecm_rrp <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) >= 15 &
           lag(IRT, n = 2, default = NA) >= 15 &
           lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA))

ecm_rrn <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) >= 15 &
           lag(IRT, n = 2, default = NA) >= 15 &
           lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA))

ecm_nrnrp <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) < 15 &
           lag(IRT, n = 2, default = NA) < 15 &
           lag(IRT, n = 1, default = NA) > lag(IRT, n = 2, default = NA))

ecm_nrnrn <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) < 15 &
           lag(IRT, n = 2, default = NA) < 15 &
           lag(IRT, n = 1, default = NA) < lag(IRT, n = 2, default = NA))

ecm_rnr <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) < 15 &
           lag(IRT, n = 2, default = NA) >= 15)

ecm_nrr <- filtered_data %>%
  filter(lag(IRT, n = 1, default = NA) >= 15 &
           lag(IRT, n = 2, default = NA) < 15)

#Removing outliers according to the second criterion

ecm_rrp <- ecm_rrp %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)  #Remove the temporary mean column

ecm_rrn <- ecm_rrn %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)

ecm_nrnrp <- ecm_nrnrp %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)

ecm_nrnrn <- ecm_nrnrn %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)

ecm_rnr <- ecm_rnr %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)

ecm_nrr <- ecm_nrr %>%
  group_by(Subj_idx, Session) %>%
  mutate(
    mean_ec_metric = mean(ec_metric, na.rm = TRUE),
    ec_metric = ifelse(mean_ec_metric > 5 | mean_ec_metric < -5, NA, ec_metric)
  ) %>%
  ungroup() %>%
  select(-mean_ec_metric)


#Merge observed data with null data

ecm_rrp <- ecm_rrp %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_rrp), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_rrp) %>%
  select(-mean_rrp)  

ecm_rrn <- ecm_rrn %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_rrn), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_rrn) %>%
  select(-mean_rrn)

ecm_nrnrp <- ecm_nrnrp %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_nrnrp), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_nrnrp) %>%
  select(-mean_nrnrp)

ecm_nrnrn <- ecm_nrnrn %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_nrnrn), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_nrnrn) %>%
  select(-mean_nrnrn)

ecm_rnr <- ecm_rnr %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_rnr), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_rnr) %>%
  select(-mean_rnr)

ecm_nrr <- ecm_nrr %>%
  left_join(mean_per_coupling %>% select(Subj_idx, Session, mean_nrr), by = c("Subj_idx", "Session")) %>%
  mutate(null_ecm = mean_nrr) %>%
  select(-mean_nrr)


#Add the difference score columns by subtracting the null means
ecm_rrp$diff <- ecm_rrp$ec_metric - ecm_rrp$null_ecm
ecm_rrn$diff <- ecm_rrn$ec_metric - ecm_rrn$null_ecm
ecm_nrnrp$diff <- ecm_nrnrp$ec_metric - ecm_nrnrp$null_ecm
ecm_nrnrn$diff <- ecm_nrnrn$ec_metric - ecm_nrnrn$null_ecm
ecm_rnr$diff <- ecm_rnr$ec_metric - ecm_rnr$null_ecm
ecm_nrr$diff <- ecm_nrr$ec_metric - ecm_nrr$null_ecm


#Correction for multiple comparisons that will be applied in the subsequent
#...analyses


#Bonferroni-Holm correction function
bonferroni_holm <- function(alpha, num_tests) {
  #Create a vector of thresholds for each test
  corrected_alpha <- alpha / (num_tests:1)
  
  #Return the corrected alpha values
  return(corrected_alpha)
}

#Uncorrected significance threshold
alpha <- 0.05
#All of our families of tests involve six p-values
num_tests <- 6
#This are the significance thresholds for our ranked p-values
bonferroni_holm(alpha, num_tests)

#Benjamini-Hochberg correction
benjamini_hochberg <- function(alpha, num_tests) {

  bh_thresholds <- (1:num_tests) / num_tests * alpha
  
  # Return the BenHoc-corrected thresholds
  return(bh_thresholds)
}

alpha <- 0.05
num_tests <- 6
#Significance thresholds for our ranked p-values according to Ben-Hoc
benjamini_hochberg(alpha, num_tests)


#You can continue with script number 5
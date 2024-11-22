#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require("patchwork")) install.packages("patchwork"); library(patchwork)
if (!require('viridis')) install.packages('viridis'); library(viridis)


#This is the estimate for the difference score for RRP sequences that is
#...reported in the manuscript
model_rrp <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrp)
summary(model_rrp)

#This is the estimate for the error correction metric for RRP sequences that
#...appears in Figure 5 
model_rrp_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrp)
summary(model_rrp_ECM)
estimate <- fixef(model_rrp_ECM)["(Intercept)"]
ci <- confint(model_rrp_ECM, parm = "(Intercept)", method = "Wald")
rrp_ECM <- c(estimate, ci)

#This is the estimate for the error correction metric obtained for the 
#...shuffled data, the null illustrated in gray in Figure 5
model_rrp_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrp)
summary(model_rrp_null)
estimate <- fixef(model_rrp_null)["(Intercept)"]
ci <- confint(model_rrp_null, parm = "(Intercept)", method = "Wald")
rrp_null <- c(estimate, ci)

#We will do the same for all other sequence classes

model_rrn <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrn)
summary(model_rrn)

model_rrn_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrn)
summary(model_rrn_ECM)
estimate <- fixef(model_rrn_ECM)["(Intercept)"]
ci <- confint(model_rrn_ECM, parm = "(Intercept)", method = "Wald")
rrn_ECM <- c(estimate, ci)

model_rrn_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rrn)
summary(model_rrn_null)
estimate <- fixef(model_rrn_null)["(Intercept)"]
ci <- confint(model_rrn_null, parm = "(Intercept)", method = "Wald")
rrn_null <- c(estimate, ci)

model_nrnrp <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrp)
summary(model_nrnrp)

model_nrnrp_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrp)
summary(model_nrnrp_ECM)
estimate <- fixef(model_nrnrp_ECM)["(Intercept)"]
ci <- confint(model_nrnrp_ECM, parm = "(Intercept)", method = "Wald")
nrnrp_ECM <- c(estimate, ci)

model_nrnrp_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrp)
summary(model_nrnrp_null)
estimate <- fixef(model_nrnrp_null)["(Intercept)"]
ci <- confint(model_nrnrp_null, parm = "(Intercept)", method = "Wald")
nrnrp_null <- c(estimate, ci)

model_nrnrn <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrn)
summary(model_nrnrn)

model_nrnrn_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrn)
summary(model_nrnrn_ECM)
estimate <- fixef(model_nrnrn_ECM)["(Intercept)"]
ci <- confint(model_nrnrn_ECM, parm = "(Intercept)", method = "Wald")
nrnrn_ECM <- c(estimate, ci)

model_nrnrn_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrnrn)
summary(model_nrnrn_null)
estimate <- fixef(model_nrnrn_null)["(Intercept)"]
ci <- confint(model_nrnrn_null, parm = "(Intercept)", method = "Wald")
nrnrn_null <- c(estimate, ci)

model_rnr <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rnr)
summary(model_rnr)

model_rnr_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rnr)
summary(model_rnr_ECM)
estimate <- fixef(model_rnr_ECM)["(Intercept)"]
ci <- confint(model_rnr_ECM, parm = "(Intercept)", method = "Wald")
rnr_ECM <- c(estimate, ci)

model_rnr_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_rnr)
summary(model_rnr_null)
estimate <- fixef(model_rnr_null)["(Intercept)"]
ci <- confint(model_rnr_null, parm = "(Intercept)", method = "Wald")
rnr_null <- c(estimate, ci)

model_nrr <- lmer(diff ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrr)
summary(model_nrr)

model_nrr_ECM <- lmer(ec_metric ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrr)
summary(model_nrr_ECM)
estimate <- fixef(model_nrr_ECM)["(Intercept)"]
ci <- confint(model_nrr_ECM, parm = "(Intercept)", method = "Wald")
nrr_ECM <- c(estimate, ci)

model_nrr_null <- lmer(null_ecm ~ 1 + (1 | Subj_idx) + (1 | Session), data = ecm_nrr)
summary(model_nrr_null)
estimate <- fixef(model_nrr_null)["(Intercept)"]
ci <- confint(model_nrr_null, parm = "(Intercept)", method = "Wald")
nrr_null <- c(estimate, ci)


#Here, we obtain the error correction metric estimates per individual just for
#...visualizing them in Figure 5 along aggregated data

#First, we do RRP sequence

#Create an empty data frame to store results
results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())

#Loop through each subject to get the estimate
for (subj in unique(ecm_rrp$Subj_idx)) {
  #Subset data for the current subject
  subset_data <- subset(ecm_rrp, Subj_idx == subj)
  
  #Fit the model on the subset data
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  #Extract the estimate for the intercept
  estimate <- fixef(model)["(Intercept)"]
  
  #Extract confidence interval
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  #Append results to the data frame
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

#Display the resulting data frame
results
#Assign a different color to each subject ID
results$color <- plasma(n = nrow(results))
#Rank the subjects in descending order acording to their estimated ecm
subj_rrp <- results[order(-results$Estimate), ]

# Initialize an empty data frame to store results
results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())

#Repeat the process with the remaining sequences

for (subj in unique(ecm_rrn$Subj_idx)) {
  
  subset_data <- subset(ecm_rrn, Subj_idx == subj)
  
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  estimate <- fixef(model)["(Intercept)"]
  
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

results
results$color <- plasma(n = nrow(results))
subj_rrn <- results[order(-results$Estimate), ]



results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())


for (subj in unique(ecm_nrnrp$Subj_idx)) {
  
  subset_data <- subset(ecm_nrnrp, Subj_idx == subj)
  
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  estimate <- fixef(model)["(Intercept)"]
  
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

results
results$color <- plasma(n = nrow(results))
subj_nrnrp <- results[order(-results$Estimate), ]



results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())


for (subj in unique(ecm_nrnrn$Subj_idx)) {
  subset_data <- subset(ecm_nrnrn, Subj_idx == subj)
  
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  estimate <- fixef(model)["(Intercept)"]
  
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

results
results$color <- plasma(n = nrow(results))
subj_nrnrn <- results[order(-results$Estimate), ]

results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())


for (subj in unique(ecm_rnr$Subj_idx)) {
  
  subset_data <- subset(ecm_rnr, Subj_idx == subj)
  
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  estimate <- fixef(model)["(Intercept)"]
  
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

results
results$color <- plasma(n = nrow(results))
subj_rnr <- results[order(-results$Estimate), ]




results <- data.frame(Subj_idx = integer(), Estimate = numeric(), CI_lower = numeric(), CI_upper = numeric())


for (subj in unique(ecm_nrr$Subj_idx)) {

  subset_data <- subset(ecm_nrr, Subj_idx == subj)
  
  model <- lmer(ec_metric ~ 1 + (1 | Session), data = subset_data)
  
  estimate <- fixef(model)["(Intercept)"]
  
  ci <- confint(model, parm = "(Intercept)", method = "Wald")
  
  results <- rbind(results, data.frame(Subj_idx = subj, Estimate = estimate, CI_lower = ci["(Intercept)", 1], CI_upper = ci["(Intercept)", 2]))
}

results
results$color <- plasma(n = nrow(results))
subj_nrr <- results[order(-results$Estimate), ]


#Prepare the data for plotting

rrp <- rbind(c(0, rrp_null, "#727272"), c(1, rrp_ECM, "#b43052"), subj_rrp) #Ben-H
rrn <- rbind(c(0, rrn_null, "#727272"), c(1, rrn_ECM, "#eaac8b"), subj_rrn) #Both
nrnrp <- rbind(c(0, nrnrp_null, "#727272"), c(1, nrnrp_ECM, "#BD9745"), subj_nrnrp) #Ben-H
nrnrn <- rbind(c(0, nrnrn_null, "#727272"), c(1, nrnrn_ECM, "#6d597a"), subj_nrnrn) #Both
rnr <- rbind(c(0, rnr_null, "#727272"), c(1, rnr_ECM, "#4b824e"), subj_rnr) #Both
nrr <- rbind(c(0, nrr_null, "#727272"), c(1, nrr_ECM, "#8c510a"), subj_nrr) #Ben-H

#RRP panel

rrp$Estimate <- as.numeric(as.character(rrp$Estimate))
rrp$CI_lower <- as.numeric(as.character(rrp$CI_lower))
rrp$CI_upper <- as.numeric(as.character(rrp$CI_upper))
rrp$color <- as.character(rrp$color)

rrp$x_pos <- c(1, 2, seq(5, length.out = nrow(rrp) - 2))

rrp_ <- ggplot() +
  geom_hline(yintercept = -1, linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = -0.8, label = "Reward securing", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = -2.4, label = "Increased risk of missing reward delivery", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = -0.53, label = "RRP Sequence", fontface = "bold", size = 4, color = "#b43052") +
  
  geom_errorbar(data = rrp[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = rrp[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = rrp[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = rrp[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  # Statistical significance annotation
  annotate("segment", x = 0.9, xend = 2.1, y = -0.85, yend = -0.85, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = -0.7, label = "* | &", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  
  scale_y_continuous(limits = c(-2.5, -0.5), name = "Error Correction Metric") +
  
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

#Display the plot
rrp_


#Repeat for all other sequence classes

rrn$Estimate <- as.numeric(as.character(rrn$Estimate))
rrn$CI_lower <- as.numeric(as.character(rrn$CI_lower))
rrn$CI_upper <- as.numeric(as.character(rrn$CI_upper))
rrn$color <- as.character(rrn$color)

rrn$x_pos <- c(1, 2, seq(5, length.out = nrow(rrn) - 2))

rrn_ <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = -0.2, label = "Reward securing", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = 1.4, label = "Increased risk of missing reward delivery", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = -0.95, label = "RRN Sequence", fontface = "bold", size = 4, color = "#eaac8b") +
  
  geom_errorbar(data = rrn[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = rrn[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = rrn[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = rrn[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  annotate("segment", x = 0.9, xend = 2.1, y = -0.15, yend = -0.15, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = -0.35, label = "** | & | #", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_reverse(limits = c(1.5, -1)) +
  
  labs(y = "Error Correction Metric") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

rrn_




nrnrp$Estimate <- as.numeric(as.character(nrnrp$Estimate))
nrnrp$CI_lower <- as.numeric(as.character(nrnrp$CI_lower))
nrnrp$CI_upper <- as.numeric(as.character(nrnrp$CI_upper))
nrnrp$color <- as.character(nrnrp$color)


nrnrp$x_pos <- c(1, 2, seq(5, length.out = nrow(nrnrp) - 2))

nrnrp_ <- ggplot() +
  annotate("text", x = 3.5, y = 2.45, label = "Higher reward probability", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = 0.1, label = "Lower reward probability", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = 2.45, label = "NRNRP Sequence", fontface = "bold", size = 4, color = "#BD9745") +
  
  geom_errorbar(data = nrnrp[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = nrnrp[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = nrnrp[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = nrnrp[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  annotate("segment", x = 0.9, xend = 2.1, y = 1.4, yend = 1.4, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = 1.6, label = "* | &", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 2.5)) +
  
  labs(y = "Error Correction Metric") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

nrnrp_





nrnrn$Estimate <- as.numeric(as.character(nrnrn$Estimate))
nrnrn$CI_lower <- as.numeric(as.character(nrnrn$CI_lower))
nrnrn$CI_upper <- as.numeric(as.character(nrnrn$CI_upper))
nrnrn$color <- as.character(nrnrn$color)


nrnrn$x_pos <- c(1, 2, seq(5, length.out = nrow(nrnrn) - 2))


nrnrn_ <- ggplot() +
  geom_hline(yintercept = -1, linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = -0.82, label = "Failure to obtain reward", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = -2.7, label = "Increased reward probability", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = -2.67, label = "NRNRN Sequence", fontface = "bold", size = 4, color = "#6d597a") +
  
  geom_errorbar(data = nrnrn[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = nrnrn[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = nrnrn[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = nrnrn[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  annotate("segment", x = 0.9, xend = 2.1, y = -2.2, yend = -2.2, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = -2.35, label = "*** | & | #", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_reverse(limits = c(-0.72, -2.72)) +
  
  labs(y = "Error Correction Metric") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

nrnrn_




rnr$Estimate <- as.numeric(as.character(rnr$Estimate))
rnr$CI_lower <- as.numeric(as.character(rnr$CI_lower))
rnr$CI_upper <- as.numeric(as.character(rnr$CI_upper))
rnr$color <- as.character(rnr$color)


rnr$x_pos <- c(1, 2, seq(5, length.out = nrow(rnr) - 2))

rnr_ <- ggplot() +
  geom_hline(yintercept = -1, linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = -1.1, label = "Reward securing", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = -0.22, label = "Lower reward probability", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = -1.15, label = "RNR Sequence", fontface = "bold", size = 4, color = "#4b824e") +
  
  geom_errorbar(data = rnr[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = rnr[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = rnr[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = rnr[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  annotate("segment", x = 0.9, xend = 2.1, y = -0.9, yend = -0.9, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = -0.96, label = "*** | & | #", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_reverse(limits = c(-0.18, -1.18), breaks = c(-0.5,-1)) +
  
  labs(y = "Error Correction Metric") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

rnr_



nrr$Estimate <- as.numeric(as.character(nrr$Estimate))
nrr$CI_lower <- as.numeric(as.character(nrr$CI_lower))
nrr$CI_upper <- as.numeric(as.character(nrr$CI_upper))
nrr$color <- as.character(nrr$color)


nrr$x_pos <- c(1, 2, seq(5, length.out = nrow(nrr) - 2))

nrr_ <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = 3.5, y = 0.1, label = "Reward securing", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 3.5, y = -0.8, label = "Increased risk of missing reward delivery", color = "black", hjust = 0.5, size = 3) +
  annotate("text", x = 8.25, y = 0.13, label = "NRR Sequence", fontface = "bold", size = 4, color = "#8c510a") +
  
  geom_errorbar(data = nrr[1:2, ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper, color = color), width = 0, size = 1.5) +
  geom_point(data = nrr[1:2, ], aes(x = x_pos, y = Estimate, color = color, fill = color), size = 3.5, shape = 21, stroke = 0) +
  
  geom_errorbar(data = nrr[-(1:2), ], aes(x = x_pos, ymin = CI_lower, ymax = CI_upper), width = 0, color = "black", size = 1) +
  geom_point(data = nrr[-(1:2), ], aes(x = x_pos, y = Estimate, fill = color), size = 3, shape = 21, color = "black") +
  
  annotate("segment", x = 0.9, xend = 2.1, y = -0.15, yend = -0.15, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = -0.08, label = "* | &", color = "black", size = 3) +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(-0.85, 0.15), breaks = c(-0.5, 0)) +
  
  labs(y = "Error Correction Metric") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

nrr_



#Figure 5, finally!
all_seqs <- rrp_/rrn_/nrnrp_/nrnrn_/rnr_/nrr_ 
all_seqs



#You can continue with script number 6


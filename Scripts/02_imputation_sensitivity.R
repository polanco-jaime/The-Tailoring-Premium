# ==============================================================================
# Replication Script: Imputation Sensitivity Analysis (Zero vs Quantiles)
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, fixest, tidyr, ggplot2, broom)

# Load Data
df <- read_csv("Data/minimal_dataset.csv")

# 2. Data Preparation & Imputation ---------------------------------------------

# A. Define Baseline SD for Standardization (Control Group Pre-Test)
sd_control_base <- sd(df$score_pre[df$treatment_arm == "Control"], na.rm = TRUE)

# B. Calculate Raw Scores
df <- df %>%
  mutate(
    treat_generic  = as.numeric(treatment_arm == "Generic AI"),
    treat_tailored = as.numeric(treatment_arm == "Tailored AI"),
    gain_raw = score_post - score_pre,
    retention_raw = score_retention - score_pre
  )

# C. Calculate Quantiles for COMPLETERS ONLY
# (These are the values we assign to dropouts in the sensitivity scenarios)
q_gain <- quantile(df$gain_raw[df$completed_module == 1], 
                   probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)

q_ret <- quantile(df$retention_raw[df$completed_retention == 1], 
                  probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)

# D. Create Imputed Outcome Variables
# We creates columns directly in the DF for feols to use
df <- df %>%
  mutate(
    # --- IMMEDIATE LEARNING OUTCOMES ---
    # If completed, use real score. If dropout, use specific imputation value.
    # Finally, standardize by sd_control_base.
    
    gain_imp_Zero = if_else(completed_module == 1, gain_raw, 0) / sd_control_base,
    gain_imp_P10  = if_else(completed_module == 1, gain_raw, q_gain["10%"]) / sd_control_base,
    gain_imp_P25  = if_else(completed_module == 1, gain_raw, q_gain["25%"]) / sd_control_base,
    gain_imp_P50  = if_else(completed_module == 1, gain_raw, q_gain["50%"]) / sd_control_base,
    gain_imp_P75  = if_else(completed_module == 1, gain_raw, q_gain["75%"]) / sd_control_base,
    gain_imp_P90  = if_else(completed_module == 1, gain_raw, q_gain["90%"]) / sd_control_base,
    
    # --- KNOWLEDGE RETENTION OUTCOMES ---
    ret_imp_Zero = if_else(completed_retention == 1, retention_raw, 0) / sd_control_base,
    ret_imp_P10  = if_else(completed_retention == 1, retention_raw, q_ret["10%"]) / sd_control_base,
    ret_imp_P25  = if_else(completed_retention == 1, retention_raw, q_ret["25%"]) / sd_control_base,
    ret_imp_P50  = if_else(completed_retention == 1, retention_raw, q_ret["50%"]) / sd_control_base,
    ret_imp_P75  = if_else(completed_retention == 1, retention_raw, q_ret["75%"]) / sd_control_base,
    ret_imp_P90  = if_else(completed_retention == 1, retention_raw, q_ret["90%"]) / sd_control_base
  )

# 3. Estimation ----------------------------------------------------------------

# We use feols with multiple LHS variables to run all models at once efficiently
# Controls: score_pre + gender + track
# Fixed Effects: school_id
# Cluster: classroom_id

# Run Gain Models
models_gain <- feols(c(gain_imp_Zero, gain_imp_P10, gain_imp_P25, gain_imp_P50, gain_imp_P75, gain_imp_P90) 
                     ~ treat_generic + treat_tailored + score_pre + gender + track | school_id, 
                     cluster = ~classroom_id, data = df)

# Run Retention Models
models_ret <- feols(c(ret_imp_Zero, ret_imp_P10, ret_imp_P25, ret_imp_P50, ret_imp_P75, ret_imp_P90) 
                    ~ treat_generic + treat_tailored + score_pre + gender + track | school_id, 
                    cluster = ~classroom_id, data = df)

# 4. Extract Results into Plotting Dataframes ----------------------------------

# Helper function to extract tidy results and format like your previous code
extract_results_to_df <- function(model_list, imputation_labels) {
  results_list <- lapply(seq_along(model_list), function(i) {
    tidy(model_list[[i]]) %>%
      filter(term %in% c("treat_generic", "treat_tailored")) %>%
      select(term, estimate, std.error) %>%
      mutate(imputation_level = imputation_labels[i])
  })
  bind_rows(results_list)
}

imp_levels <- c("Zero", "P10", "P25", "P50", "P75", "P90")

# Create the dataframes
gained_learning_plot_data <- extract_results_to_df(models_gain, imp_levels) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    treatment = ifelse(term == "treat_generic", "Generic Chatbot", "Tailored Chatbot"),
    imputation_level = factor(imputation_level, levels = imp_levels)
  )

retention_plot_data <- extract_results_to_df(models_ret, imp_levels) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    treatment = ifelse(term == "treat_generic", "Generic Chatbot", "Tailored Chatbot"),
    imputation_level = factor(imputation_level, levels = imp_levels)
  )

# 5. Plotting (Using your exact structure) -------------------------------------

# --- Plot for Gained Learning ---
plot_gained_enhanced <- ggplot() +
  # Add the transparent grey rectangle for the "Zero" imputation column
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.08) +
  
  # Add the main point and error bar estimates
  geom_point(data = gained_learning_plot_data,
             aes(x = imputation_level, y = estimate, color = treatment),
             position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(data = gained_learning_plot_data,
                aes(x = imputation_level, ymin = conf.low, ymax = conf.high, color = treatment),
                width = 0.2, position = position_dodge(width = 0.5)) +
  
  # Add the connecting lines ONLY for the percentile data
  geom_line(data = filter(gained_learning_plot_data, imputation_level != "Zero"),
            aes(x = as.numeric(imputation_level), y = estimate, color = treatment, group = treatment),
            position = position_dodge(width = 0.5), linewidth = 0.65, alpha = 0.7) +
  
  # Add the zero line for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Define colors
  scale_color_manual(values = c("Generic Chatbot" = "#F8766D", "Tailored Chatbot" = "#00BFC4")) +
  
  # Professional labels and theme
  labs(
    title = "Panel A: Gained Financial Literacy (Immediate)",
    x = "Imputation Level for Attritors",
    y = "ITT Effect Estimate (in SDs)",
    color = "Treatment Group"
  ) +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# --- Plot for Knowledge Retention ---
plot_retention_enhanced <- ggplot() +
  # Add the transparent grey rectangle
  geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.08) +
  
  # Add the main point and error bar estimates
  geom_point(data = retention_plot_data,
             aes(x = imputation_level, y = estimate, color = treatment),
             position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(data = retention_plot_data,
                aes(x = imputation_level, ymin = conf.low, ymax = conf.high, color = treatment),
                width = 0.2, position = position_dodge(width = 0.5)) +
  
  # Add the connecting lines ONLY for the percentile data
  geom_line(data = filter(retention_plot_data, imputation_level != "Zero"),
            aes(x = as.numeric(imputation_level), y = estimate, color = treatment, group = treatment),
            position = position_dodge(width = 0.5), linewidth = 0.65, alpha = 0.7) +
  
  # Add the zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Define colors
  scale_color_manual(values = c("Generic Chatbot" = "#F8766D", "Tailored Chatbot" = "#00BFC4")) +
  
  # Professional labels and theme
  labs(
    title = "Panel B: Knowledge Retention (2 Months Later)",
    x = "Imputation Level for Attritors",
    y = "ITT Effect Estimate (in SDs)",
    color = "Treatment Group"
  ) +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# 6. Save and Print ------------------------------------------------------------

print(plot_gained_enhanced)
print(plot_retention_enhanced)

dir.create("Output/Graphs", recursive = TRUE, showWarnings = FALSE)
ggsave("Output/Graphs/imputation_sensitivity_gained_learning_sd.png", plot_gained_enhanced, width = 8, height = 6, dpi = 300)
ggsave("Output/Graphs/imputation_sensitivity_knowledge_retention_sd.png", plot_retention_enhanced, width = 8, height = 6, dpi = 300)

print("Sensitivity Analysis Completed Successfully.")
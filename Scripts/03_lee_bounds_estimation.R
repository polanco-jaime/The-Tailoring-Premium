# ==============================================================================
# Replication Script: Lee (2009) Bounds with ANCOVA Adjustment
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, fixest, broom)

# Load Data
df <- read_csv("Data/minimal_dataset.csv")

# Source the Lee Bounds function (Must be present in the folder)
# Ensure "lee_bounds.R" is inside the "Scripts/functions/" folder
source("Scripts/functions/lee_bounds.R")

# 2. Data Preparation: The ANCOVA Adjustment -----------------------------------

# A. Define Baseline SD for Standardization
sd_base <- sd(df$score_pre[df$treatment_arm == "Control"], na.rm = TRUE)

# --- B. FIX FOR FACTOR LEVELS (The Correction) ---
# We must identify which factor levels exist in the Control Group Completers.
# If a level (e.g., "Other") exists in Treatment but NOT Control, prediction fails.

# 1. Identify valid levels from the Training Set (Control Completers)
train_data <- df %>% filter(treatment_arm == "Control", completed_module == 1)
valid_genders <- unique(train_data$gender)
valid_tracks  <- unique(train_data$track)

# 2. Clean the Data for Prediction
# Any level not seen in the training set becomes NA.
df_clean <- df %>%
  mutate(
    gender_safe = if_else(gender %in% valid_genders, gender, NA_character_),
    track_safe  = if_else(track %in% valid_tracks, track, NA_character_)
  )

# --- C. PREPARE IMMEDIATE LEARNING (ANCOVA) ---

# 1. Isolate Control Completers (using the safe variables)
control_comp_imm <- df_clean %>% 
  filter(treatment_arm == "Control", completed_module == 1)

# 2. Train Model (Post Score ~ Baseline + Controls | School FE)
model_ancova_imm <- feols(score_post ~ score_pre + gender_safe + track_safe | school_id, 
                          data = control_comp_imm)

# 3. Predict Counterfactual
# This will generate NAs for students with "new levels", effectively dropping them
# from the Lee Bounds, which is the statistically correct approach here.
df_clean$pred_imm <- predict(model_ancova_imm, newdata = df_clean)

# 4. Calculate and Standardize Residuals
# Residual = (Actual Score) - (Predicted Score based on Control Logic)
df_clean$resid_imm <- (df_clean$score_post - df_clean$pred_imm) / sd_base


# --- D. PREPARE KNOWLEDGE RETENTION (ANCOVA) ---

# 1. Isolate Control Completers (Retention)
control_comp_ret <- df_clean %>% 
  filter(treatment_arm == "Control", completed_retention == 1)

# 2. Train Model
model_ancova_ret <- feols(score_retention ~ score_pre + gender_safe + track_safe | school_id, 
                          data = control_comp_ret)

# 3. Predict
df_clean$pred_ret <- predict(model_ancova_ret, newdata = df_clean)

# 4. Calculate Residuals
df_clean$resid_ret <- (df_clean$score_retention - df_clean$pred_ret) / sd_base


# 3. Estimation: Run Lee Bounds ------------------------------------------------

# Wrapper function to make printing cleaner
run_lee <- function(d, outcome, treat_val) {
  lee_bounds(
    data = d,
    outcome_var = outcome,
    group_var = "treatment_arm",
    treat_value = treat_val,
    control_value = "Control",
    selection_var = if_else(outcome == "resid_imm", "completed_module", "completed_retention")
  )
}

# --- IMMEDIATE LEARNING BOUNDS ---
cat("\n========================================================")
cat("\n LEE BOUNDS: IMMEDIATE LEARNING (ANCOVA Adjusted)")
cat("\n========================================================\n")

# Generic AI vs Control
bounds_imm_generic <- run_lee(df_clean, "resid_imm", "Generic AI")
print(bounds_imm_generic)

# Tailored AI vs Control
bounds_imm_tailored <- run_lee(df_clean, "resid_imm", "Tailored AI")
print(bounds_imm_tailored)


# --- KNOWLEDGE RETENTION BOUNDS ---
cat("\n========================================================")
cat("\n LEE BOUNDS: KNOWLEDGE RETENTION (ANCOVA Adjusted)")
cat("\n========================================================\n")

# Generic AI vs Control
bounds_ret_generic <- run_lee(df_clean, "resid_ret", "Generic AI")
print(bounds_ret_generic)

# Tailored AI vs Control
bounds_ret_tailored <- run_lee(df_clean, "resid_ret", "Tailored AI")
print(bounds_ret_tailored)


# 4. Export Summary Table ------------------------------------------------------

# Helper to extract data from the Lee Bounds Object
extract_bounds <- function(obj, name) {
  if(is.null(obj)) return(NULL)
  data.frame(
    Comparison = name,
    Lower_Bound = sprintf("%.3f", obj$bounds$lower_bound),
    Upper_Bound = sprintf("%.3f", obj$bounds$upper_bound),
    CI_Lower_95 = sprintf("%.3f", obj$ci_imbens_manski$lower),
    CI_Upper_95 = sprintf("%.3f", obj$ci_imbens_manski$upper),
    Trimming_Rate = sprintf("%.1f%%", obj$summary_stats$trim_fraction * 100)
  )
}

# Combine results
results_table <- bind_rows(
  extract_bounds(bounds_imm_generic, "Immediate: Generic AI"),
  extract_bounds(bounds_imm_tailored, "Immediate: Tailored AI"),
  extract_bounds(bounds_ret_generic, "Retention: Generic AI"),
  extract_bounds(bounds_ret_tailored, "Retention: Tailored AI")
)

print(results_table)

# Create Output directory if it doesn't exist
dir.create("Output", showWarnings = FALSE)

# Save to CSV
write_csv(results_table, "Output/lee_bounds_summary.csv")

print("Lee Bounds Estimation Completed.")
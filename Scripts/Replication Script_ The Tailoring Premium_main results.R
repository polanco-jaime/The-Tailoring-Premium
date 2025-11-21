# ==============================================================================
# Replication Script: The Tailoring Premium
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, fixest, broom, knitr)
# df = clean_df
# Load Data
df <- read_csv("Data/minimal_dataset.csv")

# Source Lee Bounds function (Make sure to include the file in the package)
source("Scripts/functions/lee_bounds.R") 

# 2. Variable Creation ---------------------------------------------------------
df <- df %>%
  mutate(
    # Treatment Dummies
    treat_generic = as.numeric(treatment_arm == "Generic AI"),
    treat_tailored = as.numeric(treatment_arm == "Tailored AI"),
    
    # Standardize Scores (Using Control Group Baseline SD)
    # Logic derived from 'Learning Efficiency and Durability.R'
    sd_control_base = sd(score_pre[treatment_arm == "Control"], na.rm = TRUE),
    
    gain_raw = score_post - score_pre,
    gain_std = gain_raw / sd_control_base,
    
    retention_raw = score_retention - score_pre,
    retention_std = retention_raw / sd_control_base,
    
    # Imputation for ITT (Assumes 0 gain for attritors)
    gain_std_itt = if_else(completed_module == 1, gain_std, 0),
    retention_std_itt = if_else(completed_retention == 1, retention_std, 0),
    gain_raw_imputed = if_else(is.na(gain_raw), 0, gain_raw),
    gain_std_iv = gain_raw_imputed / sd_control_base
  )

# 3. Analysis: Engagement (Table 4) --------------------------------------------
cat("\n--- Table 4: Engagement (Module Completion) ---\n")

# Logic derived from 'ITT Engagement.R'
# Using classroom_id for Fixed Effects and Clustering as per 'LATE Estimates.R' design
unique(df$classroom_id)
model_eng_glm <- feglm(
  completed_module ~ treat_generic + treat_tailored |  school_id ,
  data = df, 
  cluster = ~school_id,
  family = binomial("logit")
)

model_eng_ols <- feglm(
  completed_module ~ treat_generic + treat_tailored |  school_id ,
  data = df,
  cluster = ~school_id,
)
etable(model_eng_ols, model_eng_glm, 
       headers = c( "OLS (LPM)", "Logit (Odds)"),
       dict = c(treat_generic = "Generic AI", treat_tailored = "Tailored AI"),
       fitstat = c("n", "r2", "pr2")) 
# 4. Analysis: Learning Outcomes ITT (Table 3) ---------------------------------
cat("\n--- Table 3: ITT Learning Effects (Imputed 0) ---\n")

# Logic derived from 'Zero_imputation_sensibility_sd.R'
model_learn <- feols(
  c(gain_std_itt, retention_std_itt) ~ treat_generic + treat_tailored 
   + score_pre + gender
  | id_classroom,
   cluster = ~id_classroom,
  data = df
)
 
etable(model_learn, headers = c("Immediate Learning (SD)", "Retention (SD)"))

# 5. Analysis: LATE Estimates (Table 5) ----------------------------------------
cat("\n--- Table 5: LATE Estimates (IV) ---\n")

# Logic derived from 'LATE Estimates.R'
# We estimate pairwise to isolate the LATE for each specific treatment
df_t2 <- df %>% filter(treatment_arm %in% c("Control", "Tailored AI"))
df_t1 <- df %>% filter(treatment_arm %in% c("Control", "Generic AI"))

# IV: Instrumenting 'completed_module' with random assignment
 

late_tailored <- feols(
  gain_std_iv ~ score_pre + gender | classroom_id | completed_module ~ treat_tailored,
  cluster = ~classroom_id,
  data = df_t2
)

# Generic AI (T1) LATE
late_generic <- feols(
  gain_std_iv ~ score_pre + gender | classroom_id | completed_module ~ treat_generic,
  cluster = ~classroom_id,
  data = df_t1
)

etable(late_generic, late_tailored, 
       headers = c("Generic AI (LATE)", "Tailored AI (LATE)"))

# 6. Mechanisms: Self-Confidence (Exploratory) --------------------------------
cat("\n--- Mechanism: Change in Self-Confidence (Completers) ---\n")

# Logic derived from 'Immediate Learning and Psychosocial Outcomes.R'
df_comp <- df %>% 
  filter(completed_module == 1) %>%
  mutate(delta_conf = psy_self_conf_post - psy_self_conf_pre)

mech_model <- feols(delta_conf ~ treat_generic + treat_tailored + score_pre | school_id,
                    cluster = ~classroom_id, data = df_comp)

etable(mech_model)

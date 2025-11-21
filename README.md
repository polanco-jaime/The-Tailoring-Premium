
--------------------------------------------------------------------------------
README - REPLICATION PACKAGE
--------------------------------------------------------------------------------

Title:   The Tailoring Premium: How AI Design Unlocks Student Engagement and Learning
Authors: Jaime Polanco-Jiménez & Kristof De Witte
Date:    November 20, 2025

--------------------------------------------------------------------------------
1. OVERVIEW
--------------------------------------------------------------------------------

This replication package contains the code and data required to reproduce the 
main findings, tables, and figures presented in the paper. 

Due to the sensitivity of student data (GDPR regulations regarding minors), 
the raw data containing Personally Identifiable Information (PII) such as names, 
IP addresses, and raw survey timestamps cannot be shared. 

Instead, we provide a "Minimal De-identified Dataset" (`minimal_dataset.csv`). 
This dataset contains:
1. Anonymized identifiers (Student, School, Classroom).
2. Treatment assignments.
3. Pre-processed outcome variables (Standardized scores).
4. Necessary covariates for the regression models.

The analysis scripts provided here take this minimal dataset as input and 
output the exact estimates reported in the paper.

--------------------------------------------------------------------------------
2. FOLDER STRUCTURE
--------------------------------------------------------------------------------

The package is organized as follows:

The-Tailoring-Premium/
|
+--- Data/
|    |
|    +--- minimal_dataset.csv        # The de-identified analysis file
|
+--- Scripts/
|    |
|    +--- 01_main_analysis.R         # Replicates Tables 3, 4, and 5
|    +--- 02_imputation_sensitivity.R# Replicates Figure 2 (Sensitivity Plots)
|    +--- 03_lee_bounds_estimation.R # Replicates Lee Bounds estimates
|    |
|    +--- functions/
|         |
|         +--- lee_bounds.R           # Custom function for Lee (2009) bounds
|
+--- Output/                        # Empty folder where results will be saved
|    |
|    +--- Graphs/                    # Generated figures will appear here
|
+--- README.txt                     # This file


--------------------------------------------------------------------------------
3. SOFTWARE REQUIREMENTS
--------------------------------------------------------------------------------

The analysis was conducted using R.

- R Version: 4.3.0 or higher is recommended.
- Required Packages: 
  The scripts use the `pacman` package manager to automatically install 
  missing dependencies. The following packages are used:
  - readr
  - dplyr
  - tidyr
  - fixest (for high-dimensional fixed effects and IV)
  - broom
  - ggplot2
  - knitr

--------------------------------------------------------------------------------
4. DATA DICTIONARY (Codebook)
--------------------------------------------------------------------------------

File: Data/minimal_dataset.csv

| Variable            | Type    | Description                                      |
|---------------------|---------|--------------------------------------------------|
| student_id          | Numeric | Anonymized unique student identifier.            |
| school_id           | Numeric | Anonymized unique school identifier.             |
| classroom_id        | Numeric | Anonymized classroom ID (derived from IP subnet).|
| treatment_arm       | String  | Experimental condition: "Control", "Generic AI", "Tailored AI". |
| gender              | String  | Student gender ("Female", "Male", "Other").      |
| track               | String  | School track (ASO, TSO, BSO, KSO).               |
| teacher_shortage    | Numeric | 1 if school has teacher shortage, 0 otherwise.   |
| score_pre           | Numeric | Baseline financial literacy score (0-100 scale). |
| score_post          | Numeric | Post-intervention score (0-100 scale).           |
| score_retention     | Numeric | 2-month follow-up score (0-100 scale).           |
| completed_module    | Numeric | 1 if student completed the learning module, 0 if attrited. |
| completed_retention | Numeric | 1 if student completed the follow-up test, 0 otherwise. |
| psy_attitude_pre    | Numeric | Baseline Attitude & Motivation (Likert 1-5).     |
| psy_self_conf_pre   | Numeric | Baseline Self-Confidence (Likert 1-5).           |
| psy_self_conf_post  | Numeric | Post-test Self-Confidence (Likert 1-5).          |

--------------------------------------------------------------------------------
5. INSTRUCTIONS TO REPLICATE
--------------------------------------------------------------------------------

1. Unzip the "Replication_Package" folder to your computer.

2. Open R or RStudio.

3. Set your Working Directory to the "Replication_Package" folder.
   Example: setwd("C:/Users/Name/Documents/Replication_Package")

4. Run the scripts in the following order:

   A. Run `Scripts/01_main_analysis.R`
      - This script loads the data and performs the primary regressions.
      - It outputs the text content for:
        * Table 4: Engagement (OLS vs Logit robustness).
        * Table 3: ITT Learning Effects.
        * Table 5: LATE Estimates (Instrumental Variable).
        * Mechanism analysis (Self-confidence).

   B. Run `Scripts/02_imputation_sensitivity.R`
      - This script performs the robustness check for missing data.
      - It generates two plots in the `Output/Graphs/` folder:
        * `imputation_sensitivity_gained_learning_sd.png`
        * `imputation_sensitivity_knowledge_retention_sd.png`

   C. Run `Scripts/03_lee_bounds_estimation.R`
      - This script calculates the Lee (2009) bounds with ANCOVA adjustment.
      - It handles factor level mismatches (e.g., genders present in treatment but not control).
      - It saves a summary CSV to `Output/lee_bounds_summary.csv`.

--------------------------------------------------------------------------------
6. NOTES ON METHODOLOGY
--------------------------------------------------------------------------------

- **Standardization:** Outcomes (Gain and Retention) are standardized using the 
  Standard Deviation of the *Control Group's Baseline Score*. This ensures 
  comparability and avoids contamination from treatment effects on variance.

- **Attrition:** 
  - For ITT estimates (Table 3), missing scores for non-completers are imputed as 0 (no learning gain).
  - For LATE estimates (Table 5), we use 2SLS where assignment instruments for completion.
  - For Lee Bounds, we strictly adhere to the monotonic selection assumption.

- **Clustering:** All standard errors are clustered at the `classroom_id` level 
  to account for correlated shocks within the physical learning environment.

--------------------------------------------------------------------------------
7. CONTACT
--------------------------------------------------------------------------------

For questions regarding this code or data, please contact:
Jaime Polanco-Jiménez
Leuven Economics of Education Research, KU Leuven
Departamente de Economia, Pontificia Universidad Javeriana
Email: jaime.polancojimenez@kuleuven.be
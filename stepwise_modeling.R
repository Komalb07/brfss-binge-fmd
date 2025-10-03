###############################################################################
# Project: Binge Drinking and Mental Distress Across Racial/Ethnic Groups
# Script: STEPWISE_MODELING
# Author: Komal Bhimireddy
# Description:
#   This script performs stepwise survey-weighted logistic regression models.
# Notes:
# - Place .rda file in the /brfss_merged_data folder before running this script.
# - Outputs (tables, plots) will be saved in /outputs.
###############################################################################

# ----------------------------- #
# 1. Load cleaned dataset
# ----------------------------- #
# Load necessary packages
library(dplyr)  # For data manipulation

# Set the file path (update if stored elsewhere)
file_path <- "brfss_merged_data/final_analytic_sample_2013_2019.rda"

# Load the .rda file into the environment
load(file_path)

# ----------------------------- #
# 2. Check for multi-collinearity
# ----------------------------- #
#Generate the correlation plot for the predictor variables

library(rcompanion)
library(corrplot)

# Exclude FMD (response variable), STRATA and SurveyWeight
categorical_vars <- setdiff(colnames(brfss_merged2),c('fmd', 'psu','survey_weight', 'strata'))
predictor_data <- brfss_merged2[, categorical_vars]

# Define the Cramér’s V matrix function
cramers_v_matrix <- function(df) {
  vars <- names(df)
  result <- matrix(NA, nrow = length(vars), ncol = length(vars),
                   dimnames = list(vars, vars))
  for (i in vars) {
    for (j in vars) {
      if (i != j) {
        tbl <- table(df[[i]], df[[j]])
        result[i, j] <- cramerV(tbl, bias.correct = TRUE)
      } else {
        result[i, j] <- 1  # perfect self-association
      }
    }
  }
  return(result)
}

# Compute the Cramér’s V matrix
cramer_v_mat <- cramers_v_matrix(predictor_data)

#Export the plot to jpg format
jpeg(file.path("outputs", "cramers_v_plot.jpg"), width = 2000, height = 2000, res = 300)

# Plot the matrix using corrplot
corrplot(cramer_v_mat, method = "number", is.corr = FALSE,
         type = "upper",
         tl.cex = 0.5,             # variable name text size
         tl.col = "black",         # variable name color
         cl.cex = 0.6,             # legend text size
         number.cex = 0.5,         # value text size
         number.digits = 2,        # show 2 decimal places
         mar = c(1,1,1,1))

dev.off()

# Computing the VIF (Variation Inflation Factor)
library(car)

# Binary logistic regression
model <- glm(fmd ~ binge_drinker + race_ethnicity + age_group + gender + income + year + education +
               employment + marital_status + insurance + home_ownership + smoking_status +
               heavy_drinker + physically_active + bmi_category + asthma + arthritis + copd +
               chd + diabetes + kidney_disease + stroke + disability + mcc,
             data = brfss_merged2, family = "binomial")

# Compute VIF
vif_values <- vif(model)

# View results
print(vif_values)

# ----------------------------- #
# 3. Define survey design object
# ----------------------------- #

library(survey)

brfss_svy <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~survey_weight,
  data = brfss_merged2,
  nest = TRUE
)

options(survey.lonely.psu = "adjust")

# ----------------------------- #
# 4. Stepwise Logistic Regression
# ----------------------------- #

# Model 1: FMD ~ BingeDrinker
model_base <- svyglm(
  fmd ~ binge_drinker,
  design = brfss_svy,
  family = binomial(link = "logit") # Use quasibinomial() to handle overdispersion (If there is any)
)

# Model 2: + socio-demographic factors
model_socio_dem <- svyglm(
  fmd ~ binge_drinker + race_ethnicity + age_group + gender + marital_status,
  design = brfss_svy,
  family = binomial(link = "logit") 
)

# Model 3: + socioeconomic factors
model_socio_econ <- svyglm(
  fmd ~ binge_drinker + race_ethnicity + age_group + gender + marital_status + education + income + 
    employment + home_ownership + insurance,
  design = brfss_svy,
  family = binomial(link = "logit") 
  
# Model 4: + health behaviors
model_health_behav <- svyglm(
  fmd ~ binge_drinker + race_ethnicity + age_group + gender + marital_status + education + income + 
    employment + home_ownership + insurance + physically_active + bmi_category,
  design = brfss_svy,
  family = binomial(link = "logit")
)

# Model 5: + smoking
model_smoking_stat <- svyglm(
  fmd ~ binge_drinker + race_ethnicity + age_group + gender + marital_status + education + income + 
    employment + home_ownership + insurance + physically_active + bmi_category + smoking_status,
  design = brfss_svy,
  family = binomial(link = "logit")
)

# Model 6: + health conditions and year
model_health_cond <- svyglm(
  fmd ~ binge_drinker + race_ethnicity + age_group + gender + marital_status + education + income + 
    employment + home_ownership + insurance + physically_active + bmi_category + smoking_status + 
    asthma + arthritis + copd + chd + diabetes + stroke + kidney_disease + disability + mcc + year,
  
  design = brfss_svy,
  family = binomial(link = "logit")
  
# ----------------------------- #
# 5. Model Performance
# ----------------------------- #
# Load the performance package
library(performance)

# Collect your models into a list
models <- list(
  Model1 = model_base,
  Model2 = model_socio_dem,
  Model3 = model_socio_econ,
  Model4 = model_health_behav,
  Model5 = model_smoking_stat,
  Model6 = model_health_cond 
)

# Compare model fit statistics
comparison <- compare_performance(models, metrics = c("AIC", "BIC", "R2", "RMSE"))

# Print table
print(comparison)

# nice formatted table
library(sjPlot)
# Save the tab_model output as HTML in /outputs folder
tab_model(
  models,
  show.aic = TRUE,
  show.ci = FALSE,
  show.icc = FALSE,
  file = "outputs/model_performance_table.html"
)
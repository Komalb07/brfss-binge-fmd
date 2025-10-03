###############################################################################
# Project: Binge Drinking and Mental Distress Across Racial/Ethnic Groups
# Script: DATA_PREPARATION
# Author: Komal Bhimireddy
# Description:
#   This script prepares the BRFSS 2013–2019 dataset.
# Notes:
# - The raw BRFSS .XPT files must be downloaded separately from CDC.
# - Place raw data in the /data folder before running this script.
# - Outputs (tables, plots) will be saved in /outputs.
###############################################################################

# ----------------------------- #
# 1. Load required libraries
# ----------------------------- #

library(haven)
library(dplyr)
library(readr)
library(purrr)

# ----------------------------- #
# 2. Data Import
# ----------------------------- #

years <- 2013:2019
file_paths <- paste0("LLCP/LLCP", years, ".XPT")

# ----------------------------- #
# 3. Data Cleaning & Recoding
# ----------------------------- #
# Define helper function to process each year

process_year <- function(file_path, year) {
  df <- read_xpt(file_path) %>%
    mutate(year = year)
  
  # Renaming PSU, STRATA and SurveyWeight
  if ("_PSU" %in% names(df)) {
    df <- df %>%
      rename(PSU = `_PSU`)
  }
  
  if ("_STSTR" %in% names(df)) {
    df <- df %>%
      rename(STRATA = `_STSTR`)
  }
  
  if ("_LLCPWT" %in% names(df)) {
    df <- df %>%
      rename(SurveyWeight = `_LLCPWT`)
  }
  
  # Recode FMD from MENTHLTH
  if ("MENTHLTH" %in% names(df)) {
    df <- df %>%
      mutate(MENTHLTH = as.numeric(MENTHLTH),
             FMD = ifelse(MENTHLTH >= 14 & MENTHLTH <= 30, "Yes",
                          ifelse(MENTHLTH >= 0 & MENTHLTH < 14, "No", NA)))
  }
  
  # Binge drinking (year-dependent)
  binge_var <- if (year <= 2021) "_RFBING5" else "_RFBING6"
  if (binge_var %in% names(df)) {
    df <- df %>%
      mutate(BingeRaw = as.numeric(.data[[binge_var]]),
             BingeDrinker = ifelse(BingeRaw == 2, "Yes",
                                   ifelse(BingeRaw == 1, "No", NA))) %>%
      select(-BingeRaw)
  }
  
  # Race/Ethnicity
  race_var <- if (year == 2022) "_RACEGR4" else "_RACEGR3"
  if (race_var %in% names(df)) {
    df <- df %>%
      mutate(RaceRaw = as.numeric(.data[[race_var]]),
             RaceEthnicity = case_when(
               RaceRaw == 1 ~ "Non-Hispanic White",
               RaceRaw == 2 ~ "Non-Hispanic Black",
               RaceRaw == 3 ~ "Non-Hispanic Other",
               RaceRaw == 4 ~ "Non-Hispanic Multiracial",
               RaceRaw == 5 ~ "Hispanic",
               TRUE ~ NA_character_)) %>%
      select(-RaceRaw)
  }
  
  # Sex/Gender
  sex_var <- if (year %in% 2013:2017) "SEX" else if (year == 2018) "SEX1" else "_SEX"
  if (sex_var %in% names(df)) {
    df <- df %>%
      mutate(SexRaw = as.numeric(.data[[sex_var]]),
             Male = ifelse(SexRaw == 1, "Male",
                           ifelse(SexRaw == 2, "Female", NA))) %>%
      select(-SexRaw)
  }
  
  # AgeGroup
  if ("_AGEG5YR" %in% names(df)) {
    df <- df %>%
      mutate(AgeGroup = {
        ag <- as.numeric(`_AGEG5YR`)
        ifelse(is.na(ag) | ag == 14, NA_character_,
               ifelse(ag == 1, "18-24",
                      ifelse(ag == 2, "25-29",
                             ifelse(ag %in% 3:4, "30-39",
                                    ifelse(ag %in% 5:6, "40-49",
                                           ifelse(ag %in% 7:8, "50-59",
                                                  ifelse(ag %in% 9:10, "60-69",
                                                         ifelse(ag %in% 11:12, "70-79",
                                                                ifelse(ag == 13, "80+",
                                                                       NA_character_)))))))))
      })
  }
  
  # Income
  income_var <- if (year %in% 2013:2020) "INCOME2" else "INCOME3"
  if (income_var %in% names(df)) {
    df <- df %>%
      mutate(IncomeRaw = as.numeric(.data[[income_var]]),
             Income = ifelse(is.na(IncomeRaw), NA_character_,
                             ifelse(IncomeRaw %in% 1:2, "<$15k",
                                    ifelse(IncomeRaw %in% 3:4, "$15k-$25k",
                                           ifelse(IncomeRaw == 5, "$25k-$35k",
                                                  ifelse(IncomeRaw == 6, "$35k-$50k",
                                                         ifelse(IncomeRaw == 7, "$50k-$75k",
                                                                ifelse(IncomeRaw %in% 8:11, "$75k+",
                                                                       NA_character_))))))) ) %>%
      select(-IncomeRaw)
  }
  
  # Education
  if ("_EDUCAG" %in% names(df)) {
    df <- df %>%
      mutate(Education = {
        edu <- as.numeric(`_EDUCAG`)
        ifelse(is.na(edu) | edu == 9, NA_character_,
               ifelse(edu == 1, "Less than HS",
                      ifelse(edu == 2, "HS Graduate",
                             ifelse(edu == 3, "Attended College",
                                    ifelse(edu == 4, "College Graduate",
                                           NA_character_)))))
      })
  }
  # Employment
  if ("EMPLOY1" %in% names(df)) {
    df <- df %>%
      mutate(Employment = {
        emp <- as.numeric(EMPLOY1)
        ifelse(is.na(emp) | emp == 9, NA_character_,
               ifelse(emp %in% 1:2, "Employed",
                      ifelse(emp == 3, "Not in Labor Force",
                             ifelse(emp == 4, "Unemployed",
                                    ifelse(emp %in% 5:8, "Not in Labor Force",
                                           NA_character_)))))
      })
  }
  
  # MaritalStatus
  if ("MARITAL" %in% names(df)) {
    df <- df %>%
      mutate(MaritalStatus = {
        m <- as.numeric(MARITAL)
        ifelse(is.na(m) | m == 9, NA_character_,
               ifelse(m == 1, "Married",
                      ifelse(m %in% 2:4, "Marital Dissolution",
                             ifelse(m == 5, "Never Married",
                                    ifelse(m == 6, "Cohabiting",
                                           NA_character_)))))
      })
  }
  
  # Insurance
  insurance_var <- if (year %in% 2013:2020) "HLTHPLN1" else if (year %in% 2021:2022) "_HLTHPLN" else "_HLTHPL1"
  if (insurance_var %in% names(df)) {
    df <- df %>%
      mutate(InsuranceRaw = as.numeric(.data[[insurance_var]]),
             Insurance = ifelse(is.na(InsuranceRaw), NA,
                                ifelse(InsuranceRaw == 1, "Yes",
                                       ifelse(InsuranceRaw == 2, "No", NA)))) %>%
      select(-InsuranceRaw)
  }
  
  # HomeOwnership
  if ("RENTHOM1" %in% names(df)) {
    df <- df %>%
      mutate(HomeOwnership = {
        h <- as.numeric(RENTHOM1)
        ifelse(is.na(h) | h %in% 7:9, NA_character_,
               ifelse(h == 1, "Own",
                      ifelse(h == 2, "Rent",
                             ifelse(h == 3, "Other",
                                    NA_character_))))
      })
  }
  
  # SmokingStatus
  if ("_SMOKER3" %in% names(df)) {
    df <- df %>%
      mutate(SmokingStatus = {
        s <- as.numeric(`_SMOKER3`)
        ifelse(is.na(s) | s == 9, NA_character_,
               ifelse(s %in% 1:3, "Smoker",
                      ifelse(s == 4, "Non-Smoker",
                             NA_character_)))
      })
  }
  
  # HeavyDrinker
  heavy_var <- if (year %in% 2013:2014) "_RFDRHV4"
  else if (year %in% 2015:2017) "_RFDRHV5"
  else if (year == 2018) "_RFDRHV6"
  else if (year %in% 2019:2021) "_RFDRHV7"
  else "_RFDRHV8"
  
  if (heavy_var %in% names(df)) {
    df <- df %>%
      mutate(HeavyRaw = as.numeric(.data[[heavy_var]]),
             HeavyDrinker = ifelse(is.na(HeavyRaw) | HeavyRaw == 9, NA_character_,
                                   ifelse(HeavyRaw == 1, "No",
                                          ifelse(HeavyRaw == 2, "Yes", NA_character_)))) %>%
      select(-HeavyRaw)
  }
  
  
  # PhysicallyActive
  if ("_TOTINDA" %in% names(df)) {
    df <- df %>%
      mutate(PhysicallyActive = {
        p <- as.numeric(`_TOTINDA`)
        ifelse(is.na(p) | p == 9, NA_character_,
               ifelse(p == 1, "Yes",
                      ifelse(p == 2, "No",
                             NA_character_)))
      })
  }
  
  # BMICategory
  if ("_BMI5CAT" %in% names(df)) {
    df <- df %>%
      mutate(BMICategory = {
        b <- as.numeric(`_BMI5CAT`)
        ifelse(is.na(b) | b == 9, NA_character_,
               ifelse(b %in% 1:2, "Normal Weight",
                      ifelse(b == 3, "Overweight",
                             ifelse(b == 4, "Obese",
                                    NA_character_))))
      })
  }
  
  # Asthma
  if ("ASTHMA3" %in% names(df)) {
    df <- df %>%
      mutate(Asthma = {
        a <- as.numeric(ASTHMA3)
        ifelse(is.na(a) | a %in% 7:9, NA_character_,
               ifelse(a == 1, "Yes",
                      ifelse(a == 2, "No",
                             NA_character_)))
      })
  }
  
  # Arthritis
  arthritis_var <- if (year %in% 2013:2018) "HAVARTH3"
  else if (year %in% c(2019, 2020, 2022, 2023)) "HAVARTH4"
  else "HAVARTH5"
  
  if (arthritis_var %in% names(df)) {
    df <- df %>%
      mutate(ArthritisRaw = as.numeric(.data[[arthritis_var]]),
             Arthritis = ifelse(is.na(ArthritisRaw) | ArthritisRaw %in% 7:9, NA_character_,
                                ifelse(ArthritisRaw == 1, "Yes",
                                       ifelse(ArthritisRaw == 2, "No", NA_character_)))) %>%
      select(-ArthritisRaw)
  }
  
  # COPD
  copd_var <- if (year %in% 2013:2018) "CHCCOPD1"
  else if (year %in% 2019:2020) "CHCCOPD2"
  else "CHCCOPD3"
  
  if (copd_var %in% names(df)) {
    df <- df %>%
      mutate(COPDRaw = as.numeric(.data[[copd_var]]),
             COPD = ifelse(is.na(COPDRaw) | COPDRaw %in% 7:9, NA_character_,
                           ifelse(COPDRaw == 1, "Yes",
                                  ifelse(COPDRaw == 2, "No", NA_character_)))) %>%
      select(-COPDRaw)
  }
  
  # CHD
  if ("CVDCRHD4" %in% names(df)) {
    df <- df %>%
      mutate(CHD = {
        c <- as.numeric(CVDCRHD4)
        ifelse(is.na(c) | c %in% 7:9, NA_character_,
               ifelse(c == 1, "Yes",
                      ifelse(c == 2, "No",
                             NA_character_)))
      })
  }
  
  # Diabetes
  diabetes_var <- if (year %in% 2013:2018) "DIABETE3" else "DIABETE4"
  
  if (diabetes_var %in% names(df)) {
    df <- df %>%
      mutate(DiabetesRaw = as.numeric(.data[[diabetes_var]]),
             Diabetes = ifelse(is.na(DiabetesRaw) | DiabetesRaw %in% 7:9, NA_character_,
                               ifelse(DiabetesRaw == 1, "Yes",
                                      ifelse(DiabetesRaw %in% 2:4, "No",
                                             NA_character_)))) %>%
      select(-DiabetesRaw)
  }
  
  # HighBP
  #    if (year %in% c(2013, 2015, 2017, 2019) && "BPHIGH4" %in% names(df)) {
  #  df <- df %>%
  #    mutate(HighBPRaw = as.numeric(BPHIGH4),
  #           HighBP = ifelse(is.na(HighBPRaw) | HighBPRaw %in% 7:9, NA_character_,
  #                    ifelse(HighBPRaw == 1, "Yes",
  #                    ifelse(HighBPRaw %in% 2:4, "No", NA_character_)))) %>%
  #    select(-HighBPRaw)
  #} else if (year %in% c(2021, 2023) && "BPHIGH6" %in% names(df)) {
  #  df <- df %>%
  #    mutate(HighBPRaw = as.numeric(BPHIGH6),
  #           HighBP = ifelse(is.na(HighBPRaw) | HighBPRaw %in% 7:9, NA_character_,
  #                    ifelse(HighBPRaw == 1, "Yes",
  #                    ifelse(HighBPRaw %in% 2:4, "No", NA_character_)))) %>%
  #    select(-HighBPRaw)
  #}
  
  # KidneyDisease
  kidney_var <- if (year %in% 2013:2017) "CHCKIDNY"
  else if (year == 2018) "CHCKDNY1"
  else "CHCKDNY2"
  
  if (kidney_var %in% names(df)) {
    df <- df %>%
      mutate(KidneyRaw = as.numeric(.data[[kidney_var]]),
             KidneyDisease = ifelse(is.na(KidneyRaw) | KidneyRaw %in% 7:9, NA_character_,
                                    ifelse(KidneyRaw == 1, "Yes",
                                           ifelse(KidneyRaw == 2, "No", NA_character_)))) %>%
      select(-KidneyRaw)
  }
  
  # Stroke
  if ("CVDSTRK3" %in% names(df)) {
    df <- df %>%
      mutate(Stroke = {
        s <- as.numeric(CVDSTRK3)
        ifelse(is.na(s) | s %in% 7:9, NA_character_,
               ifelse(s == 1, "Yes",
                      ifelse(s == 2, "No", NA_character_)))
      })
  }
  
  # HighCholesterol
  #if (year %in% c(2013, 2015, 2017, 2019) && "TOLDHI2" %in% names(df)) {
  #df <- df %>%
  #  mutate(HighCholRaw = as.numeric(TOLDHI2),
  #         HighCholesterol = ifelse(is.na(HighCholRaw) | HighCholRaw %in% 7:9, NA_character_,
  #                           ifelse(HighCholRaw == 1, "Yes",
  #                           ifelse(HighCholRaw == 2, "No", NA_character_)))) %>%
  #  select(-HighCholRaw)
  #} else if (year %in% c(2021, 2023) && "TOLDHI3" %in% names(df)) {
  #  df <- df %>%
  #    mutate(HighCholRaw = as.numeric(TOLDHI3),
  #           HighCholesterol = ifelse(is.na(HighCholRaw) | HighCholRaw %in% 7:9, NA_character_,
  #                             ifelse(HighCholRaw == 1, "Yes",
  #                             ifelse(HighCholRaw == 2, "No", NA_character_)))) %>%
  #    select(-HighCholRaw)
  #}
  
  # Disability derived from multiple variables
  vars_early <- c("BLIND", "DECIDE", "DIFFWALK", "DIFFDRES", "DIFFALON")
  vars_late  <- c("DEAF", vars_early)
  
  # Check if all required variables are present for this year
  has_all_vars <- function(var_list) all(var_list %in% names(df))
  
  if (year %in% 2013:2015 && has_all_vars(vars_early)) {
    df <- df %>%
      mutate(Disability = {
        v <- df[, vars_early]
        # Convert all to numeric
        v[] <- lapply(v, as.numeric)
        
        any_yes <- apply(v, 1, function(row) any(row == 1, na.rm = TRUE))
        all_no  <- apply(v, 1, function(row) all(row == 2, na.rm = TRUE))
        
        ifelse(any_yes, "Yes",
               ifelse(all_no, "No", NA_character_))
      })
  }
  
  if (year >= 2016 && has_all_vars(vars_late)) {
    df <- df %>%
      mutate(Disability = {
        v <- df[, vars_late]
        v[] <- lapply(v, as.numeric)
        
        any_yes <- apply(v, 1, function(row) any(row == 1, na.rm = TRUE))
        all_no  <- apply(v, 1, function(row) all(row == 2, na.rm = TRUE))
        
        ifelse(any_yes, "Yes",
               ifelse(all_no, "No", NA_character_))
      })
  }
  
  # Minimal final selection for testing (can be expanded)
  df %>%
    select(any_of(c(
      "year", "FMD", "BingeDrinker", "RaceEthnicity", "Male",
      "AgeGroup", "Income", "Education", "Employment",
      "MaritalStatus", "Insurance", "HomeOwnership", "SmokingStatus",
      "HeavyDrinker", "PhysicallyActive", "BMICategory", "Asthma",
      "Arthritis", "COPD", "CHD", "Diabetes",
      "KidneyDisease", "Stroke", "Disability", "PSU",
      "STRATA", "SurveyWeight"
    )))
  
}

# Process all years and merge into one data frame without saving individual files
all_data <- map2_dfr(file_paths, years, process_year)

# ----------------------------- #
# 4. Data Pre-processing 
# ----------------------------- #

brfss_merged <- all_data

factor_vars <- c(
  "year","FMD", "BingeDrinker", "RaceEthnicity", "Male",
  "AgeGroup", "Income", "Education", "Employment",
  "MaritalStatus", "Insurance", "HomeOwnership", "SmokingStatus",
  "HeavyDrinker", "PhysicallyActive", "BMICategory", "Asthma",
  "Arthritis", "COPD", "CHD", "Diabetes",
  "KidneyDisease", "Stroke", "Disability"
)

# Convert those columns to factors
brfss_merged <- brfss_merged %>%
  mutate(across(all_of(factor_vars), as.factor))

# Removing null rows
brfss_merged2 <- brfss_merged %>%filter(complete.cases(.)) 

# Deriving the MCC variable using the 9 chronic condition variables (Reference: Price et al)
# List of recoded chronic condition variable names
chronic_conditions <- c("Asthma", "Arthritis", "COPD", "CHD", "Diabetes",
                        "KidneyDisease", "Stroke")

brfss_merged2$MCC <- apply(brfss_merged2[, chronic_conditions], 1, function(row) { 
  yes_count <- sum(row == "Yes", na.rm = TRUE) #Counting the number of "Yes" values in the entire row
  
  if (yes_count >= 2) {
    return("Yes") # Returns MCC - True when the yes values are greater than or equal to 2
  } else {
    return("No") # Returns MCC - False when the yes values are less than 2
  }
})
brfss_merged2$MCC <- factor(brfss_merged2$MCC)

# Explicitly setting reference levels for each variable

# Race/Ethnicity → Reference: Hispanic
brfss_merged2$RaceEthnicity <- relevel(brfss_merged2$RaceEthnicity, ref = "Hispanic")

# Education → Reference: College Graduate
brfss_merged2$Education <- relevel(brfss_merged2$Education, ref = "College Graduate")

# Income → Reference: $75k+ 
brfss_merged2$Income <- relevel(brfss_merged2$Income, ref = "$75k+")

# Marital Status → Reference: Married
brfss_merged2$MaritalStatus <- relevel(brfss_merged2$MaritalStatus, ref = "Married")

# Insurance → Reference: Yes (Insured)
brfss_merged2$Insurance <- relevel(brfss_merged2$Insurance, ref = "Yes")

# Employment → Reference: Employed
brfss_merged2$Employment <- relevel(brfss_merged2$Employment, ref = "Employed")

# Age Group → Reference: 18–24
brfss_merged2$AgeGroup <- relevel(brfss_merged2$AgeGroup, ref = "18-24")

# Gender (Male Binary) → Reference: 0 (Female)
brfss_merged2$Male <- relevel(brfss_merged2$Male, ref = "Female")

# Binge Drinker → Reference: 0 (Non-Binge Drinker)
brfss_merged2$BingeDrinker <- relevel(brfss_merged2$BingeDrinker, ref = "No")

# FMD → Reference: 0 (No Frequent Mental Distress)
brfss_merged2$FMD <- relevel(brfss_merged2$FMD, ref = "No")

# Smoking Status → Reference: Non-Smoker
brfss_merged2$SmokingStatus <- relevel(brfss_merged2$SmokingStatus, ref = "Non-Smoker")

# Heavy Alcohol Use → Reference: No
brfss_merged2$HeavyDrinker <- relevel(brfss_merged2$HeavyDrinker, ref = "No")

# Physical Activity → Reference: Yes (Active)
brfss_merged2$PhysicallyActive <- relevel(brfss_merged2$PhysicallyActive, ref = "Yes")

# BMI Category → Reference: Normal Weight
brfss_merged2$BMICategory <- relevel(brfss_merged2$BMICategory, ref = "Normal Weight")

# Home Ownership → Reference: Own
brfss_merged2$HomeOwnership <- relevel(brfss_merged2$HomeOwnership, ref = "Own")

# Chronic Conditions → Reference: No
brfss_merged2$Asthma <- relevel(brfss_merged2$Asthma, ref = "No")
brfss_merged2$Arthritis <- relevel(brfss_merged2$Arthritis, ref = "No")
brfss_merged2$COPD <- relevel(brfss_merged2$COPD, ref = "No")
brfss_merged2$CHD <- relevel(brfss_merged2$CHD, ref = "No")
brfss_merged2$Diabetes <- relevel(brfss_merged2$Diabetes, ref = "No")
brfss_merged2$KidneyDisease <- relevel(brfss_merged2$KidneyDisease, ref = "No")
brfss_merged2$Stroke <- relevel(brfss_merged2$Stroke, ref = "No")

# Disability → Reference: No
brfss_merged2$Disability <- relevel(brfss_merged2$Disability, ref = "No")

# MCC → Reference: No
brfss_merged2$MCC <- relevel(brfss_merged2$MCC, ref = "No")

# Cleaning names of the variables
names(brfss_merged2) <- c("year", "fmd", "binge_drinker", "race_ethnicity", "gender", "age_group", "income", "education", "employment", 
                          "marital_status", "insurance", "home_ownership", "smoking_status", "heavy_drinker", "physically_active", 
                          "bmi_category", "asthma", "arthritis", "copd", "chd", "diabetes", "kidney_disease", "stroke", "disability",
                          "psu","strata", "survey_weight", "mcc"
)

# Saving the final analytic sample in .rda
save(brfss_merged2, file = "brfss_merged_data/final_analytic_sample_2013_2019.rda")
# Binge Drinking and Mental Distress Across Racial/Ethnic Groups

This repository contains the analysis conducted for my research project on the association between binge drinking and frequent mental distress (FMD) using pooled BRFSS data from 2013–2019.

The study investigates whether the relationship between binge drinking and mental distress changes after accounting for socio-demographic, socioeconomic, lifestyle, and health condition covariates. It also evaluates how the observed association evolves through a stepwise modeling approach.

## Repository Structure
- data_processing.R
  - Reads in BRFSS annual data files (2013–2019).
  - Harmonizes variable names across years.
  - Creates key outcome (FMD), exposure (BingeDrinker), and covariates.
  - Prepares a pooled dataset ready for analysis.

- stepwise_modeling.R
  - Defines the BRFSS survey design object (weights, strata, PSU).
  - Implements nested logistic regression models (svyglm) in chunks:
    1. Base model (FMD ~ BingeDrinker)
    2. Add Socio-demographic variables
    3. Add Socioeconomic factors
    4. Add Health behaviors
    5. Add Smoking status
    6. Add Chronic health conditions + year
  - Includes multicollinearity diagnostics (Cramér’s V, GVIF).
  - Generates tables and model fit comparisons (AIC, pseudo R²).
 
## Data
The analysis uses publicly available BRFSS annual survey data (2013–2019), which can be downloaded from the CDC website:
- BRFSS Overview: https://www.cdc.gov/brfss/
- Annual data files: https://www.cdc.gov/brfss/smart/Smart_data.htm

## Requirements
- This project is written in R and relies on the packages present in requirements.txt file.
- Those packages can be downloaded using install.packages()

## Usage
1. Clone this repository: git clone https://github.com/Komalb07/brfss-binge-fmd.git
2. Open R or RStudio and set the working directory to the repo.
3. Run the notebooks in order:
   - Start with data_processing.R to prepare the dataset.
   - Then run stepwise_modeling.R to fit the nested models and produce results.

## Notes
- Ensure that the raw BRFSS .XPT files for 2013–2019 are downloaded and accessible to the data preparation script.
- The analysis uses survey weights; results are representative at the population level.
- This project was conducted as part of a research collaboration exploring racial/ethnic disparities in alcohol-related mental health outcomes.

## License
This project is licensed under the MIT License. See the LICENSE file for details.


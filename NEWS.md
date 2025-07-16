# admiralmetabolic 0.2.0

## New features

- New vignette on "Creating a Metabolic ADLB ADaM". (#102)
- New ADLB template (`ad_adlb.R`) for creating metabolic specific ADLB dataset. (#108)
- A SDTM dataset for metabolic specific laboratory measurements (`lb_metabolic`) in {pharmaversesdtm}. (#94)
- A ADSL dataset for metabolic specific variables (`adsl_metabolic`). (#73 & #116)

## Documentation

- Updated installation instructions to install from CRAN. (#88)
- Changed vignettes to use `admiralmetabolic::admiralmetabolic_adsl` instead of `admiral::admiral_adsl`. (#123)

## Template programs

- Changed template programs to use `admiralmetabolic::admiralmetabolic_adsl` instead of `admiral::admiral_adsl`. (#123)

## Various

- Moved test data `vs_metabolic`, `qs_metabolic` and `dm_metabolic` from {admiralmetabolic} to {pharmaversesdtm}. (#95)

<details>
<summary>Developer Notes</summary>

- Activated automatic version bumping CICD workflow. (#98)

- Added pharmaverse, CRAN and Test Coverage badges to home page. (#97)

- Added initial package scope to home page. (#101)

- Added `advs` as a dataset. (#113)

- Updated contributor list. (#122)

</details>

# admiralmetabolic 0.1.0

- Initial package release mainly focused on obesity therapeutic area.

## Functions

- The function `derive_param_waisthip()` for deriving Waist to Hip Ratio. (#33 &  #52)
- The function `derive_param_waisthgt()` for deriving Waist to Height Ratio. (#33 & #52)

## Template programs

- ADVS template scripts `ad_advs.R` which creates a Vital Sign Analysis Dataset with metabolic specific derivations. (#35)
- ADCOEQ template scripts `ad_adcoeq.R` which creates a Control of Eating Questionnaires Analysis Dataset. (#41)

## Documentation

- Vignette "Creating a Metabolic ADVS ADaM". (#28) 
- Vignette "Creating a Control of Eating Questionnaire ADaM". (#53)

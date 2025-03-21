# admiralmetabolic (development version)

# New features

<details>
<summary>Developer Notes</summary>

- Activated automatic version bumping CICD workflow. (#98)

- Removed `vs_metabolic`, `qs_metabolic` and `dm_metabolic` test data from the package and referenced them from the {pharmaversesdtm} package. (#95)

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

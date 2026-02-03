# Changelog

## admiralmetabolic 0.3.0

CRAN release: 2026-02-02

### Bug fixes

- Changed template save directory.
  ([\#129](https://github.com/pharmaverse/admiralmetabolic/issues/129))

### Documentation

- A link to the [{admiral}
  ecosystem](https://pharmaverse.org/e2eclinical/adam/) page was added
  to the README sidebar and main text.
  ([\#136](https://github.com/pharmaverse/admiralmetabolic/issues/136))
- The “Ask AI” widget was added to the bottom right of each page. It
  enables users to ask questions about
  [admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/)
  and the rest of the [admiral](https://pharmaverse.github.io/admiral/)
  ecosystem and receive answers from an LLM. It is trained on the
  documentation of all [admiral](https://pharmaverse.github.io/admiral/)
  packages and provided by
  [kapa.ai](https://docs.kapa.ai/kapa-for-open-source).
  ([\#137](https://github.com/pharmaverse/admiralmetabolic/issues/137))

### Various

Developer Notes

- Updated [lintr](https://lintr.r-lib.org) configurations to use central
  configurations from
  [admiraldev](https://pharmaverse.github.io/admiraldev/).
  ([\#133](https://github.com/pharmaverse/admiralmetabolic/issues/133))
- Fixes CRAN issue related to usage of relative paths.
  ([\#138](https://github.com/pharmaverse/admiralmetabolic/issues/138))

## admiralmetabolic 0.2.0

CRAN release: 2025-07-16

### New features

- New vignette on “Creating a Metabolic ADLB ADaM”.
  ([\#102](https://github.com/pharmaverse/admiralmetabolic/issues/102))
- New ADLB template (`ad_adlb.R`) for creating metabolic specific ADLB
  dataset.
  ([\#108](https://github.com/pharmaverse/admiralmetabolic/issues/108))
- A SDTM dataset for metabolic specific laboratory measurements
  (`lb_metabolic`) in {pharmaversesdtm}.
  ([\#94](https://github.com/pharmaverse/admiralmetabolic/issues/94))
- A ADSL dataset for metabolic specific variables (`adsl_metabolic`).
  ([\#73](https://github.com/pharmaverse/admiralmetabolic/issues/73) &
  [\#116](https://github.com/pharmaverse/admiralmetabolic/issues/116))

### Documentation

- Updated installation instructions to install from CRAN.
  ([\#88](https://github.com/pharmaverse/admiralmetabolic/issues/88))
- Changed vignettes to use
  [`admiralmetabolic::admiralmetabolic_adsl`](https://pharmaverse.github.io/admiralmetabolic/reference/admiralmetabolic_adsl.md)
  instead of
  [`admiral::admiral_adsl`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/admiral_adsl.html).
  ([\#123](https://github.com/pharmaverse/admiralmetabolic/issues/123))

### Template programs

- Changed template programs to use
  [`admiralmetabolic::admiralmetabolic_adsl`](https://pharmaverse.github.io/admiralmetabolic/reference/admiralmetabolic_adsl.md)
  instead of
  [`admiral::admiral_adsl`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/admiral_adsl.html).
  ([\#123](https://github.com/pharmaverse/admiralmetabolic/issues/123))

### Various

- Moved test data `vs_metabolic`, `qs_metabolic` and `dm_metabolic` from
  {admiralmetabolic} to {pharmaversesdtm}.
  ([\#95](https://github.com/pharmaverse/admiralmetabolic/issues/95))

Developer Notes

- Activated automatic version bumping CICD workflow.
  ([\#98](https://github.com/pharmaverse/admiralmetabolic/issues/98))

- Added pharmaverse, CRAN and Test Coverage badges to home page.
  ([\#97](https://github.com/pharmaverse/admiralmetabolic/issues/97))

- Added initial package scope to home page.
  ([\#101](https://github.com/pharmaverse/admiralmetabolic/issues/101))

- Added `advs` as a dataset.
  ([\#113](https://github.com/pharmaverse/admiralmetabolic/issues/113))

- Updated contributor list.
  ([\#122](https://github.com/pharmaverse/admiralmetabolic/issues/122))

## admiralmetabolic 0.1.0

CRAN release: 2025-01-20

- Initial package release mainly focused on obesity therapeutic area.

### Functions

- The function
  [`derive_param_waisthip()`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthip.md)
  for deriving Waist to Hip Ratio.
  ([\#33](https://github.com/pharmaverse/admiralmetabolic/issues/33) &
  [\#52](https://github.com/pharmaverse/admiralmetabolic/issues/52))
- The function
  [`derive_param_waisthgt()`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthgt.md)
  for deriving Waist to Height Ratio.
  ([\#33](https://github.com/pharmaverse/admiralmetabolic/issues/33) &
  [\#52](https://github.com/pharmaverse/admiralmetabolic/issues/52))

### Template programs

- ADVS template scripts `ad_advs.R` which creates a Vital Sign Analysis
  Dataset with metabolic specific derivations.
  ([\#35](https://github.com/pharmaverse/admiralmetabolic/issues/35))
- ADCOEQ template scripts `ad_adcoeq.R` which creates a Control of
  Eating Questionnaires Analysis Dataset.
  ([\#41](https://github.com/pharmaverse/admiralmetabolic/issues/41))

### Documentation

- Vignette “Creating a Metabolic ADVS ADaM”.
  ([\#28](https://github.com/pharmaverse/admiralmetabolic/issues/28))
- Vignette “Creating a Control of Eating Questionnaire ADaM”.
  ([\#53](https://github.com/pharmaverse/admiralmetabolic/issues/53))

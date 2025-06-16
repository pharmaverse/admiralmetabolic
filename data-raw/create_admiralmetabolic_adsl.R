#  Create dataset:   data/admiralmetabolic_adsl
#  This script:  create_admiralmetabolic_adsl.R creates dataset data/admiralmetabolic_adsl.rda.

# Run template script to create advs
source("inst/templates/ad_adsl_metabolic.R", echo = TRUE) # nolint

# Set object
admiralmetabolic_adsl <- adsl

# Get previous dataset for comparison
adsl_old <- admiralmetabolic::admiralmetabolic_adsl

# Finally, save reduced dataset
usethis::use_data(admiralmetabolic_adsl, overwrite = TRUE)

# Compare with previous version
diffdf::diffdf(
  base = admiralmetabolic_adsl,
  compare = adsl_old,
  keys = c("STUDYID", "USUBJID")
)

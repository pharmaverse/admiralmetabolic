#  Create dataset:   data/admiralmetabolic_advs.rda
#  This script:  create_admiralmetabolic_advs.R creates dataset data/admiralmetabolic_advs.rda.

# Run template script to create advs
source("inst/templates/ad_advs.R", echo = TRUE) # nolint

# Set object
admiralmetabolic_advs <- advs

# Get previous dataset for comparison
advs_old <- admiral::admiralmetabolic_advs

# Finally, save reduced dataset
usethis::use_data(admiralmetabolic_advs, overwrite = TRUE)

# Compare with previous version
diffdf::diffdf(
  base = admiralmetabolic_advs,
  compare = advs_old,
  keys = c("USUBJID", "PARAMCD", "AVISIT", "ADT")
)

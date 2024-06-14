# Name: ADXX
#
# Label: XXX
#
# Input: xx, xx, xx

library(admiral)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project

# Add your template ADaM script code

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiraltemplate_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adxx, file = file.path(dir, "adxx.rda"), compress = "bzip2")

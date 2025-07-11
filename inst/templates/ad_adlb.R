# Name: ADLB
#
# Label: Laboratory Analysis dataset for metabolic trials
#
# Description: This template focuses on metabolic specific derivations. For additional
# often found in `ADLB` view the `admiral` template by running `admiral::use_ad_template("adlb")`.
#
# Input: lb_metabolic, admiralmetabolic_adsl, admiralmetabolic_advs

# Attach/load required packages ----
library(admiral)
library(admiralmetabolic)
library(pharmaversesdtm)
library(dplyr)
library(tibble)
library(stringr)

# Define project options/variables ----
# Use the admiral option functionality to store subject key variables in one
# place (note: `subject_keys` defaults to STUDYID and USUBJID)
set_admiral_options(subject_keys = exprs(STUDYID, USUBJID))

# Store ADSL variables required for derivations as an R object, enabling
# simplified usage throughout the program
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

# Read in and prepare data ----
# See the "Read in Data" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#readdata)
lb_metabolic <- pharmaversesdtm::lb_metabolic
admiralmetabolic_adsl <- admiralmetabolic::admiralmetabolic_adsl
admiralmetabolic_advs <- admiralmetabolic::admiralmetabolic_advs

# Convert SAS missing character values to NA
adlb <- lb_metabolic %>%
  convert_blanks_to_na()
adsl <- admiralmetabolic_adsl %>%
  convert_blanks_to_na()
advs <- admiralmetabolic_advs %>%
  convert_blanks_to_na()

# Remove glucose and insulin measurements collected in an un-fasted state
adlb <- adlb %>%
  filter(!(LBTESTCD %in% c("GLUC", "INSULIN") & LBFAST != "Y"))

# Merge ADSL variables (stored in `adsl_vars`) needed for ADLB
adlb <- adlb %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

# Add parameter information ----
# Define parameter look-up table used for merging parameter codes to ADLB
param_lookup <- tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "ALB", "ALB", "Albumin (g/L)", 1,
  "ALP", "ALKPH", "Alkaline Phosphatase (U/L)", 2,
  "AST", "AST", "Aspartate Aminotransferase (U/L)", 3,
  "CHOL", "CHOLES", "Cholesterol (mmol/L)", 4,
  "GGT", "GGT", "Gamma Glutamyl Transferase (U/L)", 5,
  "GLUC", "GLUC", "Glucose (mmol/L)", 6,
  "HBA1CHGB", "HBA1CHGB", "Hemoglobin A1C/Hemoglobin (mmol/mol)", 7,
  "INSULIN", "INSULIN", "Insulin (mIU/L)", 8,
  "TRIG", "TRIG", "Triglycerides (mg/dL)", 9
)

# Add parameter (PARAMCD, PARAM, PARAMN) info to enable later ADLB derivations.

# See function documentation for `derive_vars_merged_lookup()`:
# (https://pharmaverse.github.io/admiral/reference/derive_vars_merged_lookup.html)
adlb <- adlb %>%
  # Add PARAMCD PARAM and PARAMN - from parameter lookup table
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(LBTESTCD)
  )

# Derive Date/Time and Analysis Day ----
# See the "Derive/Impute Numeric Date/Time and Analysis Day" vignette section
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#datetime)

# Add vital sign analysis date (ADT) and treatment start date (TRTSDT)
adlb <- adlb %>%
  derive_vars_dt(new_vars_prefix = "A", dtc = LBDTC) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

# Derive visit info ----
# See the "Visit and Period Variables" vignette for more information:
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html)

# Derive analysis visit (AVISIT, AVISITN)
adlb <- adlb %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ "Baseline",
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = case_when(
      AVISIT == "Baseline" ~ 0,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )

# Derive results ----
# See the "Derive Results (AVAL, AVALC)" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#aval)

# Derive AVAL and AVALC and define parameter categories
adlb <- adlb %>%
  slice_derivation(
    derivation = mutate,
    args = params(
      PARCAT1 = LBCAT,
    ),
    # Handle specific parameters requiring conventional units (CV)
    derivation_slice(
      filter = LBTESTCD %in% c("TRIG", "INSULIN"),
      args = params(
        AVAL = as.numeric(LBORRES),
        AVALC = NA_character_,
        ANRLO = as.numeric(LBORNRLO),
        ANRHI = as.numeric(LBORNRHI),
        PARCAT2 = "CV"
      )
    ),
    # Handle other parameters using standard units (SI)
    derivation_slice(
      filter = TRUE,
      args = params(
        AVAL = LBSTRESN,
        # Only populate AVALC if character value is non-redundant with AVAL
        AVALC = if_else(
          is.na(AVAL) | as.character(AVAL) != LBSTRESC,
          LBSTRESC,
          NA_character_
        ),
        ANRLO = LBSTNRLO,
        ANRHI = LBSTNRHI,
        PARCAT2 = "SI"
      )
    )
  )

# Derive domain specific parameters ----
# See the "Derive Additional Parameters" vignette section for general
# information about domain specific parameters:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#derive_param)
# See the "Creating a Metabolic ADLB ADaM" vignette for specific examples of
# metabolic ADLB parameters
# (https://pharmaverse.github.io/admiralmetabolic/dev/articles/adlb.html)'

# Merge BMI and WSTCIR (needed for later derivations) from ADVS to ADLB
adlb <- adlb %>%
  derive_vars_transposed(
    advs,
    by_vars = exprs(!!!get_admiral_option("subject_keys"), ADT),
    key_var = PARAMCD,
    value_var = AVAL,
    filter = PARAMCD %in% c("BMI", "WSTCIR")
  )

# Derive HOMA-IR
adlb <- adlb %>%
  derive_param_computed(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), AVISIT, AVISITN, ADT, ADY, !!!adsl_vars),
    parameters = c("INSULIN", "GLUC"),
    set_values_to = exprs(
      AVAL = AVAL.INSULIN * AVAL.GLUC / 22.5,
      PARAMCD = "HOMAIR",
      PARAM = "Homeostasis Model Assessment - Insulin Resistance",
      PARAMN = 10
    )
  )

# Derive FLI score
adlb <- adlb %>%
  derive_param_computed(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), AVISIT, AVISITN, ADT, ADY, BMI, WSTCIR, !!!adsl_vars),
    parameters = c("TRIG", "GGT"),
    set_values_to = exprs(
      AVAL = {
        lambda <- 0.953 * log(AVAL.TRIG) + 0.139 * BMI + 0.718 * log(AVAL.GGT) + 0.053 * WSTCIR - 15.745
        (exp(lambda) / (1 + exp(lambda))) * 100
      },
      PARAMCD = "FLI",
      PARAM = "Fatty Liver Index",
      PARAMN = 11
    )
  )

# Add Labels and Attributes ----
# This process is usually based on one's metadata. As such, no specific example
# will be given. See the "Add Labels and Attributes" vignette section for
# description of several open source R packages which can be used to handle
# metadata.
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#attributes)

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiral_templates_data", which = "cache") # Cache
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adlb, file = file.path(dir, "adlb.rda"), compress = "bzip2")

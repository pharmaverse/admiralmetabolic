# Name: ADCOEQ
#
# Label: Control of Eating Questionnaires Analysis dataset
#
# Input: adsl, qs

# License ----

# Note that University of Leeds are the copyright holders of the Control of Eating
# Questionnaire (CoEQ) and the test data included within `{admiralmetabolic}` as
# well as the ADCOEQ code are for not-for-profit use only within `{admiralmetabolic}`
# and pharmaverse-related examples/documentation. Any persons or companies wanting
# to use the CoEQ should request a license to do so from the following
# [link](https://licensing.leeds.ac.uk/product/control-of-eating-questionnaire-coeq).

# Attach/load required packages ----
library(admiral)
library(admiralmetabolic)
library(pharmaversesdtm)
library(dplyr)
library(stringr)

# Define project options/variables ----
# Use the admiral option functionality to store subject key variables in one
# place (note: `subject_keys` defaults to STUDYID and USUBJID)
set_admiral_options(subject_keys = exprs(STUDYID, USUBJID))

# Store ADSL variables required for derivations as an R object, enabling
# simplified usage throughout the program
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01P, TRT01A)

# Read in data ----
# See the "Read in Data" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#readdata)

# Read data
qs_metabolic <- pharmaversesdtm::qs_metabolic
adsl <- admiralmetabolic::admiralmetabolic_adsl

# Convert SAS missing character values to NA
adcoeq <- qs_metabolic %>%
  convert_blanks_to_na()
adsl <- adsl %>%
  convert_blanks_to_na()

# Merge ADSL variables (stored in `adsl_vars`) needed for ADQS
adcoeq <- adcoeq %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

# Add analysis parameter variables
adcoeq <- adcoeq %>%
  mutate(
    PARAMCD = QSTESTCD,
    PARAM = QSTEST,
    PARAMN = as.numeric(str_extract(QSTESTCD, "\\d+")),
    PARCAT1 = QSCAT
  )

# Derive Date/Time and Analysis Day ----
# See the "Derive/Impute Numeric Date/Time and Analysis Day" vignette section
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#datetime)

# Add questionnaire analysis date (ADT) and treatment start date (TRTSDT)
adcoeq <- adcoeq %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = QSDTC
  ) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = exprs(ADT)
  )

# Derive visit info ----
# See the "Visit and Period Variables" vignette for more information:
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html)

# Derive analysis visit (AVISIT, AVISITN)
adcoeq <- adcoeq %>%
  mutate(
    AVISIT = case_when(
      is.na(VISIT) ~ NA_character_,
      str_detect(VISIT, "UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      TRUE ~ str_to_title(VISIT)
    ),
    AVISITN = case_when(
      AVISIT == "Baseline" ~ 0,
      str_detect(AVISIT, "Screen") ~ -1,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )

# Derive results ----
# See the "Derive Results (AVAL, AVALC)" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#aval)

# Derive analysis result (AVAL, AVALC)
adcoeq <- adcoeq %>%
  mutate(
    # Invert the original scores for COEQ item 6 as they indicate anxiety and
    # is used in calculating the subscale for "Positive Mood"
    AVAL = if_else(PARAMCD == "COEQ06", 100 - QSSTRESN, QSSTRESN),
    AVALC = if_else(PARAMCD == "COEQ20", QSORRES, NA_character_)
  )

# Derive summary records ----

# See the "Example 2 (Deriving a Summary Record)" vignette section for more
# information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#example-2-deriving-a-summary-record)

# For the Control of Eating Questionnaire, four subscales are derived.
# These subscales are derived as the mean across a subset of the
# various items/questions.

# The subscales are defined as follows:

# 1) Craving Control: Calculate mean of items 9, 10, 11, 12 and 19.

# 2) Craving for Sweet: Calculate mean of items 3, 13, 14 and 15.

# 3) Craving for Savoury: Calculate mean of items 4, 16, 17 and 18.

# 4) Positive Mood: Calculate mean of items 5, 7, 8 and 6 (reversed).

# Derive four subscales
adcoeq <- adcoeq %>%
  call_derivation(
    derivation = derive_summary_records,
    variable_params = list(
      params(
        filter_add = PARAMCD %in% c(
          "COEQ09", "COEQ10", "COEQ11", "COEQ12", "COEQ19"
        ) & AVISITN >= 0,
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRCO",
          PARAM = "COEQ - Craving Control",
          PARAMN = 22
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ03", "COEQ13", "COEQ14", "COEQ15") &
          AVISITN >= 0,
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRSW",
          PARAM = "COEQ - Craving for Sweet",
          PARAMN = 23
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ04", "COEQ16", "COEQ17", "COEQ18") &
          AVISITN >= 0,
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQCRSA",
          PARAM = "COEQ - Craving for Savoury",
          PARAMN = 24
        )
      ),
      params(
        filter_add = PARAMCD %in% c("COEQ05", "COEQ07", "COEQ08", "COEQ06") &
          AVISITN >= 0,
        set_values_to = exprs(
          AVAL = mean(AVAL, na.rm = TRUE),
          PARAMCD = "COEQPOMO",
          PARAM = "COEQ - Positive Mood",
          PARAMN = 25
        )
      )
    ),
    dataset_add = adcoeq,
    by_vars = exprs(
      STUDYID, USUBJID, AVISIT, AVISITN, ADT, ADY, PARCAT1,
      TRTSDT, TRTEDT, TRT01P, TRT01A
    )
  )

# Derive Baseline variables ----
# See the "Derive Baseline" and "Derive Change from Baseline " vignette sections
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#baseline)
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#bchange)

# Add baseline flag (ABLFL)
adcoeq <- adcoeq %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
      order = exprs(ADT, AVISITN),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  )

# Derive baseline analysis value (BASE)
adcoeq <- adcoeq %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVAL,
    new_var = BASE
  )

# Derive absolute (CHG) and relative (PCHG) change from baseline
adcoeq <- adcoeq %>%
  restrict_derivation(
    derivation = derive_var_chg,
    filter = AVISITN > 0
  ) %>%
  restrict_derivation(
    derivation = derive_var_pchg,
    filter = AVISITN > 0
  )

# Assign ASEQ ----
# See the "Assign ASEQ" vignette section for more information:
# https://pharmaverse.github.io/admiral/articles/bds_finding.html#aseq

# Calculate ASEQ
adcoeq <- adcoeq %>%
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMCD, ADT, AVISITN),
    check_type = "error"
  )

# Add ADSL variables ----
# See the "Add ADSL variables" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#adsl_vars)

# Add all ADSL variables besides TRTSDT, TRTEDT, TRT01P, TRT01A (stored in
# `adsl_vars`)
adcoeq <- adcoeq %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
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
save(adcoeq, file = file.path(dir, "adcoeq.rda"), compress = "bzip2")

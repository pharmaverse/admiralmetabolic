# Name: ADVS
#
# Label: Vital Signs Analysis dataset for metabolic trials
#
# Input: adsl, vs

# Attach/load required packages ----
library(admiral)
library(admiralmetabolic)
library(pharmaversesdtm)
library(tibble)
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
vs_metabolic <- pharmaversesdtm::vs_metabolic
adsl <- admiralmetabolic::admiralmetabolic_adsl

# Convert SAS missing character values to NA
advs <- vs_metabolic %>%
  convert_blanks_to_na()
adsl <- adsl %>%
  convert_blanks_to_na()

# Merge ADSL variables (stored in `adsl_vars`) needed for ADVS
advs <- advs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

# Define parameter look-up table used for merging parameter codes to ADVS
param_lookup <- tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN, ~PARCAT1, ~PARCAT1N,
  "HEIGHT", "HEIGHT", "Height (cm)", 1, "Anthropometric measurement", 1,
  "WEIGHT", "WEIGHT", "Weight (kg)", 2, "Anthropometric measurement", 1,
  "BMI", "BMI", "Body Mass Index (kg/m2)", 3, "Anthropometric measurement", 1,
  "HIPCIR", "HIPCIR", "Hip Circumference (cm)", 4, "Anthropometric measurement", 1,
  "WSTCIR", "WSTCIR", "Waist Circumference (cm)", 5, "Anthropometric measurement", 1,
  "", "WAISTHIP", "Waist to Hip Ratio", 6, "Anthropometric measurement", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 7, "Vital Sign", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 8, "Vital Sign", 2,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 9, "Vital Sign", 2,
  "TEMP", "TEMP", "Temperature (C)", 10, "Vital Sign", 2
)

# Add parameter (PARAMCD) info to enable later ADVS derivations. Additional
# parameter information will be merged again, after all AVDS derivations are
# completed.

# See function documentation for `derive_vars_merged_lookup()`:
# (https://pharmaverse.github.io/admiral/reference/derive_vars_merged_lookup.html)
advs <- advs %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD),
    by_vars = exprs(VSTESTCD)
  )

# Derive Date/Time and Analysis Day ----
# See the "Derive/Impute Numeric Date/Time and Analysis Day" vignette section
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#datetime)

# Add vital sign analysis date (ADT) and treatment start date (TRTSDT)
advs <- advs %>%
  derive_vars_dt(new_vars_prefix = "A", dtc = VSDTC) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

# Derive visit info ----
# See the "Visit and Period Variables" vignette for more information:
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html)

# Derive analysis time point (ATPT, ATPTN) and analysis visit (AVISIT, AVISITN)
advs <- advs %>%
  mutate(
    ATPT = VSTPT,
    ATPTN = VSTPTNUM,
    AVISIT = case_when(
      is.na(VISIT) ~ NA_character_,
      VSTESTCD == "HEIGHT" & VISIT == "SCREENING 1" ~ "Baseline",
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      TRUE ~ str_to_title(VISIT)
    ),
    AVISITN = case_when(
      VISIT == "BASELINE" | (VSTESTCD == "HEIGHT" & VISIT == "SCREENING 1") ~ 0,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )

# Derive results ----
# See the "Derive Results (AVAL, AVALC)" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#aval)

# Derive analysis result (AVAL)
advs <- advs %>%
  mutate(AVAL = VSSTRESN)

# Derive domain specific parameters ----
# See the "Derive Additional Parameters" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#derive_param)

# Derive BMI (remove BMI from source, and re-derive it)
advs <- advs %>%
  filter(VSTESTCD != "BMI" | is.na(VSTESTCD)) %>%
  derive_param_bmi(
    by_vars = exprs(
      !!!get_admiral_option("subject_keys"), !!!adsl_vars,
      AVISIT, AVISITN, ADT, ADY, ATPT, ATPTN
    ),
    set_values_to = exprs(
      PARAMCD = "BMI"
    ),
    get_unit_expr = VSSTRESU,
    constant_by_vars = get_admiral_option("subject_keys")
  )

# Derive waist-hip ratio
advs <- advs %>%
  derive_param_waisthip(
    by_vars = exprs(
      !!!get_admiral_option("subject_keys"), !!!adsl_vars,
      AVISIT, AVISITN, ADT, ADY, ATPT, ATPTN
    ),
    wstcir_code = "WSTCIR",
    hipcir_code = "HIPCIR",
    set_values_to = exprs(
      PARAMCD = "WAISTHIP"
    ),
    get_unit_expr = VSSTRESU
  )

# Derive categorization variables ----
# See the "Derive Categorization Variables" vignette section for more
# information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#cat)

# Create analysis categories look-up (conditional look-up) table
avalcat_lookup <- exprs(
  ~PARAMCD, ~condition,                ~AVALCAT1,           ~AVALCA1N,
  "BMI",    AVAL < 18.5,               "Underweight",       1,
  "BMI",    AVAL >= 18.5 & AVAL < 25,  "Normal weight",     2,
  "BMI",    AVAL >= 25 & AVAL < 30,    "Overweight",        3,
  "BMI",    AVAL >= 30 & AVAL < 35,    "Obesity class I",   4,
  "BMI",    AVAL >= 35 & AVAL < 40,    "Obesity class II",  5,
  "BMI",    AVAL >= 40,                "Obesity class III", 6,
  "BMI",    is.na(AVAL),               NA_character_,       NA_integer_
)

# Derive BMI class (AVALCAT1, AVALCA1N)
advs <- advs %>%
  derive_vars_cat(
    definition = avalcat_lookup,
    by_vars = exprs(PARAMCD)
  )

# Derive Baseline variables ----
# See the "Derive Baseline" and "Derive Change from Baseline " vignette sections
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#baseline)
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#bchange)

# Add baseline flag (ABLFL)
advs <- advs %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
      order = exprs(ADT, ATPTN, AVISITN),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  )

# Derive baseline analysis value (BASE)
advs <- advs %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVAL,
    new_var = BASE
  )

# Derive absolute (CHG) and relative (PCHG) change from baseline
advs <- advs %>%
  restrict_derivation(
    derivation = derive_var_chg,
    filter = !(is.na(AVISIT) | toupper(AVISIT) %in% "BASELINE")
  ) %>%
  restrict_derivation(
    derivation = derive_var_pchg,
    filter = !(is.na(AVISIT) | toupper(AVISIT) %in% "BASELINE")
  )

# Derive baseline BMI class (BASECAT1, BASECA1N)
advs <- advs %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVALCAT1,
    new_var = BASECAT1
  ) %>%
  derive_var_base(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), PARAMCD),
    source_var = AVALCA1N,
    new_var = BASECA1N
  )

# Derive criterion variables ----
# See the "Derive Criterion Variables" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#crit_vars)

# Set weight loss criterion flags (CRIT1, CRIT1FL)
advs <- advs %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      condition = PCHG <= -5 & PARAMCD == "WEIGHT",
      description = "Achievement of >= 5% weight reduction from baseline",
      crit_nr = 1,
      values_yn = TRUE,
      create_numeric_flag = FALSE
    ),
    filter = AVISITN > 0 & PARAMCD == "WEIGHT"
  ) %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      condition = PCHG <= -10 & PARAMCD == "WEIGHT",
      description = "Achievement of >= 10% weight reduction from baseline",
      crit_nr = 2,
      values_yn = TRUE,
      create_numeric_flag = FALSE
    ),
    filter = AVISITN > 0 & PARAMCD == "WEIGHT"
  )

# Assign parameter variables ----
# See the "Assign PARAMCD, PARAM, PARAMN, PARCAT1" vignette section for more
# information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#paramcd)

# Add all parameter variables (PARAM, PARAMN, PARCAT1, PARCAT1N)
advs <- advs %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT1N),
    by_vars = exprs(PARAMCD)
  )

# Add ADSL variables ----
# See the "Add ADSL variables" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#adsl_vars)

# Add all ADSL variables besides TRTSDT, TRTEDT, TRT01P, TRT01A (stored in
# `adsl_vars`)
advs <- advs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = get_admiral_option("subject_keys")
  )

# Assign ASEQ ----
# See the "Assign ASEQ" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#aseq)

# Calculate ASEQ
advs <- advs %>%
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMN, ADT, AVISITN, VISITNUM, ATPTN),
    check_type = "error"
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
save(advs, file = file.path(dir, "advs.rda"), compress = "bzip2")

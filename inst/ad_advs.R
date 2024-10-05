# Name: ADVS
#
# Label: Vital Signs Analysis Dataset
#
# Input: adsl, vs


# Attach/load required packages ----
library(admiral)
library(admiralmetabolic)
library(tibble)
library(dplyr)
library(stringr)


# Define look-up tables ----
# See function documentation for `derive_vars_merged_lookup()` and
# `derive_vars_cat` for information of how look-up tables are used
# (https://pharmaverse.github.io/admiral/reference/derive_vars_merged_lookup.html)
# (https://pharmaverse.github.io/admiral/reference/derive_vars_cat.html)

# Parameter look-up
param_lookup <- tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN, ~PARCAT1, ~PARCAT1N,
  "HEIGHT", "HEIGHT", "Height (cm)", 1, "Subject Characteristic", 1,
  "WEIGHT", "WEIGHT", "Weight (kg)", 2, "Subject Characteristic", 1,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 3, "Subject Characteristic", 1,
  "HIPCIR", "HIPCIR", "Hip Circumference (cm)", 4, "Subject Characteristic", 1,
  "WSTCIR", "WSTCIR", "Waist Circumference (cm)", 5, "Subject Characteristic", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 6, "Vital Sign", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 7, "Vital Sign", 2,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 8, "Vital Sign", 2,
  "TEMP", "TEMP", "Temperature (C)", 9, "Vital Sign", 2
)

# Analysis categories look-up (conditional look-up)
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

# Read in data ----
# See the "Read in Data" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#readdata)

# Read data
vs_metabolic <- admiralmetabolic::vs_metabolic
adsl <- admiral::admiral_adsl

# Convert SAS empty values to NA
advs <- vs_metabolic |>
  convert_blanks_to_na()
adsl <- adsl |>
  convert_blanks_to_na()

# Merge ADSL variables (TRTSDT, TRTEDT, TRT01P, TRT01A) needed for ADVS
# derivations
advs <- advs |>
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = exprs(TRTSDT, TRTEDT, TRT01P, TRT01A),
    by_vars = exprs(STUDYID, USUBJID)
  )

# Add parameter info to enable later derivations. Parameter information will
# be merged again, when all required AVDS derivations are made.
advs <- advs |>
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
advs <- advs |>
  derive_vars_dt(new_vars_prefix = "A", dtc = VSDTC)
advs <- advs |>
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))


# Derive visit info ----
# See the "Visit and Period Variables" vignette for more information:
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html)

# Derive analysis time point (ATPT, ATPTN) and analysis visit (AVISIT, AVISITN)
advs <- advs |>
  mutate(
    ATPT = VSTPT,
    ATPTN = VSTPTNUM,
    AVISIT = case_when(
      is.na(VISIT) ~ NA_character_,
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      TRUE ~ str_to_title(VISIT)
    ),
    AVISITN = case_when(
      VISIT == "BASELINE" ~ 0,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    )
  )


# Derive results ----
# See the "Derive Results (AVAL, AVALC)" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#aval)

# Derive analysis result (AVAL)
advs <- advs |>
  mutate(
    AVAL = VSSTRESN
  )


# Derive domain specific variables ----
# See the "Derive Additional Parameters" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#derive_param)

# Derive BMI
advs <- advs |>
  filter(VSTESTCD != "BMI") |>
  derive_param_bmi(
    by_vars = exprs(
      STUDYID, USUBJID, TRTSDT, TRTEDT, TRT01P, TRT01A, VISIT,
      VISITNUM, ADT, ADY, VSTPT, VSTPTNUM
    ),
    set_values_to = exprs(
      PARAMCD = "BMI",
      DOMAIN = "VS"
    ),
    get_unit_expr = VSSTRESU,
    filter = VSSTAT != "NOT DONE" | is.na(VSSTAT),
    constant_by_vars = exprs(USUBJID)
  )


# Derive categorization variables ----
# See the "Derive Categorization Variables" vignette section for more
# information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#cat)

# Derive BMI class (AVALCAT1, AVALCA1N)
advs <- advs |>
  derive_vars_cat(
    definition = avalcat_lookup,
    by_vars = exprs(PARAMCD)
  )


# Derive Baseline variables ----
# See the "Derive Baseline" and "Derive Change from Baseline " vignette section
# for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#baseline)
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#bchange)

# Add baseline flag (ABLFL)
advs <- advs |>
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = exprs(STUDYID, USUBJID, PARAMCD),
      order = exprs(ADT, VSTPTNUM, VISITNUM),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = (!is.na(AVAL) & ADT <= TRTSDT)
  )

# Derive baseline analysis value (BASE)
advs <- advs |>
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE
  )

# Derive absolute (CHG) and relative (PCHG) change from baseline
advs <- advs |>
  derive_var_chg()
advs <- advs |>
  derive_var_pchg()


# Derive criterion variables ----
# See the "Derive Criterion Variables" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#crit_vars)

# Set weight loss criterion flags (CRIT1, CRIT1FL)
advs <- advs |>
  derive_vars_crit_flag(
    condition = PCHG <= -5 & PARAMCD == "WEIGHT",
    description = "Achievement of ≥ 5% weight reduction from baseline",
    crit_nr = 1,
    values_yn = TRUE,
    create_numeric_flag = FALSE
  ) |>
  derive_vars_crit_flag(
    condition = PCHG <= -10 & PARAMCD == "WEIGHT",
    description = "Achievement of ≥ 10% weight reduction from baseline",
    crit_nr = 2,
    values_yn = TRUE,
    create_numeric_flag = FALSE
  )


# Assign parameter variables ----
# See the "Assign PARAMCD, PARAM, PARAMN, PARCAT1" vignette section for more
# information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#paramcd)

# Add all parameter variables (PARAM, PARAMN, PARCAT1)
advs <- advs |>
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT1N),
    by_vars = exprs(PARAMCD)
  )


# Add ADSL variables ----
# See the "Add ADSL variables" vignette section for more information:
# (https://pharmaverse.github.io/admiral/articles/bds_finding.html#adsl_vars)

# Add all ADSL variables besides TRTSDT, TRTEDT, TRT01P, TRT01A
advs <- advs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(TRTSDT, TRTEDT, TRT01P, TRT01A)),
    by_vars = exprs(STUDYID, USUBJID)
  )


# Add Labels and Attributes ----
# This process is commonly based on your metadata. As such, no specific example
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

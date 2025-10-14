library(admiral)
#library(admiralmetabolic)
library(dplyr)
library(stringr)
library(tidyr)
library(preventr)

# Set subject keys ----

set_admiral_options(subject_keys = exprs(STUDYID, USUBJID))

# Fetch predictors from ADSL ----

adsl_keep_vars <- exprs(!!!get_admiral_option("subject_keys"), AGE, SEX, TRTSDT, TRTEDT, TRT01P, TRT01A)

adsl <- select(admiralmetabolic_adsl, !!!adsl_keep_vars)

# Fetch predictors from ADLB ----

src_keep_vars <- exprs(PARAM, PARAMCD, AVAL, ADT, ADY, AVISIT, AVISITN)

adrsksc_lb <- pharmaverseadam::adlb_metabolic %>%
  filter(
    PARAMCD %in% c(
      "CHOLES",     # Cholesterol
      "HDL",        # HDL cholesterol
      "GLYCSTT",    # Glycemic Status
      "EGFR",       # eGFR
      "HBA1CHGB",   # HbA1c
      "UACR"        # UACR
    )
  ) %>%
  # Convert HbA1c from mmol/mol to % (required by PREVENT equations)
  restrict_derivation(
    derivation = mutate,
    args = params(
      AVAL = AVAL / 10.929 + 2.15,
      PARAM = str_replace(PARAM, "\\(.+\\)$", "(%)"),
      PARCAT2 = "CV"
    ),
    filter = (PARAMCD == "HBA1CHGB")
  ) %>%
  # Assign traceability variables
  mutate(
    SRCDOM = "ADLB",
    SRCVAR = if_else(!is.na(LBSEQ), "LBSEQ", NA_character_),
    SRCSEQ = LBSEQ
  ) %>%
  select(
    !!!get_admiral_option("subject_keys"),
    PARAM, PARAMCD, AVAL, AVALC, ADT, ADY, AVISIT, AVISITN,
    PARCAT1, PARCAT2, SRCDOM, SRCVAR, SRCSEQ
  )

# Fetch predictors from ADVS ----

adrsksc_vs <- pharmaverseadam::advs_metabolic %>%
  filter(
    PARAMCD %in% c(
      "SYSBP",   # Systolic Blood Pressure
      "BMI"      # Body Mass Index
    )
    & (ABLFL == "Y" | AVISIT != "Baseline")
  ) %>%
  # For the sake of example: Keep the latest measurement per visit
  slice_max(
    order_by = ATPTN,
    by = vars2chr(exprs(!!!get_admiral_option("subject_keys"), PARAMCD, AVISIT)),
    with_ties = FALSE
  ) %>%
  # Assign traceability variables
  mutate(
    SRCDOM = "ADVS",
    SRCVAR = if_else(!is.na(VSSEQ), "VSSEQ", NA_character_),
    SRCSEQ = VSSEQ
  ) %>%
  select(
    !!!get_admiral_option("subject_keys"),
    PARAM, PARAMCD, AVAL, ADT, ADY, AVISIT, AVISITN, SRCDOM, SRCVAR, SRCSEQ
  )

# ADCM pre-processing: Derive ATC Class variables

adcm <- pharmaverseadam::adcm %>%
  #derive_vars_atc()   # Where can we get WHODrug codings?
  mutate(
    ATC2CD = NA_character_,
    ATC4CD = NA_character_
  )

# Use of Blood Pressure treatments ----

# Select BP treatments from ADCM based on WHODrug coding

adcm_bptx <- adcm %>%
  filter(
    ATC2CD %in% c("C02", "C03", "C07", "C08", "C09")
  ) %>%
  mutate(
    ASTDY = if_else(is.na(ASTDY), -Inf, ASTDY),
    AENDY = if_else(is.na(AENDY), Inf, AENDY)
  ) %>%
  distinct(!!!get_admiral_option("subject_keys"), ASTDY, AENDY)

# Identify visits where BP treatments were received at the day of each BP measurement
# and derive a flag as a new parameter

adrsksc_bptx <- adrsksc_vs %>%
  filter(
    PARAMCD == "SYSBP"
  ) %>%
  select(!!!get_admiral_option("subject_keys"), ADT, ADY, AVISIT, AVISITN) %>%
  mutate(
    ASTDY = ADY,
    AENDY = ADY
  ) %>%
  derive_var_joined_exist_flag(
    dataset_add = adcm_bptx,
    by_vars = get_admiral_option("subject_keys"),
    new_var = BPTX,
    join_vars = exprs(ASTDY, AENDY),
    join_type = "all",
    order = exprs(ASTDY, AENDY),
    filter_join =
      !is.na(ADY) &
      between(ADY, ASTDY.join, AENDY.join),
    true_value = "Y",
    false_value = "N"
  ) %>%
  select(-ASTDY, -AENDY) %>%
  pivot_longer(
    cols = "BPTX",
    names_to = "PARAMCD",
    values_to = "AVALC"
  ) %>%
  mutate(
    SRCDOM = "ADCM",
    PARAM = "Use of Blood Pressure Treatment",
    PARCAT1 = "TREATMENT USE"
  )

# Use of Statin ----

# Select Statins from ADCM based on WHODrug coding

adcm_statin <- adcm %>%
  filter(
    ATC4CD %in% c("C10AA", "C10BA", "C10BX")
  ) %>%
  mutate(
    ASTDY = if_else(is.na(ASTDY), -Inf, ASTDY),
    AENDY = if_else(is.na(AENDY), Inf, AENDY)
  ) %>%
  distinct(!!!get_admiral_option("subject_keys"), ASTDY, AENDY)

# Identify visits where Statins were received at the day of each lab sample
# and derive a flag as a new parameter

adrsksc_statin <- adrsksc_lb %>%
  filter(
    SRCDOM == "ADLB"
  ) %>%
  select(!!!get_admiral_option("subject_keys"), ADT, ADY, AVISIT, AVISITN) %>%
  slice_max(
    by = c(vars2chr(get_admiral_option("subject_keys")), "AVISIT", "AVISITN"),
    order_by = ADT,
    with_ties = FALSE,
    na_rm = TRUE
  ) %>%
  mutate(
    ASTDY = ADY,
    AENDY = ADY
  ) %>%
  derive_var_joined_exist_flag(
    dataset_add = adcm_statin,
    by_vars = get_admiral_option("subject_keys"),
    new_var = STATIN,
    join_vars = exprs(ASTDY, AENDY),
    join_type = "all",
    order = exprs(ASTDY, AENDY),
    filter_join =
      !is.na(ADY) &
      between(ADY, ASTDY.join, AENDY.join),
    true_value = "Y",
    false_value = "N"
  ) %>%
  select(-ASTDY, -AENDY) %>%
  pivot_longer(
    cols = "STATIN",
    names_to = "PARAMCD",
    values_to = "AVALC"
  ) %>%
  mutate(
    SRCDOM = "ADCM",
    PARAM = "Use of Statin",
    PARCAT1 = "TREATMENT USE"
  )

# Combine all parameters together ----

adrsksc_predictors <- bind_rows(
  adrsksc_lb, adrsksc_vs,
  adrsksc_bptx, adrsksc_statin
)

# Derive A1LO, A1HI and A1IND (limits required by PREVENT equations) ----

adrsksc_range_lookup <- tribble(
  ~PARAMCD,   ~A1LO, ~A1HI,
  "SYSBP",    90,    180,    # Systolic Blood Pressure
  "BMI",      18.5,  39.9,   # BMI
  "CHOLES",   3.36,  8.28,   # Total cholesterol
  "HDL",      0.52,  2.59,   # HDL cholesterol
  "EGFR",     15,    140,    # eGFR
  "HBA1CHGB", 4.5,   15,     # HbA1c
  "UACR",     0.1,   25000   # UACR
)

adrsksc_predictors <- adrsksc_predictors %>%
  derive_vars_merged_lookup(
    dataset_add = adrsksc_range_lookup,
    by_vars = exprs(PARAMCD),
    print_not_mapped = FALSE
  ) %>%
  mutate(
    A1IND = if_else(
      between(AVAL, A1LO, A1HI),
      "WITHIN RANGE",
      "OUTSIDE RANGE",
      NA_character_
    )
  )

# Merge ADSL variables ----

adrsksc_predictors <- adsl %>%
  right_join(
    adrsksc_predictors,
    by = vars2chr(get_admiral_option("subject_keys"))
  )

# Derive Risk Scores using {preventr} package ----

prevent_results <- adrsksc_predictors %>%
  derive_param_computed(
    by_vars = exprs(!!!adsl_keep_vars, AVISIT, AVISITN),
    parameters = c(
      "SYSBP",      # Systolic Blood Pressure
      "BMI",        # Body Mass Index
      "CHOLES",     # Total cholesterol
      #"HDL",        # HDL cholesterol (not available)
      #"EGFR",       # eGFR (not available)
      "HBA1CHGB",   # HbA1c
      #"UACR",       # UACR (not available)
      #"GLYCSTT",    # Glycemic Status (not available)
      "BPTX",       # Use of Blood Pressure Treatment
      "STATIN"      # Use of Statin
    ),
    set_values_to = exprs(
      PARCAT1 = "PREVENT",
      RISK_EST = mapply(
        preventr::estimate_risk,
        age = AGE,
        sex = tolower(SEX),
        sbp = AVAL.SYSBP,
        bp_tx = AVALC.BPTX == "Y",
        total_c = AVAL.CHOLES,
        hdl_c = 1, #AVAL.HDL,
        statin = AVALC.STATIN == "Y",
        dm = TRUE, #AVALC.GLYCSTT == "DIABETIC",
        smoking = FALSE,
        egfr = 140, #AVAL.EGFR,
        bmi = AVAL.BMI,
        hba1c = AVAL.HBA1CHGB,
        #uacr = AVAL.UACR,
        MoreArgs = list(
          chol_unit = "mmol/L",
          quiet = TRUE
        ),
        SIMPLIFY = FALSE
      )
    ),
    keep_nas = TRUE
  ) %>%
  filter(PARCAT1 == "PREVENT") %>%
  select(-PARAMCD, -PARAM, -AVAL) %>%
  tidyr::unnest_wider(RISK_EST)

# Derive 10-year risk scores as separate parameters

risk_est_10yr <- prevent_results %>%
  select(-risk_est_30yr) %>%
  tidyr::unnest_wider(
    col = risk_est_10yr,
    names_repair = ~ case_match(
      .x,
      "total_cvd" ~ "TCVD10",
      "ascvd" ~ "ASCVD10",
      "heart_failure" ~ "HF10",
      "chd" ~ "CHD10",
      "stroke" ~ "STROK10",
      .default = .x
    )
  ) %>%
  mutate(
    over_years = 10
  ) %>%
  tidyr::pivot_longer(
    cols = c("TCVD10", "ASCVD10", "HF10", "CHD10", "STROK10"),
    names_to = "PARAMCD",
    values_to = "AVAL"
  )

# Derive 30-year risk scores as separate parameters

risk_est_30yr <- prevent_results %>%
  select(-risk_est_10yr) %>%
  tidyr::unnest_wider(
    col = risk_est_30yr,
    names_repair = ~ case_match(
      .x,
      "total_cvd" ~ "TCVD30",
      "ascvd" ~ "ASCVD30",
      "heart_failure" ~ "HF30",
      "chd" ~ "CHD30",
      "stroke" ~ "STROK30",
      .default = .x
    )
  ) %>%
  mutate(
    over_years = 30
  ) %>%
  tidyr::pivot_longer(
    cols = c("TCVD30", "ASCVD30", "HF30", "CHD30", "STROK30"),
    names_to = "PARAMCD",
    values_to = "AVAL"
  )

# Combine all risk scores together and convert them to %

adrsksc_prevent <- bind_rows(risk_est_10yr, risk_est_30yr) %>%
  mutate(
    PARCAT2 = paste0(over_years, "-YEAR RISK"),
    AVAL = AVAL * 100,
    DTYPE = if_else(
      model == "none" | is.na(AVAL),
      "NOT DERIVED",
      paste("PREVENT", toupper(model))
    ),
    COMMENT = input_problems
  ) %>%
  select(-model, -over_years, -input_problems)

# Map PARAM for Risk Scores parameters ----

adrsksc_param_lookup <- tribble(
  ~PARAMCD,  ~PARAM,
  "TCVD10",  "10-yr risk of Total CVD (%)",
  "ASCVD10", "10-yr risk of ASCVD (%)",
  "HF10",    "10-yr risk of Heart Failure (%)",
  "CHD10",   "10-yr risk of CHD (%)",
  "STROK10", "10-yr risk of Stroke (%)",
  "TCVD30",  "30-yr risk of Total CVD (%)",
  "ASCVD30", "30-yr risk of ASCVD (%)",
  "HF30",    "30-yr risk of Heart Failure (%)",
  "CHD30",   "30-yr risk of CHD (%)",
  "STROK30", "30-yr risk of Stroke (%)"
)

adrsksc_prevent <- adrsksc_prevent %>%
  derive_vars_merged_lookup(
    dataset_add = adrsksc_param_lookup,
    order = exprs(PARAMCD, PARAM),
    mode = "last",
    check_type = "none",
    by_vars = exprs(PARAMCD),
    new_vars = exprs(PARAM)
  )

# Final ADRSKSC ----

adrsksc <-
  # Combine risk scores with predictors for traceability
  bind_rows(adrsksc_predictors, adrsksc_prevent) %>%
  # Reorder columns
  relocate(DTYPE, A1LO, A1HI, A1IND, .before = ADT) %>%
  # Sorting order
  arrange(!!!get_admiral_option("subject_keys"), SRCDOM, PARCAT1, PARCAT2, PARAMCD, AVISITN)

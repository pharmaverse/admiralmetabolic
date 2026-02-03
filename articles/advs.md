# Creating a Metabolic ADVS ADaM

## Introduction

This article describes creating a vital signs ADaM for metabolic
clinical trials.

We advise you first consult the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).
The programming workflow around creating the general set-up of an `ADVS`
using [admiral](https://pharmaverse.github.io/admiral/) functions is the
same. In this vignette, we focus on the most common endpoints and their
derivations mainly found in metabolic trials to avoid repeating
information and maintaining the same content in two places. As such, the
code in this vignette is not completely executable; we recommend
consulting the ADVS template script to view the full workflow.

### Required Packages

The examples of this vignette require the following packages.

``` r
library(admiral)
library(admiralmetabolic)
library(pharmaversesdtm)
library(dplyr)
```

## Programming Workflow

- [Read in Data](#readdata)
- [Derive Core ADLB Variables](#derive_core)
- [Assign `PARAMCD`, `PARAM`, `PARAMN`, `PARCAT1`](#paramcd)
- [Derive Metabolic Parameters](#derive_param)
  - [Derive BMI](#bmi)
  - [Derive waist hip ratio](#whr)
- [Common Metabolic Endpoints](#common_endpoints)
  - [Derive Categorization Variables (`AVALCATy`, `BASECATy`)](#cat)
  - [Derive Criterion Variables (`CRITy`, `CRITyFL`,
    `CRITyFN`)](#crit_vars)
- [Remaining ADVS Set-up](#advs_end)

### Read in Data

To start, all data frames needed for the creation of `ADVS` should be
loaded into the global environment. Reading data will usually be a
company specific process, however, for the purpose of this vignette, we
will use example data from
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/) and
[admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/). We
will utilize `DM`, `VS` and `ADSL` for the basis of `ADVS`.

``` r
dm_metabolic <- pharmaversesdtm::dm_metabolic
vs_metabolic <- pharmaversesdtm::vs_metabolic
admiralmetabolic_adsl <- admiralmetabolic::admiralmetabolic_adsl

dm <- convert_blanks_to_na(dm_metabolic)
vs <- convert_blanks_to_na(vs_metabolic)
adsl <- convert_blanks_to_na(admiralmetabolic_adsl)
```

### Derive Core ADVS Variables

The following steps are to merge `ADSL` variables with the source data
and derive the usual `ADVS` analysis variables. Note that only the
sections required for this vignette are covered in the following steps.
To get a detailed guidance on all the steps, refer the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).

``` r
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01P, TRT01A)

advs <- derive_vars_merged(
  vs,
  dataset_add = adsl,
  new_vars = adsl_vars,
  by_vars = exprs(STUDYID, USUBJID)
)

advs <- derive_vars_dt(advs, new_vars_prefix = "A", dtc = VSDTC)
advs <- derive_vars_dy(advs, reference_date = TRTSDT, source_vars = exprs(ADT))
```

### Create `PARAMCD`, `PARAM`, `PARAMN`, `PARCAT1` variables

The next step is to create and assign parameter level variables such as
`PARAMCD`, `PARAM`, `PARAMN`, `PARCAT1`, etc. For this, a lookup can be
created based on the SDTM `--TESTCD` value to join to the source data.
One key addition in metabolic trials are vital sign parameters
associated to body measurements, such as `BMI`, `HIPCIR`, and `WSTCIR`.

``` r
param_lookup <- tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN, ~PARCAT1, ~PARCAT1N,
  "HEIGHT", "HEIGHT", "Height (cm)", 1, "Anthropometric Measurement", 1,
  "WEIGHT", "WEIGHT", "Weight (kg)", 2, "Anthropometric Measurement", 1,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 3, "Anthropometric Measurement", 1,
  "HIPCIR", "HIPCIR", "Hip Circumference (cm)", 4, "Anthropometric Measurement", 1,
  "WSTCIR", "WSTCIR", "Waist Circumference (cm)", 5, "Anthropometric Measurement", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 6, "Vital Sign", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 7, "Vital Sign", 2,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 8, "Vital Sign", 2,
  "TEMP", "TEMP", "Temperature (C)", 9, "Vital Sign", 2
)
```

This lookup may now be joined to the source data and this is how the
parameters will look like:

``` r
advs <- derive_vars_merged_lookup(
  advs,
  dataset_add = param_lookup,
  new_vars = exprs(PARAMCD, PARAM, PARAMN, PARCAT1, PARCAT1N),
  by_vars = exprs(VSTESTCD)
)
```

### Derive Parameters for Metabolic indicators

In clinical trials focused on metabolic conditions, it’s common to
derive additional parameters from the collected data. These derived
parameters often provide valuable insights into the metabolic health of
the subjects. In this vignette, we will explore how one could derive BMI
and waist-hip ratio.

#### Derive BMI

In metabolic trials, `BMI` is often calculated at source. But while
creating the `ADVS` dataset, we re-derive `BMI` from the collected
height and weight values. This is done to ensure that the `BMI` is
calculated consistently across all subjects and visits.

In this step, we create parameter Body Mass Index (`BMI`) for the `ADVS`
domain using the
[`derive_param_bmi()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_bmi.html)
function. Note that only variables specified in the `by_vars` argument
will be populated in the newly created records. Also note that if height
is collected only once for a subject use `constant_by_vars` to specify
the function to merge by the subject-level variable - otherwise BMI is
only calculated for visits where both are collected.

``` r
# Remove BMI collected in SDTM
advs <- advs %>% filter(VSTESTCD != "BMI" | is.na(VSTESTCD))

# Re-calculate BMI
advs <- derive_param_bmi(
  advs,
  by_vars = c(
    get_admiral_option("subject_keys"),
    exprs(!!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM)
  ),
  set_values_to = exprs(
    PARAMCD = "BMI",
    PARAM = "Body Mass Index (kg/m^2)",
    PARAMN = 3,
    PARCAT1 = "Anthropometric Measurement",
    PARCAT1N = 1
  ),
  get_unit_expr = VSSTRESU,
  constant_by_vars = exprs(USUBJID)
)
```

#### Derive waist hip-ratio

Metabolic trials often include ratios between different anthropometric
measurements. For this,
[`{admiralmetabolic}`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthip.html)
provides several functions to quickly calculate various anthropometric
ratios. For instance, the function
[`derive_param_waisthip()`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthip.html)
can be used to derive the waist-hip ratio.

``` r
advs <- advs %>%
  derive_param_waisthip(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), VISIT, VISITNUM),
    wstcir_code = "WSTCIR",
    hipcir_code = "HIPCIR",
    set_values_to = exprs(
      PARAMCD = "WAISTHIP",
      PARAM = "Waist to Hip Ratio"
    ),
    get_unit_expr = VSSTRESU
  )
```

### Derive Variables for Metabolic indicators

In the following sections, we will explore some of the most common
endpoints typically observed in metabolic trials.

One such endpoint is the improvement in weight category from baseline to
the end of treatment, which is often assessed using Body Mass Index
(`BMI`). To capture this, we will derive variables such as `AVALCATy`
and `BASECATy`, as detailed in the subsequent section.

Additionally, the achievement of weight reduction thresholds, such as
\>= 5%, \>= 10%, or \>= 15% from baseline to end of treatment or at a
certain visit, is a common endpoint in metabolic trials. To accommodate
these criteria, we will derive relevant criterion variables such as
`CRITy`, `CRITyFL`, and `CRITyFLN`, with the necessary functions
provided by [admiral](https://pharmaverse.github.io/admiral/) outlined
below.

#### Derive Categorization Variables (`AVALCATy`, `BASECATy`)

For deriving categorization variables (`AVALCATy`, `BASECATy`)
[admiral](https://pharmaverse.github.io/admiral/) provides
[`derive_vars_cat()`](https://pharmaverse.github.io/admiral/dev/reference/derive_vars_cat.html)
(see documentation of the function for details).

``` r
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
```

Now we can use `derive_var_base` to derive the `BASECATy`/ `BASECAyN`
variables.

``` r
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
```

#### Derive Criterion Variables (`CRITy`, `CRITyFL`, `CRITyFN`)

For deriving criterion variables (`CRITy`, `CRITyFL`, `CRITyFN`)
[admiral](https://pharmaverse.github.io/admiral/) provides
[`derive_vars_crit_flag()`](https://pharmaverse.github.io/admiral/dev/reference/derive_vars_crit_flag.html).
It ensures that they are derived in an ADaM-compliant way (see
documentation of the function for details).

In most cases the criterion depends on the parameter and in this case
the higher order function
[`restrict_derivation()`](https://pharmaverse.github.io/admiral/dev/reference/restrict_derivation.html)
can be useful. In the following example, the criterion flags for weight
based on percentage change in weight reduction from baseline is derived.
Additional criterion flags can be added as needed.

``` r
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
    filter = VISITNUM > 0 & PARAMCD == "WEIGHT"
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
    filter = VISITNUM > 0 & PARAMCD == "WEIGHT"
  )
```

### Remaining ADVS Set-up

The [admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html)
covers all the steps that are not shown here, such as merging the
parameter-level values, timing variables, and analysis flags.

## Example Scripts

| ADaM | Sample Code                                                                                     |
|------|-------------------------------------------------------------------------------------------------|
| ADVS | [ad_advs.R](https://github.com/pharmaverse/admiralmetabolic/blob/main/inst/templates/ad_advs.R) |

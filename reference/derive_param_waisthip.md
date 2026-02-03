# Adds a Parameter for Waist to Hip Ratio

Adds a parameter for Waist to Hip Ratio using Waist Circumference and
Hip Circumference for each by group (e.g., subject and visit) where the
source parameters are available.

**Note:** This is a wrapper function for the more generic
[`admiral::derive_param_computed()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_computed.html).

## Usage

``` r
derive_param_waisthip(
  dataset,
  by_vars,
  wstcir_code = "WSTCIR",
  hipcir_code = "HIPCIR",
  set_values_to = exprs(PARAMCD = "WAISTHIP"),
  filter = NULL,
  get_unit_expr
)
```

## Arguments

- dataset:

  Input dataset

  The variables specified by the `by_vars` argument are expected to be
  in the dataset. `PARAMCD`, and `AVAL` are expected as well.

  The variable specified by `by_vars` and `PARAMCD` must be a unique key
  of the input dataset after restricting it by the filter condition
  (`filter` argument) and to the parameters specified by `wstcir_code`
  and `hipcir_code`.

- by_vars:

  Grouping variables

  Grouping variables uniquely identifying a set of records for which
  `new_vars` are to be calculated.

  *Permitted Values:* list of variables created by exprs()

- wstcir_code:

  Waist Circumference parameter code

  The observations where `PARAMCD` equals the specified value are
  considered as the Waist Circumference.

  *Permitted Values:* character value

- hipcir_code:

  Hip Circumference parameter code

  The observations where `PARAMCD` equals the specified value are
  considered as the Hip Circumference

  *Permitted Values:* character value

- set_values_to:

  Variables to be set

  The specified variables are set to the specified values for the new
  observations. For example `exprs(PARAMCD = "RATIO")` defines the
  parameter code for the new parameter.

  *Permitted Values:* List of variable-value pairs

- filter:

  Filter condition

  The specified condition is applied to the input dataset before
  deriving the new parameter, i.e., only observations fulfilling the
  condition are taken into account.

  *Permitted Values:* a condition

- get_unit_expr:

  An expression providing the unit of the parameter

  The result is used to check the units of the input parameters. If the
  units are not consistent within each parameter, an error will be
  thrown.

  Additionally, if the input parameters are measured in different units
  but are mutually convertible (e.g., centimeters for one parameter and
  inches for another), an automatic conversion will be performed in
  order to uniform the values before calculating the ratio.

  **Note:** Conversion factors come from unit definitions as per CDISC
  standards.  
  *m* is defined as 100 cm  
  *mm* is defined as 0.1 cm  
  *in* is defined as 2.54 cm  
  *ft* is defined as 30.48 cm

  *Permitted Values:* A variable of the input dataset or a function call

## Value

The input dataset with the new parameter added. Note, a variable will
only be populated in the new parameter rows if it is specified in
`by_vars`.

## Details

The analysis value of the new parameter is derived as \$\$WAISTHIP =
\frac{WSTCIR}{HIPCIR}\$\$

## See also

[`admiral::derive_param_computed()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_computed.html)

ADVS Functions for adding Parameters:
[`derive_param_waisthgt()`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthgt.md)

## Examples

``` r
library(tibble)
library(rlang)

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 125, "cm", "SCREENING",
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 124, "cm", "WEEK 2",
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 123, "cm", "WEEK 3",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 135, "cm", "SCREENING",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 133, "cm", "WEEK 2",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 132, "cm", "WEEK 3"
)

derive_param_waisthip(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  hipcir_code = "HIPCIR",
  set_values_to = exprs(
    PARAMCD = "WAISTHIP",
    PARAM = "Waist to Hip Ratio"
  ),
  get_unit_expr = admiral::extract_unit(PARAM)
)
#> # A tibble: 18 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 WSTCIR   Waist Circumference (cm) 110     cm    SCREENING
#>  2 01-101-1001 WSTCIR   Waist Circumference (cm) 108     cm    WEEK 2   
#>  3 01-101-1001 WSTCIR   Waist Circumference (cm) 107     cm    WEEK 3   
#>  4 01-101-1001 HIPCIR   Hip Circumference (cm)   125     cm    SCREENING
#>  5 01-101-1001 HIPCIR   Hip Circumference (cm)   124     cm    WEEK 2   
#>  6 01-101-1001 HIPCIR   Hip Circumference (cm)   123     cm    WEEK 3   
#>  7 01-101-1002 WSTCIR   Waist Circumference (cm) 120     cm    SCREENING
#>  8 01-101-1002 WSTCIR   Waist Circumference (cm) 118     cm    WEEK 2   
#>  9 01-101-1002 WSTCIR   Waist Circumference (cm) 117     cm    WEEK 3   
#> 10 01-101-1002 HIPCIR   Hip Circumference (cm)   135     cm    SCREENING
#> 11 01-101-1002 HIPCIR   Hip Circumference (cm)   133     cm    WEEK 2   
#> 12 01-101-1002 HIPCIR   Hip Circumference (cm)   132     cm    WEEK 3   
#> 13 01-101-1001 WAISTHIP Waist to Hip Ratio         0.88  NA    SCREENING
#> 14 01-101-1001 WAISTHIP Waist to Hip Ratio         0.871 NA    WEEK 2   
#> 15 01-101-1001 WAISTHIP Waist to Hip Ratio         0.870 NA    WEEK 3   
#> 16 01-101-1002 WAISTHIP Waist to Hip Ratio         0.889 NA    SCREENING
#> 17 01-101-1002 WAISTHIP Waist to Hip Ratio         0.887 NA    WEEK 2   
#> 18 01-101-1002 WAISTHIP Waist to Hip Ratio         0.886 NA    WEEK 3   

# Only adding Waist to Hip Ratio at certain visits

derive_param_waisthip(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  hipcir_code = "HIPCIR",
  set_values_to = exprs(
    PARAMCD = "WAISTHIP",
    PARAM = "Waist to Hip Ratio"
  ),
  get_unit_expr = admiral::extract_unit(PARAM),
  filter = VISIT %in% c("SCREENING", "WEEK 3")
)
#> # A tibble: 16 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 WSTCIR   Waist Circumference (cm) 110     cm    SCREENING
#>  2 01-101-1001 WSTCIR   Waist Circumference (cm) 108     cm    WEEK 2   
#>  3 01-101-1001 WSTCIR   Waist Circumference (cm) 107     cm    WEEK 3   
#>  4 01-101-1001 HIPCIR   Hip Circumference (cm)   125     cm    SCREENING
#>  5 01-101-1001 HIPCIR   Hip Circumference (cm)   124     cm    WEEK 2   
#>  6 01-101-1001 HIPCIR   Hip Circumference (cm)   123     cm    WEEK 3   
#>  7 01-101-1002 WSTCIR   Waist Circumference (cm) 120     cm    SCREENING
#>  8 01-101-1002 WSTCIR   Waist Circumference (cm) 118     cm    WEEK 2   
#>  9 01-101-1002 WSTCIR   Waist Circumference (cm) 117     cm    WEEK 3   
#> 10 01-101-1002 HIPCIR   Hip Circumference (cm)   135     cm    SCREENING
#> 11 01-101-1002 HIPCIR   Hip Circumference (cm)   133     cm    WEEK 2   
#> 12 01-101-1002 HIPCIR   Hip Circumference (cm)   132     cm    WEEK 3   
#> 13 01-101-1001 WAISTHIP Waist to Hip Ratio         0.88  NA    SCREENING
#> 14 01-101-1001 WAISTHIP Waist to Hip Ratio         0.870 NA    WEEK 3   
#> 15 01-101-1002 WAISTHIP Waist to Hip Ratio         0.889 NA    SCREENING
#> 16 01-101-1002 WAISTHIP Waist to Hip Ratio         0.886 NA    WEEK 3   

# Automatic conversion is performed when deriving the ratio
# if parameters are provided in different units

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 125, "cm", "SCREENING",
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 124, "cm", "WEEK 2",
  "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 123, "cm", "WEEK 3",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 43.31, "in", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 42.52, "in", "WEEK 2",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 42.13, "in", "WEEK 3",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 135, "cm", "SCREENING",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 133, "cm", "WEEK 2",
  "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 132, "cm", "WEEK 3",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 47.24, "in", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 46.46, "in", "WEEK 2",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 46.06, "in", "WEEK 3"
)

derive_param_waisthip(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  hipcir_code = "HIPCIR",
  set_values_to = exprs(
    PARAMCD = "WAISTHIP",
    PARAM = "Waist to Hip Ratio"
  ),
  get_unit_expr = admiral::extract_unit(PARAM)
)
#> ℹ Unit conversion performed for "HIPCIR". Values converted from "cm" to "in".
#> # A tibble: 18 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 HIPCIR   Hip Circumference (cm)   125     cm    SCREENING
#>  2 01-101-1001 HIPCIR   Hip Circumference (cm)   124     cm    WEEK 2   
#>  3 01-101-1001 HIPCIR   Hip Circumference (cm)   123     cm    WEEK 3   
#>  4 01-101-1001 WSTCIR   Waist Circumference (in)  43.3   in    SCREENING
#>  5 01-101-1001 WSTCIR   Waist Circumference (in)  42.5   in    WEEK 2   
#>  6 01-101-1001 WSTCIR   Waist Circumference (in)  42.1   in    WEEK 3   
#>  7 01-101-1002 HIPCIR   Hip Circumference (cm)   135     cm    SCREENING
#>  8 01-101-1002 HIPCIR   Hip Circumference (cm)   133     cm    WEEK 2   
#>  9 01-101-1002 HIPCIR   Hip Circumference (cm)   132     cm    WEEK 3   
#> 10 01-101-1002 WSTCIR   Waist Circumference (in)  47.2   in    SCREENING
#> 11 01-101-1002 WSTCIR   Waist Circumference (in)  46.5   in    WEEK 2   
#> 12 01-101-1002 WSTCIR   Waist Circumference (in)  46.1   in    WEEK 3   
#> 13 01-101-1001 WAISTHIP Waist to Hip Ratio         0.880 NA    SCREENING
#> 14 01-101-1001 WAISTHIP Waist to Hip Ratio         0.871 NA    WEEK 2   
#> 15 01-101-1001 WAISTHIP Waist to Hip Ratio         0.870 NA    WEEK 3   
#> 16 01-101-1002 WAISTHIP Waist to Hip Ratio         0.889 NA    SCREENING
#> 17 01-101-1002 WAISTHIP Waist to Hip Ratio         0.887 NA    WEEK 2   
#> 18 01-101-1002 WAISTHIP Waist to Hip Ratio         0.886 NA    WEEK 3   
```

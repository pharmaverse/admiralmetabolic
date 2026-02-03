# Adds a Parameter for Waist to Height Ratio

Adds a parameter for Waist to Height Ratio using Waist Circumference and
Height for each by group (e.g., subject and visit) where the source
parameters are available.

**Note:** This is a wrapper function for the more generic
[`admiral::derive_param_computed()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_computed.html).

## Usage

``` r
derive_param_waisthgt(
  dataset,
  by_vars,
  wstcir_code = "WSTCIR",
  height_code = "HEIGHT",
  set_values_to = exprs(PARAMCD = "WAISTHGT"),
  filter = NULL,
  constant_by_vars = NULL,
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
  and `height_code`.

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

- height_code:

  Height parameter code

  The observations where `PARAMCD` equals the specified value are
  considered as the Height.

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

- constant_by_vars:

  By variables for when Height is constant

  When Height is constant, the Height parameters (measured only once)
  are merged to the other parameters using the specified variables.

  If Height is constant (e.g. only measured once at screening or
  baseline) then use `constant_by_vars` to select the subject-level
  variable to merge on (e.g. `USUBJID`). This will produce Waist to
  Height Ratio at all visits where Waist Circumference is measured.
  Otherwise it will only be calculated at visits with both Height and
  Waist Circumference collected.

  *Permitted Values*: list of variables created by
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.html),
  e.g. `exprs(USUBJID, VISIT)`

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

The analysis value of the new parameter is derived as \$\$WAISTHGT =
\frac{WSTCIR}{HEIGHT}\$\$

## See also

[`admiral::derive_param_computed()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_computed.html)

ADVS Functions for adding Parameters:
[`derive_param_waisthip()`](https://pharmaverse.github.io/admiralmetabolic/reference/derive_param_waisthip.md)

## Examples

``` r
library(tibble)
library(rlang)

# Example 1: Derive Waist to Height Ratio where Height is measured only once

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
  "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
  "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3",
)

derive_param_waisthgt(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  height_code = "HEIGHT",
  set_values_to = exprs(
    PARAMCD = "WAISTHGT",
    PARAM = "Waist to Height Ratio"
  ),
  constant_by_vars = exprs(USUBJID),
  get_unit_expr = admiral::extract_unit(PARAM)
)
#> # A tibble: 14 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 HEIGHT   Height (cm)              147     cm    SCREENING
#>  2 01-101-1001 WSTCIR   Waist Circumference (cm) 110     cm    SCREENING
#>  3 01-101-1001 WSTCIR   Waist Circumference (cm) 108     cm    WEEK 2   
#>  4 01-101-1001 WSTCIR   Waist Circumference (cm) 107     cm    WEEK 3   
#>  5 01-101-1002 HEIGHT   Height (cm)              163     cm    SCREENING
#>  6 01-101-1002 WSTCIR   Waist Circumference (cm) 120     cm    SCREENING
#>  7 01-101-1002 WSTCIR   Waist Circumference (cm) 118     cm    WEEK 2   
#>  8 01-101-1002 WSTCIR   Waist Circumference (cm) 117     cm    WEEK 3   
#>  9 01-101-1001 WAISTHGT Waist to Height Ratio      0.748 NA    SCREENING
#> 10 01-101-1001 WAISTHGT Waist to Height Ratio      0.735 NA    WEEK 2   
#> 11 01-101-1001 WAISTHGT Waist to Height Ratio      0.728 NA    WEEK 3   
#> 12 01-101-1002 WAISTHGT Waist to Height Ratio      0.736 NA    SCREENING
#> 13 01-101-1002 WAISTHGT Waist to Height Ratio      0.724 NA    WEEK 2   
#> 14 01-101-1002 WAISTHGT Waist to Height Ratio      0.718 NA    WEEK 3   

# Example 2: Same as above but only adding Waist to Height Ratio
# at certain visits

derive_param_waisthgt(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  height_code = "HEIGHT",
  set_values_to = exprs(
    PARAMCD = "WAISTHGT",
    PARAM = "Waist to Height Ratio"
  ),
  constant_by_vars = exprs(USUBJID),
  get_unit_expr = admiral::extract_unit(PARAM),
  filter = VISIT %in% c("SCREENING", "WEEK 3")
)
#> # A tibble: 12 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 HEIGHT   Height (cm)              147     cm    SCREENING
#>  2 01-101-1001 WSTCIR   Waist Circumference (cm) 110     cm    SCREENING
#>  3 01-101-1001 WSTCIR   Waist Circumference (cm) 108     cm    WEEK 2   
#>  4 01-101-1001 WSTCIR   Waist Circumference (cm) 107     cm    WEEK 3   
#>  5 01-101-1002 HEIGHT   Height (cm)              163     cm    SCREENING
#>  6 01-101-1002 WSTCIR   Waist Circumference (cm) 120     cm    SCREENING
#>  7 01-101-1002 WSTCIR   Waist Circumference (cm) 118     cm    WEEK 2   
#>  8 01-101-1002 WSTCIR   Waist Circumference (cm) 117     cm    WEEK 3   
#>  9 01-101-1001 WAISTHGT Waist to Height Ratio      0.748 NA    SCREENING
#> 10 01-101-1001 WAISTHGT Waist to Height Ratio      0.728 NA    WEEK 3   
#> 11 01-101-1002 WAISTHGT Waist to Height Ratio      0.736 NA    SCREENING
#> 12 01-101-1002 WAISTHGT Waist to Height Ratio      0.718 NA    WEEK 3   

# Example 3: Pediatric study where Height and Waist Circumference
# are measured multiple times

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
  "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
  "01-101-1001", "HEIGHT", "Height (cm)", 148, "cm", "WEEK 2",
  "01-101-1001", "HEIGHT", "Height (cm)", 149, "cm", "WEEK 3",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 100, "cm", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 99, "cm", "WEEK 2",
  "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 98, "cm", "WEEK 3",
  "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
  "01-101-1002", "HEIGHT", "Height (cm)", 164, "cm", "WEEK 2",
  "01-101-1002", "HEIGHT", "Height (cm)", 165, "cm", "WEEK 3",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 109, "cm", "WEEK 2",
  "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 3"
)

derive_param_waisthgt(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  height_code = "HEIGHT",
  set_values_to = exprs(
    PARAMCD = "WAISTHGT",
    PARAM = "Waist to Height Ratio"
  ),
  get_unit_expr = admiral::extract_unit(PARAM)
)
#> # A tibble: 18 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 HEIGHT   Height (cm)              147     cm    SCREENING
#>  2 01-101-1001 HEIGHT   Height (cm)              148     cm    WEEK 2   
#>  3 01-101-1001 HEIGHT   Height (cm)              149     cm    WEEK 3   
#>  4 01-101-1001 WSTCIR   Waist Circumference (cm) 100     cm    SCREENING
#>  5 01-101-1001 WSTCIR   Waist Circumference (cm)  99     cm    WEEK 2   
#>  6 01-101-1001 WSTCIR   Waist Circumference (cm)  98     cm    WEEK 3   
#>  7 01-101-1002 HEIGHT   Height (cm)              163     cm    SCREENING
#>  8 01-101-1002 HEIGHT   Height (cm)              164     cm    WEEK 2   
#>  9 01-101-1002 HEIGHT   Height (cm)              165     cm    WEEK 3   
#> 10 01-101-1002 WSTCIR   Waist Circumference (cm) 110     cm    SCREENING
#> 11 01-101-1002 WSTCIR   Waist Circumference (cm) 109     cm    WEEK 2   
#> 12 01-101-1002 WSTCIR   Waist Circumference (cm) 108     cm    WEEK 3   
#> 13 01-101-1001 WAISTHGT Waist to Height Ratio      0.680 NA    SCREENING
#> 14 01-101-1001 WAISTHGT Waist to Height Ratio      0.669 NA    WEEK 2   
#> 15 01-101-1001 WAISTHGT Waist to Height Ratio      0.658 NA    WEEK 3   
#> 16 01-101-1002 WAISTHGT Waist to Height Ratio      0.675 NA    SCREENING
#> 17 01-101-1002 WAISTHGT Waist to Height Ratio      0.665 NA    WEEK 2   
#> 18 01-101-1002 WAISTHGT Waist to Height Ratio      0.655 NA    WEEK 3   

# Example 4: Automatic conversion is performed when deriving the ratio
# if parameters are provided in different units (e.g. centimeters and inches)

advs <- tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
  "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 39.37, "in", "SCREENING",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 38.98, "in", "WEEK 2",
  "01-101-1001", "WSTCIR", "Waist Circumference (in)", 38.58, "in", "WEEK 3",
  "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 43.31, "in", "SCREENING",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 42.91, "in", "WEEK 2",
  "01-101-1002", "WSTCIR", "Waist Circumference (in)", 42.52, "in", "WEEK 3"
)

derive_param_waisthgt(
  advs,
  by_vars = exprs(USUBJID, VISIT),
  wstcir_code = "WSTCIR",
  height_code = "HEIGHT",
  set_values_to = exprs(
    PARAMCD = "WAISTHGT",
    PARAM = "Waist to Height Ratio"
  ),
  constant_by_vars = exprs(USUBJID),
  get_unit_expr = admiral::extract_unit(PARAM)
)
#> ℹ Unit conversion performed for "HEIGHT". Values converted from "cm" to "in".
#> # A tibble: 14 × 6
#>    USUBJID     PARAMCD  PARAM                       AVAL AVALU VISIT    
#>    <chr>       <chr>    <chr>                      <dbl> <chr> <chr>    
#>  1 01-101-1001 HEIGHT   Height (cm)              147     cm    SCREENING
#>  2 01-101-1001 WSTCIR   Waist Circumference (in)  39.4   in    SCREENING
#>  3 01-101-1001 WSTCIR   Waist Circumference (in)  39.0   in    WEEK 2   
#>  4 01-101-1001 WSTCIR   Waist Circumference (in)  38.6   in    WEEK 3   
#>  5 01-101-1002 HEIGHT   Height (cm)              163     cm    SCREENING
#>  6 01-101-1002 WSTCIR   Waist Circumference (in)  43.3   in    SCREENING
#>  7 01-101-1002 WSTCIR   Waist Circumference (in)  42.9   in    WEEK 2   
#>  8 01-101-1002 WSTCIR   Waist Circumference (in)  42.5   in    WEEK 3   
#>  9 01-101-1001 WAISTHGT Waist to Height Ratio      0.680 NA    SCREENING
#> 10 01-101-1001 WAISTHGT Waist to Height Ratio      0.674 NA    WEEK 2   
#> 11 01-101-1001 WAISTHGT Waist to Height Ratio      0.667 NA    WEEK 3   
#> 12 01-101-1002 WAISTHGT Waist to Height Ratio      0.675 NA    SCREENING
#> 13 01-101-1002 WAISTHGT Waist to Height Ratio      0.669 NA    WEEK 2   
#> 14 01-101-1002 WAISTHGT Waist to Height Ratio      0.663 NA    WEEK 3   
```

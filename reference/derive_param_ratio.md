# Adds a Ratio Parameter Computed from the Analysis Value of Other Parameters

Adds a record for a generic Ratio parameter using two existing parameter
(numerator and denominator) each by group (e.g., subject and visit)
where the source parameters are available.

**Note:** This is a wrapper function for the more generic
[`admiral::derive_param_computed()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_param_computed.html).

## Usage

``` r
derive_param_ratio(
  dataset,
  by_vars,
  numerator_code,
  denominator_code,
  set_values_to,
  constant_numerator = FALSE,
  constant_denominator = FALSE,
  filter = NULL,
  constant_by_vars = NULL,
  get_unit_expr = NULL,
  unit_conversion = FALSE
)
```

## Arguments

- dataset:

  Input dataset

  The variables specified by the `by_vars` argument are expected to be
  in the dataset. `PARAMCD`, and `AVAL` are expected as well.

  The variable specified by `by_vars` and `PARAMCD` must be a unique key
  of the input dataset after restricting it by the filter condition
  (`filter` argument) and to the parameters specified by
  `numerator_code` and `denominator_code`.

- by_vars:

  Grouping variables

  Grouping variables uniquely identifying a set of records for which
  `new_vars` are to be calculated.

  *Permitted Values:* list of variables created by exprs()

- numerator_code:

  Numerator parameter code

  The observations where `PARAMCD` equals the specified value are
  considered as the numerator.

  *Permitted Values:* character value

- denominator_code:

  Denominator parameter code

  The observations where `PARAMCD` equals the specified value are
  considered as the denominator.

  *Permitted Values:* character value

- set_values_to:

  Variables to be set

  The specified variables are set to the specified values for the new
  observations. For example `exprs(PARAMCD = "RATIO")` defines the
  parameter code for the new parameter.

  *Permitted Values:* List of variable-value pairs

- constant_numerator:

  Is numerator parameter constant?

  It is expected that the parameter code (PARAMCD) specified in
  `numerator_code` which is required to derive the new parameter is
  measured only once. For example, if Height to Weight Ratio should be
  derived and height is measured only once while Weight is measured at
  each visit. Height could be specified in the `numerator_code` argument
  and `constant_numerator` is to be set to `TRUE`.

  *Permitted Values:* logical scalar

- constant_denominator:

  Is denominator parameter constant?

  It is expected that the parameter code (PARAMCD) specified in
  `numerator_code` which is required to derive the new parameter is
  measured only once. For example, if Waist to Height Ratio should be
  derived and height is measured only once while Waist Circumference is
  measured at each visit. Height could be specified in the
  `denominator_code` argument and `constant_denominator` is to be set to
  `TRUE`.

  *Permitted Values:* logical scalar

- filter:

  Filter condition

  The specified condition is applied to the input dataset before
  deriving the new parameter, i.e., only observations fulfilling the
  condition are taken into account.

  *Permitted Values:* a condition

- constant_by_vars:

  By variables for when numerator and/or denominator is constant

  When numerator and/or denominator is constant, the parameters
  (measured only once) are merged to the other parameters using the
  specified variables.

  If numerator and/or denominator is constant (e.g. only measured once
  at screening or baseline) then use `constant_by_vars` to select the
  subject-level variable to merge on (e.g. `USUBJID`). This will produce
  a generic Ratio parameter at all visits where numerator and/or
  denominator is measured. Otherwise it will only be calculated at
  visits with both numerator and denominator parameters collected.

  *Permitted Values*: list of variables created by `exprs()`, e.g.
  `exprs(USUBJID, VISIT)`

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

- unit_conversion:

  Enable/Disable unit conversion

  Unit conversion is disabled by default. Ignored if `get_unit_expr` is
  `NULL`.

  *Permitted Values:* logical scalar

## Value

The input dataset with the new parameter added. Note, a variable will
only be populated in the new parameter rows if it is specified in
`by_vars`.

## Details

The analysis value of the new parameter is derived as \$\$RATIO =
\frac{NUMERATOR}{DENOMINATOR}\$\$

## See also

Other internal:
[`admiralmetabolic-package`](https://pharmaverse.github.io/admiralmetabolic/reference/admiralmetabolic-package.md)

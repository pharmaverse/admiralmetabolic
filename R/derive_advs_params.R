#' Adds a Parameter for Waist-to-Hip Ratio
#'
#' @description Adds a record for Waist-to-Hip Ratio using Waist Circumference and Hip Circumference
#' each by group (e.g., subject and visit) where the source parameters are available.
#'
#' **Note:** This is a wrapper function for the more generic \code{admiral::derive_param_ratio()}.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the \code{by_vars} argument are expected to be in the dataset.
#'   \code{PARAMCD}, and \code{AVAL} are expected as well.
#'
#'   The variable specified by \code{by_vars} and \code{PARAMCD} must be a unique key of
#'   the input dataset after restricting it by the filter condition (\code{filter}
#'   parameter) and to the parameters specified by \code{wstcir_code} and \code{hipcir_code}.
#'
#' @param wstcir_code Waist Circumference parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the Waist Circumference
#'
#'   *Permitted Values:* character value
#'
#' @param hipcir_code Hip Circumference parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the Hip Circumference
#'
#'   *Permitted Values:* character value
#'
#' @inheritParams admiral::derive_param_ratio
#'
#' @details
#' The analysis value of the new parameter is derived as
#' \deqn{WAISTHIP = \frac{WSTCIR}{HIPCIR}}
#'
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'    be populated in the new parameter rows if it is specified in \code{by_vars}.
#'
#' @export
#'
#' @seealso \code{\link[=admiral::derive_param_ratio]{admiral::derive_param_ratio()}},
#'          \code{\link[=admiral::compute_ratio]{admiral::compute_ratio()}}
#'
#' @examples
#'
#' library(tibble)
#'
#' advs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
#'   "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 125, "cm", "SCREENING",
#'   "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 124, "cm", "WEEK 2",
#'   "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 123, "cm", "WEEK 3",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3",
#'   "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 135, "cm", "SCREENING",
#'   "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 133, "cm", "WEEK 2",
#'   "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 132, "cm", "WEEK 3"
#' )
#'
#' derive_param_waisthip(
#'   advs,
#'   by_vars = exprs(USUBJID, VISIT),
#'   wstcir_code = "WSTCIR",
#'   hipcir_code = "HIPCIR",
#'   set_values_to = exprs(
#'     PARAMCD = "WAISTHIP",
#'     PARAM = "Waist-to-Hip Ratio"
#'   )
#' )
derive_param_waisthip <- function(dataset,
                                  by_vars,
                                  wstcir_code = "WSTCIR",
                                  hipcir_code = "HIPCIR",
                                  set_values_to = exprs(PARAMCD = "WAISTHIP"),
                                  filter = NULL) {
  assert_vars(by_vars)
  assert_data_frame(dataset, required_vars = exprs(!!!by_vars, PARAMCD, AVAL))
  assert_character_scalar(wstcir_code)
  assert_character_scalar(hipcir_code)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)

  admiral::derive_param_ratio(
    dataset,
    filter = !!filter,
    dividend_code = wstcir_code,
    divisor_code = hipcir_code,
    by_vars = by_vars,
    set_values_to = set_values_to
  )
}

#' Adds a Parameter for Waist-to-Height Ratio
#'
#' @description Adds a record for Waist-to-Height Ratio using Waist Circumference and Height
#' each by group (e.g., subject and visit) where the source parameters are available.
#'
#' **Note:** This is a wrapper function for the more generic \code{admiral::derive_param_ratio()}.
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the \code{by_vars} argument are expected to be in the dataset.
#'   \code{PARAMCD}, and \code{AVAL} are expected as well.
#'
#'   The variable specified by \code{by_vars} and \code{PARAMCD} must be a unique key of
#'   the input dataset after restricting it by the filter condition (\code{filter}
#'   parameter) and to the parameters specified by \code{wstcir_code} and \code{height_code}.
#'
#' @param wstcir_code Waist Circumference parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the Waist Circumference
#'
#'   *Permitted Values:* character value
#'
#' @param height_code Height parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the Height. It is expected that Height is measured in cm
#'
#'   *Permitted Values:* character value
#'
#' @param constant_by_vars By variables for when Height is constant
#'
#'   When Height is constant, the Height parameters (measured only once) are merged
#'   to the other parameters using the specified variables.
#'
#'   If Height is constant (e.g. only measured once at screening or baseline) then use
#'   \code{constant_by_vars} to select the subject-level variable to merge on (e.g. \code{USUBJID}).
#'   This will produce Waist-to-Height Ratio at all visits where Waist Circumference is measured.
#'   Otherwise it will only be calculated at visits with both Height and Waist Circumference
#'   collected.
#'
#'   *Permitted Values*: list of variables created by \code{exprs()}
#'   e.g. \code{exprs(USUBJID, VISIT)}
#'
#' @inheritParams admiral::derive_param_ratio
#'
#' @details
#' The analysis value of the new parameter is derived as
#' \deqn{WAISTHGT = \frac{WSTCIR}{HEIGHT}}
#'
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'    be populated in the new parameter rows if it is specified in \code{by_vars}.
#'
#' @export
#'
#' @seealso \code{\link[=admiral::derive_param_ratio]{admiral::derive_param_ratio()}},
#'          \code{\link[=admiral::compute_ratio]{admiral::compute_ratio()}}
#'
#' @examples
#'
#' library(tibble)
#'
#' # Example 1: Derive Waist-to-Height Ratio where Height is measured only once
#'
#' advs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
#'   "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
#'   "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3",
#' )
#'
#' derive_param_waisthgt(
#'   advs,
#'   by_vars = exprs(USUBJID, VISIT),
#'   wstcir_code = "WSTCIR",
#'   height_code = "HEIGHT",
#'   set_values_to = exprs(
#'     PARAMCD = "WAISTHGT",
#'     PARAM = "Waist-to-Height Ratio"
#'   ),
#'   constant_by_vars = exprs(USUBJID)
#' )
#'
#' # Example 2: Pediatric study where Height and Waist Circumference are measured multiple times
#'
#' advs <- tribble(
#'   ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
#'   "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
#'   "01-101-1001", "HEIGHT", "Height (cm)", 148, "cm", "WEEK 2",
#'   "01-101-1001", "HEIGHT", "Height (cm)", 149, "cm", "WEEK 3",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 100, "cm", "SCREENING",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 99, "cm", "WEEK 2",
#'   "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 98, "cm", "WEEK 3",
#'   "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
#'   "01-101-1002", "HEIGHT", "Height (cm)", 164, "cm", "WEEK 2",
#'   "01-101-1002", "HEIGHT", "Height (cm)", 165, "cm", "WEEK 3",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 109, "cm", "WEEK 2",
#'   "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 3"
#' )
#'
#' derive_param_waisthgt(
#'   advs,
#'   by_vars = exprs(USUBJID, VISIT),
#'   wstcir_code = "WSTCIR",
#'   height_code = "HEIGHT",
#'   set_values_to = exprs(
#'     PARAMCD = "WAISTHGT",
#'     PARAM = "Waist-to-Height Ratio"
#'   )
#' )
derive_param_waisthgt <- function(dataset,
                                  by_vars,
                                  wstcir_code = "WSTCIR",
                                  height_code = "HGHT",
                                  set_values_to = exprs(PARAMCD = "WAISTHGT"),
                                  filter = NULL,
                                  constant_by_vars = NULL) {
  assert_vars(by_vars)
  assert_data_frame(dataset, required_vars = exprs(!!!by_vars, PARAMCD, AVAL))
  assert_character_scalar(wstcir_code)
  assert_character_scalar(height_code)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)
  assert_vars(constant_by_vars, optional = TRUE)

  admiral::derive_param_ratio(
    dataset,
    filter = !!filter,
    dividend_code = wstcir_code,
    divisor_code = height_code,
    by_vars = by_vars,
    set_values_to = set_values_to,
    constant_dividend = FALSE,
    constant_divisor = !is.null(constant_by_vars),
    constant_by_vars = constant_by_vars
  )
}

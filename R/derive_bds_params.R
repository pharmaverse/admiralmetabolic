#' Adds a Ratio Parameter Computed from the Analysis Value of Other Parameters
#'
#' @description Adds a record for a generic Ratio parameter using two existing parameter
#' (dividend and divisor) each by group (e.g., subject and visit) where the source parameters
#' are available.
#'
#' **Note:** This is a wrapper function for the more generic \code{derive_param_computed()}
#'
#' @param dataset Input dataset
#'
#'   The variables specified by the \code{by_vars} argument are expected to be in the dataset.
#'   \code{PARAMCD}, and \code{AVAL} are expected as well.
#'
#'   The variable specified by \code{by_vars} and \code{PARAMCD} must be a unique key of
#'   the input dataset after restricting it by the filter condition (\code{filter}
#'   parameter) and to the parameters specified by \code{dividend_code} and \code{divisor_code}.
#'
#' @param dividend_code Dividend parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the dividend
#'
#'   *Permitted Values:* character value
#'
#' @param divisor_code Divisor parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the divisor
#'
#'   *Permitted Values:* character value
#'
#' @param constant_dividend Is dividend parameter constant?
#'
#'   It is expected that the parameter code (PARAMCD) specified in \code{dividend_code}
#'   which is required to derive the new parameter is measured only once. For example,
#'   if Height-to-Weight Ratio should be derived and height is measured only once
#'   while Weight is measured at each visit. Height could be specified
#'   in the \code{dividend_code} argument and \code{constant_dividend} is to be set to \code{TRUE}.
#'
#'   *Permitted Values:* logical scalar
#'
#' @param constant_divisor Is divisor parameter constant?
#'
#'   It is expected that the parameter code (PARAMCD) specified in \code{dividend_code}
#'   which is required to derive the new parameter is measured only once. For example,
#'   if Waist-to-Height Ratio should be derived and height is measured only once
#'   while Waist Circumference is measured at each visit. Height could be specified
#'   in the \code{divisor_code} argument and \code{constant_divisor} is to be set to \code{TRUE}.
#'
#'   *Permitted Values:* logical scalar
#'
#' @param constant_by_vars By variables for when dividend and/or divisor is constant
#'
#'   When dividend and/or divisor is constant, the parameters (measured only once) are merged
#'   to the other parameters using the specified variables.
#'
#'   If dividend and/or divisor is constant (e.g. only measured once at screening or baseline)
#'   then use \code{constant_by_vars} to select the subject-level variable to merge on
#'   (e.g. \code{USUBJID}). This will produce a generic Ratio parameter at all visits where dividend
#'   and/or divisor is measured. Otherwise it will only be calculated at visits with both dividend
#'   and divisor parameters collected.
#'
#'   *Permitted Values*: list of variables created by \code{exprs()}
#'   e.g. \code{exprs(USUBJID, VISIT)}
#'
#' @param get_unit_expr An expression providing the unit of the parameter
#'
#'   The result is used to check the units of the input parameters. If the units are not consistent
#'   within each parameter, an error will be thrown.
#'
#'   Additionally, if the input parameters are measured in different units but are mutually
#'   convertible (e.g., centimeters for one parameter and inches for another), an automatic
#'   conversion will be performed in order to uniform the values before calculating the ratio.
#'
#'   *Permitted Values:* A variable of the input dataset or a function call
#'
#' @inheritParams admiral::derive_param_bmi
#'
#' @details
#' The analysis value of the new parameter is derived as
#' \deqn{RATIO = \frac{DIVIDENT}{DIVISOR}}
#'
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'    be populated in the new parameter rows if it is specified in \code{by_vars}.
#'
#' @family internal
#' @keywords internal
derive_param_ratio <- function(dataset,
                               by_vars,
                               dividend_code,
                               divisor_code,
                               set_values_to,
                               constant_dividend = FALSE,
                               constant_divisor = FALSE,
                               filter = NULL,
                               constant_by_vars = NULL,
                               get_unit_expr = NULL) {
  assert_vars(by_vars)
  assert_data_frame(dataset, required_vars = exprs(!!!by_vars, PARAMCD, AVAL))
  assert_character_scalar(dividend_code)
  assert_character_scalar(divisor_code)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  assert_logical_scalar(constant_dividend)
  assert_logical_scalar(constant_divisor)
  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)
  assert_vars(constant_by_vars, optional = TRUE)
  get_unit_expr <- assert_expr(enexpr(get_unit_expr), optional = TRUE)

  if (!missing(get_unit_expr) && !is.null(get_unit_expr)) {
    # Check if units are the same within each parameter
    for (param in c(dividend_code, divisor_code)) {
      units <- dataset %>%
        mutate(`_unit` = !!get_unit_expr) %>%
        filter(PARAMCD == param) %>%
        pull(`_unit`) %>%
        unique()

      if (length(units) != 1L) {
        cli_abort(
          "Multiple units {.val {units}} found for {.val {param}}.
          Please review and update the units."
        )
      }
    }

    # Set units in AVAL context to enable automatic conversion when used in formula
    dataset <- dataset %>%
      mutate(
        AVAL = mixed_units(
          AVAL,
          !!get_unit_expr %>% if_else(is.na(.), "", .),
          mode = "standard"
        )
      )
  }

  ratio_formula <- expr(
    !!sym(paste0("AVAL.", dividend_code)) / !!sym(paste0("AVAL.", divisor_code))
  )

  parameters <- c(dividend_code, divisor_code)
  constant_parameters <- NULL

  if (constant_dividend) {
    constant_parameters <- c(constant_parameters, dividend_code)

    parameters <- parameters %>%
      .[!. == dividend_code]
  }

  if (constant_divisor) {
    constant_parameters <- c(constant_parameters, divisor_code)

    parameters <- parameters %>%
      .[!. == divisor_code] %>%
      ifelse(length(.) == 0, NULL, .)
  }

  result <- dataset %>%
    derive_param_computed(
      filter = !!filter,
      parameters = parameters,
      by_vars = by_vars,
      set_values_to = exprs(
        AVAL = !!ratio_formula,
        !!!set_values_to
      ),
      constant_parameters = constant_parameters,
      constant_by_vars = constant_by_vars
    )

  if ("mixed_units" %in% class(result$AVAL)) {
    # Drop units from AVAL context
    result$AVAL <- drop_units(result$AVAL)
  }

  result
}

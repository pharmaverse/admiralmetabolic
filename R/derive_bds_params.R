#' Adds a Ratio Parameter Computed from the Analysis Value of Other Parameters
#'
#' @description Adds a record for a generic Ratio parameter using two existing parameter
#' (numerator and denominator) each by group (e.g., subject and visit) where the source parameters
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
#'   the input dataset after restricting it by the filter condition (\code{filter} argument)
#'   and to the parameters specified by \code{numerator_code} and \code{denominator_code}.
#'
#' @param numerator_code Numerator parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the numerator
#'
#'   *Permitted Values:* character value
#'
#' @param denominator_code Denominator parameter code
#'
#'   The observations where \code{PARAMCD} equals the specified value are considered
#'   as the denominator
#'
#'   *Permitted Values:* character value
#'
#' @param constant_numerator Is numerator parameter constant?
#'
#'   It is expected that the parameter code (PARAMCD) specified in \code{numerator_code}
#'   which is required to derive the new parameter is measured only once. For example,
#'   if Height to Weight Ratio should be derived and height is measured only once
#'   while Weight is measured at each visit. Height could be specified in the
#'   \code{numerator_code} argument and \code{constant_numerator} is to be set to \code{TRUE}.
#'
#'   *Permitted Values:* logical scalar
#'
#' @param constant_denominator Is denominator parameter constant?
#'
#'   It is expected that the parameter code (PARAMCD) specified in \code{numerator_code}
#'   which is required to derive the new parameter is measured only once. For example,
#'   if Waist to Height Ratio should be derived and height is measured only once
#'   while Waist Circumference is measured at each visit. Height could be specified in the
#'   \code{denominator_code} argument and \code{constant_denominator} is to be set to \code{TRUE}.
#'
#'   *Permitted Values:* logical scalar
#'
#' @param constant_by_vars By variables for when numerator and/or denominator is constant
#'
#'   When numerator and/or denominator is constant, the parameters (measured only once) are merged
#'   to the other parameters using the specified variables.
#'
#'   If numerator and/or denominator is constant (e.g. only measured once at screening or baseline)
#'   then use \code{constant_by_vars} to select the subject-level variable to merge on (e.g.
#'   \code{USUBJID}). This will produce a generic Ratio parameter at all visits where numerator
#'   and/or denominator is measured. Otherwise it will only be calculated at visits with both
#'   numerator and denominator parameters collected.
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
#' \deqn{RATIO = \frac{NUMERATOR}{DENOMINATOR}}
#'
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'    be populated in the new parameter rows if it is specified in \code{by_vars}.
#'
#' @family internal
#' @keywords internal
derive_param_ratio <- function(dataset,
                               by_vars,
                               numerator_code,
                               denominator_code,
                               set_values_to,
                               constant_numerator = FALSE,
                               constant_denominator = FALSE,
                               filter = NULL,
                               constant_by_vars = NULL,
                               get_unit_expr = NULL) {
  assert_vars(by_vars)
  assert_data_frame(dataset, required_vars = exprs(!!!by_vars, PARAMCD, AVAL))
  assert_character_scalar(numerator_code)
  assert_character_scalar(denominator_code)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  assert_logical_scalar(constant_numerator)
  assert_logical_scalar(constant_denominator)
  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)
  assert_vars(constant_by_vars, optional = TRUE)
  get_unit_expr <- assert_expr(enexpr(get_unit_expr), optional = TRUE)

  ### Default formula with no units conversion applied ----

  ratio_formula <- expr(
    !!sym(paste0("AVAL.", numerator_code)) /
      !!sym(paste0("AVAL.", denominator_code))
  )

  ### If `get_unit_expr` provided then check units and enable units conversion ----

  if (!missing(get_unit_expr) && !is.null(get_unit_expr)) {
    param_units <- list()

    # Check if units are the same within each of two parameters

    for (param in c(numerator_code, denominator_code)) {
      units_found <- dataset %>%
        mutate(`_unit` = !!get_unit_expr) %>%
        filter(PARAMCD == param) %>%
        pull(`_unit`) %>%
        unique()

      if (length(units_found) != 1L) {
        cli_abort(
          "Multiple units {.val {units_found}} found for {.val {param}}.
          Please review and update the units."
        )
      }

      param_units[[param]] <- units_found
    }

    # If the input parameters are measured in different units
    # but are convertible from one to another (and this kind of conversion supported)
    # then modify the formula in order to perform units conversion on the fly

    if (param_units[[denominator_code]] != param_units[[numerator_code]]) {
      # Find conversion factor for denominator
      conv_factor <- get_conv_factor(
        param_units[[denominator_code]],
        param_units[[numerator_code]]
      )

      if (!is.na(conv_factor)) {
        ratio_formula <- expr(
          !!sym(paste0("AVAL.", numerator_code)) / (
            !!sym(paste0("AVAL.", denominator_code)) * !!conv_factor
          )
        )

        cli_alert_info(
          "Unit conversion performed for {.val {denominator_code}}. Values converted from
          {.val {param_units[[denominator_code]]}} to {.val {param_units[[numerator_code]]}}.",
          wrap = TRUE
        )
      }
    }
  }

  ### Identify constant parameters ----

  parameters <- c(numerator_code, denominator_code)
  constant_parameters <- NULL

  if (constant_numerator) {
    constant_parameters <- c(constant_parameters, numerator_code)

    parameters <- parameters %>%
      .[!. == numerator_code]
  }

  if (constant_denominator) {
    constant_parameters <- c(constant_parameters, denominator_code)

    parameters <- parameters %>%
      .[!. == denominator_code] %>%
      ifelse(length(.) == 0, NULL, .)
  }

  ### Call the core {admiral} function to derive Ratio parameter ----

  derive_param_computed(
    dataset,
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
}

get_conv_factor <- function(from_unit, to_unit) {
  # Get all conversion factors supported
  conv_factors_all <- get_conv_factors_all()

  # Return conversion factor if units are supported and convertible
  for (unit_category in names(conv_factors_all)) {
    if (all(c(from_unit, to_unit) %in% names(conv_factors_all[[unit_category]]))) {
      return(
        conv_factors_all[[unit_category]][[from_unit]] /
          conv_factors_all[[unit_category]][[to_unit]]
      )
    }
  }

  # If units are not supported/convertible
  return(NA_real_)
}

get_conv_factors_all <- function() {
  list(
    # Conversion factors for length relative to centimeters
    length = list(
      "cm" = 1,
      "m" = 100,
      "mm" = 0.1,
      "in" = 2.54,
      "ft" = 30.48
    )
  )
}

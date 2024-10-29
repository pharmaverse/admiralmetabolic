#' Asserts That a Parameter is Provided in One of the Expected Units
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is to be *deprecated*. Please use `admiraldev::assert_unit()` instead
#' once https://github.com/pharmaverse/admiraldev/issues/468 is closed.
#'
#' @inherit admiraldev::assert_unit
#'
#' @seealso [admiraldev::assert_unit]
#'
#' @examples
#' # See examples of `admiraldev::assert_unit`
#'
#' @family internal deprecated
#' @keywords internal deprecated
assert_unit <- function(dataset,
                        param,
                        required_unit,
                        get_unit_expr,
                        arg_name = rlang::caller_arg(required_unit),
                        message = NULL,
                        class = "assert_unit",
                        call = parent.frame()) {
  assert_data_frame(dataset, required_vars = exprs(PARAMCD))
  assert_character_scalar(param)
  assert_character_vector(required_unit)
  get_unit_expr <- enexpr(get_unit_expr)

  units <- dataset %>%
    mutate(tmp_unit = !!get_unit_expr) %>%
    filter(PARAMCD == param & !is.na(.data$tmp_unit)) %>%
    pull(.data$tmp_unit) %>%
    unique()

  if (length(units) != 1L) {
    message <-
      message %||%
      "Multiple units {.val {units}} found for {.val {param}}. Please review and update the units."

    cli_abort(
      message = message,
      call = call,
      class = c(class, "assert-admiraldev")
    )
  }

  if (tolower(units) %notin% tolower(required_unit)) {
    message <-
      message %||%
      "It is expected that {.val {param}} has unit of {.or {required_unit}}.
       In the input dataset the unit is {.val {units}}."

    cli_abort(
      message = message,
      call = call,
      class = c(class, "assert-admiraldev")
    )
  }

  invisible(dataset)
}

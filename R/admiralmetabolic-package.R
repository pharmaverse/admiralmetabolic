#' @keywords internal
#' @family internal
#' @importFrom dplyr arrange bind_rows case_when desc ends_with filter full_join group_by
#'             if_else mutate mutate_at mutate_if n pull rename rename_at row_number select slice
#'             starts_with transmute ungroup vars n_distinct union distinct
#'             summarise_at summarise coalesce bind_cols na_if tibble
#' @importFrom magrittr %>%
#' @importFrom rlang := abort arg_match as_function as_string call2 caller_env
#'             call_name current_env .data enexpr enquo eval_bare eval_tidy expr
#'             expr_interp expr_label f_lhs f_rhs inform
#'             is_bare_formula is_call is_character is_formula is_integerish
#'             is_logical is_quosure is_quosures is_symbol new_formula
#'             parse_expr parse_exprs quo quo_get_expr quo_is_call
#'             quo_is_missing quo_is_null quo_is_symbol quos quo_squash quo_text
#'             set_names sym syms type_of warn quo_set_env quo_get_env
#' @importFrom utils capture.output str
#' @importFrom stringr str_c str_detect str_extract str_remove str_remove_all
#'             str_replace str_trim str_to_lower str_to_title str_to_upper str_glue
#' @importFrom lubridate as_datetime ceiling_date date days duration floor_date is.Date is.instant
#'             time_length %--% ymd ymd_hms weeks years hours minutes
#' @importFrom tidyselect all_of contains vars_select
#' @importFrom lifecycle deprecate_warn deprecated deprecate_stop
"_PACKAGE"

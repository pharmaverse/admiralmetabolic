#' Adds a parameter for Glycemic Status
#'
#' @description
#' Derives a new parameter using HbA1c and Fasting Plasma Glucose that represents a glycemic status
#' each by group (e.g., subject and visit) where the source parameters are available.
#'
#' @param dataset
#'  `r admiral:::roxygen_param_dataset(expected_vars = c("by_vars"))`
#'  `PARAMCD`, and `AVAL` are expected as well.
#'
#'   The variable specified by `by_vars` and `order` as well as `PARAMCD` must be
#'   a unique key of the input dataset after restricting it by the filter condition
#'   (`filter` argument) and to the parameters specified by `hba1c_code` and `fpg_code`.
#'
#' @param filter Filter condition
#'
#'   The specified condition is applied to the input dataset before deriving the
#'   new parameter, i.e., only observations fulfilling the condition are taken
#'   into account.
#'
#' @permitted [condition]
#'
#' @param by_vars Grouping variables
#'
#'   Only variables specified in `by_vars` will be populated
#'   in the newly created records.
#'
#'   `r admiral:::roxygen_param_by_vars()`
#'
#' @permitted [var_list]
#'
#' @param order Sort order
#'
#'   The observations are ordered by the specified order while looking for
#'   consecutive results fulfilling the criteria to confirm a DIABETIC status.
#'
#'   If an expression is named, e.g., `exprs(LBDT = convert_dtc_to_dt(LBDTC), LBSEQ)`,
#'   a corresponding variable (`LBDT`) is added to the additional dataset and used
#'   for sorting. This variable is not included in the output dataset.
#'
#'   `r admiral:::roxygen_order_na_handling()`
#'
#' @permitted [var_list]
#'
#' @param hba1c_code HbA1c parameter code
#'
#'   The observations where `PARAMCD` equals the specified value are considered
#'   as the HbA1c. It is expected that HbA1c is measured in mmol/mol or %.
#'
#' @permitted character value
#'
#' @param fpg_code Fasting Plasma Glucose parameter code
#'
#'   The observations where `PARAMCD` equals the specified value are considered
#'   as the Fasting Plasma Glucose. It is expected that Fasting Plasma Glucose
#'   is measured in mmol/L or mg/dL.
#'
#' @permitted character value
#'
#' @param set_values_to Variables to be set
#'
#'   The specified variables are set to the specified values for the new
#'   observations. For example `exprs(PARAMCD = "GLYCSTT")` defines the parameter code
#'   for the new parameter.
#'
#' @permitted List of variable-value pairs
#'
#' @param get_unit_expr An expression providing the unit of the parameter
#'
#'   The result is used to check the units of the input parameters.
#'
#' @permitted An expression which is evaluable in the input dataset
#'   and results in a character value
#'
#' @param prediabetic_thresholds,diabetic_thresholds
#'   Criteria for prediabetic and diabetic statuses
#'
#'   The specified thresholds are used to identify HbA1c and Fasting Plasma Glucose
#'   results that corresponds to a certain glycemic status.
#'
#'   *Diabetic:*
#'   - HbA1c: >=6.5% (48 mmol/mol)
#'   - Fasting Plasma Glucose: >=126 mg/dL (7.0 mmol/L)
#'
#'   *Prediabetic:*
#'   - HbA1c: >=5.7% (39 mmol/mol) but less than the threshold for *Diabetic*
#'   - Fasting Plasma Glucose: >=100 mg/dL (5.6 mmol/L)
#'
#'   *Normoglycemic:*
#'   - Any values not fulfilling the criteria above
#'
#'   Normally the criteria don’t need to be changed, but in case they do,
#'   it can be done as follows:
#'
#'   ```
#'   prediabetic_thresholds = list(
#'     HBA1CHGB = xx.x,
#'     GLUC = xx.x
#'   ),
#'   diabetic_thresholds = list(
#'     HBA1CHGB = xx.x,
#'     GLUC = xx.x
#'   )
#'   ```
#'
#'   where `xx.x` is a numeric threshold and names of the elements must match
#'   `hba1c_code` and `fpg_code`.
#'
#' @permitted List of parameter-value pairs
#'
#' @param subject_keys Variables to uniquely identify a subject
#'
#'   A list of expressions where the expressions are symbols as returned by
#'   `exprs()` is expected.
#'
#' @details
#' Implementation is based on ADA 2024.
#'
#' @return The input dataset with the new parameter added. Note, a variable will only
#'   be populated in the new parameter rows if it is specified in `by_vars`.
#'
#' Possible resulting values (AVALC) are:
#'
#' - NORMOGLYCEMIC
#' - PREDIABETIC
#' - DIABETIC
#'
#' @family der_prm_adlb
#' @keywords der_prm_adlb
#'
#' @export
#'
#' @examples
#' library(tibble)
#' library(rlang)
#'
#' adlb <- tribble(
#'   ~STUDYID, ~USUBJID, ~PARAMCD, ~AVISIT, ~AVISITN, ~AVAL, ~AVALU,
#'
#'   # Subject 1 - From Normoglycemic at BL to Prediabetic at Week 4 onwards
#'
#'   "STUDY01", "SUBJ001", "HBA1C", "BASELINE", 0, 5.4, "%",
#'   "STUDY01", "SUBJ001", "HBA1C", "WEEK 4", 4, 5.8, "%",
#'   "STUDY01", "SUBJ001", "HBA1C", "WEEK 8", 8, 6.1, "%",
#'   "STUDY01", "SUBJ001", "HBA1C", "WEEK 12", 12, 6.4, "%",
#'   "STUDY01", "SUBJ001", "FPG", "BASELINE", 0, 90, "mg/dL",
#'   "STUDY01", "SUBJ001", "FPG", "WEEK 4", 4, 110, "mg/dL",
#'   "STUDY01", "SUBJ001", "FPG", "WEEK 8", 8, 115, "mg/dL",
#'   "STUDY01", "SUBJ001", "FPG", "WEEK 12", 12, 120, "mg/dL",
#'
#'   # Subject 2 - From Normoglycemic at BL to Diabetic at Week 12
#'
#'   "STUDY01", "SUBJ002", "HBA1C", "BASELINE", 0, 5.2, "%",
#'   "STUDY01", "SUBJ002", "HBA1C", "WEEK 4", 4, 5.6, "%",
#'   "STUDY01", "SUBJ002", "HBA1C", "WEEK 8", 8, 6.4, "%",
#'   "STUDY01", "SUBJ002", "HBA1C", "WEEK 12", 12, 7.0, "%",
#'   "STUDY01", "SUBJ002", "FPG", "BASELINE", 0, 88, "mg/dL",
#'   "STUDY01", "SUBJ002", "FPG", "WEEK 4", 4, 100, "mg/dL",
#'   "STUDY01", "SUBJ002", "FPG", "WEEK 8", 8, 120, "mg/dL",
#'   "STUDY01", "SUBJ002", "FPG", "WEEK 12", 12, 130, "mg/dL",
#'
#'   # Subject 3 - From Prediabetic at BL to Diabetic at Week 8 onwards
#'
#'   "STUDY01", "SUBJ003", "HBA1C", "BASELINE", 0, 6.0, "%",
#'   "STUDY01", "SUBJ003", "HBA1C", "WEEK 4", 4, 6.2, "%",
#'   "STUDY01", "SUBJ003", "HBA1C", "WEEK 8", 8, 6.7, "%",
#'   "STUDY01", "SUBJ003", "HBA1C", "WEEK 12", 12, 7.2, "%",
#'   "STUDY01", "SUBJ003", "FPG", "BASELINE", 0, 105, "mg/dL",
#'   "STUDY01", "SUBJ003", "FPG", "WEEK 4", 4, 112, "mg/dL",
#'   "STUDY01", "SUBJ003", "FPG", "WEEK 8", 8, 130, "mg/dL",
#'   "STUDY01", "SUBJ003", "FPG", "WEEK 12", 12, 145, "mg/dL"
#' )
#'
#' derive_param_glycstt(
#'   dataset = adlb,
#'   by_vars = exprs(STUDYID, USUBJID, AVISITN, AVISIT),
#'   order = exprs(AVISITN),
#'   set_values_to = exprs(
#'     PARAMCD = "GLYCSTT"
#'   ),
#'   hba1c_code = "HBA1C",
#'   fpg_code = "FPG",
#'   get_unit_expr = AVALU
#' )
derive_param_glycstt <- function(dataset,
                                 filter = NULL,
                                 by_vars,
                                 order,
                                 set_values_to = exprs(PARAMCD = "GLYCSTT"),
                                 hba1c_code = "HBA1CHGB",
                                 fpg_code = "GLUC",
                                 get_unit_expr,
                                 prediabetic_thresholds = rlang::list2(
                                   !!hba1c_code := c("mmol/mol" = 39, "%" = 5.7),
                                   !!fpg_code := c("mmol/L" = 5.6, "mg/dL" = 100)
                                 ),
                                 diabetic_thresholds = rlang::list2(
                                   !!hba1c_code := c("mmol/mol" = 48, "%" = 6.5),
                                   !!fpg_code := c("mmol/L" = 7, "mg/dL" = 126)
                                 ),
                                 subject_keys = get_admiral_option("subject_keys")) {
  # Assertions ----

  assert_vars(subject_keys)
  assert_data_frame(dataset, required_vars = subject_keys)

  assert_vars(by_vars)
  assert_expr_list(order)

  assert_data_frame(
    dataset,
    required_vars = expr_c(
      by_vars,
      extract_vars(order),
      exprs(PARAMCD, AVAL)
    )
  )

  filter <- assert_filter_cond(enexpr(filter), optional = TRUE)

  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_param_does_not_exist(dataset, set_values_to$PARAMCD)
  assert_character_scalar(hba1c_code)
  assert_character_scalar(fpg_code)

  get_unit_expr <- assert_expr(enexpr(get_unit_expr))

  hba1c_supported_units <- c("mmol/mol", "%")
  fpg_supported_units <- c("mmol/L", "mg/dL")

  hba1c_units <- dataset %>%
    assert_unit(
      param = hba1c_code,
      required_unit = hba1c_supported_units,
      get_unit_expr = !!get_unit_expr
    ) %>%
    filter(PARAMCD == hba1c_code) %>%
    pull(!!get_unit_expr) %>%
    unique()

  fpg_units <- dataset %>%
    assert_unit(
      param = fpg_code,
      required_unit = fpg_supported_units,
      get_unit_expr = !!get_unit_expr
    ) %>%
    filter(PARAMCD == fpg_code) %>%
    pull(!!get_unit_expr) %>%
    unique()

  assert_thresholds <- function(thresholds,
                                arg_name = rlang::caller_arg(thresholds),
                                class = "assert_thresholds",
                                call = parent.frame()) {
    assert_list_of(
      thresholds, "numeric",
      named = TRUE,
      arg_name = arg_name,
      class = class,
      call = call
    )

    missing_elements <- setdiff(c(hba1c_code, fpg_code), names(thresholds))

    if (length(missing_elements) >= 1L) {
      cli_abort(
        message =
          "The following required elements are missing from argument
          {.arg {arg_name}}: {.val {missing_elements}}.",
        class = class,
        call = call
      )
    }

    for (param in names(thresholds)) {
      if (is.null(names(thresholds[[param]]))) {
        next
      }

      unit_choices <- case_when(
        param == hba1c_code ~ hba1c_units,
        param == fpg_code ~ fpg_units
      )

      missing_elements <- setdiff(unit_choices, names(thresholds[[param]]))

      if (length(missing_elements) >= 1L) {
        cli_abort(
          message =
            "The following required elements are missing from {.arg {param}} child
            of {.arg {arg_name}} argument: {.val {unit_choices}}.",
          class = class,
          call = call
        )
      }
    }

    invisible(thresholds)
  }

  assert_thresholds(prediabetic_thresholds)
  assert_thresholds(diabetic_thresholds)

  # Initial derivation assigning either NORMOGLYCEMIC or PREDIABETIC status ----

  dataset <- dataset %>%
    mutate(
      # Map `prediabetic_thresholds` into a variable
      PRED_THRESHOLD = purrr::map2_dbl(
        PARAMCD, !!get_unit_expr,
        \(x, y) {
          purrr::pluck(prediabetic_thresholds, x, y) %||%
            purrr::pluck(prediabetic_thresholds, x, .default = NA_real_)
        }
      )
    ) %>%
    derive_param_computed(
      by_vars = by_vars,
      parameters = c(hba1c_code, fpg_code),
      set_values_to = exprs(
        AVALC = case_when(
          (
            !!sym(paste0("AVAL.", hba1c_code)) >= !!sym(paste0("PRED_THRESHOLD.", hba1c_code)) |
              !!sym(paste0("AVAL.", fpg_code)) >= !!sym(paste0("PRED_THRESHOLD.", fpg_code))
          )
          ~ "PREDIABETIC",
          (
            !!sym(paste0("AVAL.", hba1c_code)) < !!sym(paste0("PRED_THRESHOLD.", hba1c_code)) |
              !!sym(paste0("AVAL.", fpg_code)) < !!sym(paste0("PRED_THRESHOLD.", fpg_code))
          )
          ~ "NORMOGLYCEMIC",
          .default = "UNKNOWN"
        ),
        !!!set_values_to
      ),
      keep_nas = TRUE
    ) %>%
    select(-PRED_THRESHOLD)

  # Identify records that meet the criteria for DIABETIC status ----

  # Diabetic status requires a confirmation, so that there should be
  # two consecutive time points where the results meet the criteria.

  diabetes_confirmed <- dataset %>%
    filter(
      PARAMCD %in% c(hba1c_code, fpg_code)
    ) %>%
    mutate(
      # Map `diabetic_thresholds` into a variable
      D_THRESHOLD = purrr::map2_dbl(
        PARAMCD, !!get_unit_expr,
        \(x, y) {
          purrr::pluck(diabetic_thresholds, x, y) %||%
            purrr::pluck(diabetic_thresholds, x, .default = NA_real_)
        }
      ),
      # Change PARAMCD values for convenience
      PARAMCD = case_match(
        PARAMCD,
        {{ hba1c_code }} ~ "HBA1C",
        {{ fpg_code }} ~ "FPG"
      )
    ) %>%
    # Transpose the results and thresholds to a wide structure
    tidyr::pivot_wider(
      id_cols = vars2chr(by_vars),
      names_from = "PARAMCD",
      names_glue = "{PARAMCD}.{.value}",
      values_from = c("AVAL", "D_THRESHOLD")
    ) %>%
    # Identify cases where the criteria is met twice consecutively
    derive_var_joined_exist_flag(
      dataset = .,
      dataset_add = .,
      by_vars = subject_keys,
      new_var = DIABETES_CONFIRMED,
      tmp_obs_nr_var = TMP_OBS_NR,
      join_vars = exprs(HBA1C.AVAL, FPG.AVAL),
      join_type = "all",
      order = order,
      filter_join = (
        # Check only consecutive results
        TMP_OBS_NR == TMP_OBS_NR.join + 1 &
          (
            # Both HbA1c and FPG are above the threshold at the same time point
            HBA1C.AVAL >= HBA1C.D_THRESHOLD & FPG.AVAL >= FPG.D_THRESHOLD
            # Or there are 2 consecutive HbA1c above the threshold
            | HBA1C.AVAL >= HBA1C.D_THRESHOLD & HBA1C.AVAL.join >= HBA1C.D_THRESHOLD
            # Or there are 2 consecutive FPG above the threshold
            | FPG.AVAL >= FPG.D_THRESHOLD & FPG.AVAL.join >= FPG.D_THRESHOLD
            # Or there is HbA1C following by FPG, both above the threshold
            | HBA1C.AVAL >= HBA1C.D_THRESHOLD & FPG.AVAL.join >= FPG.D_THRESHOLD
            # Or there is FPG following by HbA1C, both above the threshold
            | FPG.AVAL >= FPG.D_THRESHOLD & HBA1C.AVAL.join >= HBA1C.D_THRESHOLD
          )
      )
    )

  # Adjust initially derived parameter assigning DIABETIC status where criteria have been met ----

  dataset <- dataset %>%
    restrict_derivation(
      derivation = derive_vars_merged,
      args = params(
        dataset_add = diabetes_confirmed,
        by_vars = by_vars,
        new_vars = exprs(DIABETES_CONFIRMED),
      ),
      filter = PARAMCD == "GLYCSTT"
    ) %>%
    mutate(
      AVALC = if_else(
        PARAMCD == "GLYCSTT" & DIABETES_CONFIRMED == "Y",
        "DIABETIC", AVALC, AVALC
      )
    ) %>%
    select(-DIABETES_CONFIRMED)

  invisible(dataset)
}

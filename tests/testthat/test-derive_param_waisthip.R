test_that(
  "derive_param_waisthip Test 1: Cross-check with admiral::derive_param_computed()",
  {
    input <- tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 125, "cm", "SCREENING",
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 124, "cm", "WEEK 2",
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 123, "cm", "WEEK 3",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 135, "cm", "SCREENING",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 133, "cm", "WEEK 2",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 132, "cm", "WEEK 3",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3"
    )

    wrapper_output <- derive_param_waisthip(
      input,
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      ),
      get_unit_expr = admiral::extract_unit(PARAM)
    )

    expected_output <- derive_param_computed(
      input,
      parameters = c("WSTCIR", "HIPCIR"),
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.WSTCIR / AVAL.HIPCIR,
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      )
    )

    expect_dfs_equal(
      wrapper_output,
      expected_output,
      keys = c("USUBJID", "PARAMCD", "VISIT")
    )
  }
)

test_that(
  "derive_param_waisthip Test 2: Cross-check with and without units conversion",
  {
    input_diff_units <- tribble(
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

    output_units_unified <- derive_param_waisthip(
      input_diff_units,
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      ),
      get_unit_expr = admiral::extract_unit(PARAM)
    ) %>%
      filter(PARAMCD == "WAISTHIP")

    input_same_units <- tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 125, "cm", "SCREENING",
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 124, "cm", "WEEK 2",
      "01-101-1001", "HIPCIR", "Hip Circumference (cm)", 123, "cm", "WEEK 3",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 135, "cm", "SCREENING",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 133, "cm", "WEEK 2",
      "01-101-1002", "HIPCIR", "Hip Circumference (cm)", 132, "cm", "WEEK 3",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3"
    )

    expected_output <- derive_param_waisthip(
      input_same_units,
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      ),
      get_unit_expr = admiral::extract_unit(PARAM)
    ) %>%
      filter(PARAMCD == "WAISTHIP")

    expect_dfs_equal(
      output_units_unified,
      expected_output,
      keys = c("USUBJID", "PARAMCD", "VISIT"),
      tolerance = 0.0001
    )
  }
)

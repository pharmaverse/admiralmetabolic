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
        PARAM = "Waist-to-Hip Ratio"
      )
    )

    expected_output <- derive_param_computed(
      input,
      parameters = c("WSTCIR", "HIPCIR"),
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.WSTCIR / AVAL.HIPCIR,
        PARAMCD = "WAISTHIP",
        PARAM = "Waist-to-Hip Ratio"
      )
    )

    expect_dfs_equal(
      wrapper_output,
      expected_output,
      keys = c("USUBJID", "PARAMCD", "VISIT")
    )
  }
)

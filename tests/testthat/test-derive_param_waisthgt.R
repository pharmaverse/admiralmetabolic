test_that(
  "derive_param_waisthgt Test 1: Cross-check with admiral::derive_param_computed(),
  new observations with constant Height",
  {
    input <- tribble(
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

    wrapper_output <- derive_param_waisthgt(
      input,
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        PARAMCD = "WAISTHGT",
        PARAM = "Waist-to-Height Ratio"
      ),
      constant_by_vars = exprs(USUBJID)
    )

    expected_output <- derive_param_computed(
      input,
      parameters = "WSTCIR",
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.WSTCIR / AVAL.HEIGHT,
        PARAMCD = "WAISTHGT",
        PARAM = "Waist-to-Height Ratio"
      ),
      constant_parameters = "HEIGHT",
      constant_by_vars = exprs(USUBJID)
    )

    expect_dfs_equal(
      wrapper_output,
      expected_output,
      keys = c("USUBJID", "PARAMCD", "VISIT")
    )
  }
)

test_that(
  "derive_param_waisthgt Test 2: Cross-check with admiral::derive_param_computed(),
  pediatric study where Height is measured multiple times",
  {
    input <- tribble(
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

    wrapper_output <- derive_param_waisthgt(
      input,
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        PARAMCD = "WAISTHGT",
        PARAM = "Waist-to-Height Ratio"
      )
    )

    expected_output <- derive_param_computed(
      input,
      parameters = c("WSTCIR", "HEIGHT"),
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.WSTCIR / AVAL.HEIGHT,
        PARAMCD = "WAISTHGT",
        PARAM = "Waist-to-Height Ratio"
      )
    )

    expect_dfs_equal(
      wrapper_output,
      expected_output,
      keys = c("USUBJID", "PARAMCD", "VISIT")
    )
  }
)

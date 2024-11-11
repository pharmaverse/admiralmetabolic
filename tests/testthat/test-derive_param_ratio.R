test_that(
  "derive_param_ratio Test 1: Cross-check with derive_param_computed()",
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

    wrapper_output <- derive_param_ratio(
      input,
      by_vars = exprs(USUBJID, VISIT),
      numerator_code = "WSTCIR",
      denominator_code = "HIPCIR",
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      )
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
  "derive_param_computed Test 2: Cross-check with derive_param_computed(),
  new observations with constant denominator",
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

    wrapper_output <- derive_param_ratio(
      input,
      by_vars = exprs(USUBJID, VISIT),
      numerator_code = "WSTCIR",
      denominator_code = "HEIGHT",
      set_values_to = exprs(
        PARAMCD = "WAISTHGT",
        PARAM = "Waist to Height Ratio"
      ),
      constant_denominator = TRUE,
      constant_by_vars = exprs(USUBJID)
    )

    expected_output <- derive_param_computed(
      input,
      parameters = "WSTCIR",
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.WSTCIR / AVAL.HEIGHT,
        PARAMCD = "WAISTHGT",
        PARAM = "Waist to Height Ratio"
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
  "derive_param_computed Test 3: Cross-check with derive_param_computed(),
  new observations with constant numerator",
  {
    input <- tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
      "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
      "01-101-1001", "WEIGHT", "Weight (kg)", 95, "kg", "SCREENING",
      "01-101-1001", "WEIGHT", "Weight (kg)", 94.5, "kg", "WEEK 2",
      "01-101-1001", "WEIGHT", "Weight (kg)", 94, "kg", "WEEK 3",
      "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
      "01-101-1002", "WEIGHT", "Weight (kg)", 105, "kg", "SCREENING",
      "01-101-1002", "WEIGHT", "Weight (kg)", 104.5, "kg", "WEEK 2",
      "01-101-1002", "WEIGHT", "Weight (kg)", 104, "kg", "WEEK 3"
    )

    wrapper_output <- derive_param_ratio(
      input,
      by_vars = exprs(USUBJID, VISIT),
      numerator_code = "HEIGHT",
      denominator_code = "WEIGHT",
      set_values_to = exprs(
        PARAMCD = "HGTWGT",
        PARAM = "Height to Weight Ratio"
      ),
      constant_numerator = TRUE,
      constant_by_vars = exprs(USUBJID)
    )

    expected_output <- derive_param_computed(
      input,
      parameters = "WEIGHT",
      by_vars = exprs(USUBJID, VISIT),
      set_values_to = exprs(
        AVAL = AVAL.HEIGHT / AVAL.WEIGHT,
        PARAMCD = "HGTWGT",
        PARAM = "Height to Weight Ratio"
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
  "derive_param_ratio Test 4: Cross-check with and without units conversion",
  {
    input_diff_units <- tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
      "01-101-1001", "HIPCIR", "Hip Circumference (in)", round(125 / 2.54, 2), "in", "SCREENING",
      "01-101-1001", "HIPCIR", "Hip Circumference (in)", round(124 / 2.54, 2), "in", "WEEK 2",
      "01-101-1001", "HIPCIR", "Hip Circumference (in)", round(123 / 2.54, 2), "in", "WEEK 3",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 108, "cm", "WEEK 2",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 107, "cm", "WEEK 3",
      "01-101-1002", "HIPCIR", "Hip Circumference (in)", round(135 / 2.54, 2), "in", "SCREENING",
      "01-101-1002", "HIPCIR", "Hip Circumference (in)", round(133 / 2.54, 2), "in", "WEEK 2",
      "01-101-1002", "HIPCIR", "Hip Circumference (in)", round(132 / 2.54, 2), "in", "WEEK 3",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 118, "cm", "WEEK 2",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 117, "cm", "WEEK 3"
    )

    output_units_unified <- derive_param_ratio(
      input_diff_units,
      by_vars = exprs(USUBJID, VISIT),
      numerator_code = "WSTCIR",
      denominator_code = "HIPCIR",
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      ),
      get_unit_expr = admiral::extract_unit(PARAM),
      unit_conversion = TRUE
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

    expected_output <- derive_param_ratio(
      input_same_units,
      by_vars = exprs(USUBJID, VISIT),
      numerator_code = "WSTCIR",
      denominator_code = "HIPCIR",
      set_values_to = exprs(
        PARAMCD = "WAISTHIP",
        PARAM = "Waist to Hip Ratio"
      )
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

test_that(
  "derive_param_ratio Test 5: Both input parameters are constant",
  {
    input <- tribble(
      ~USUBJID, ~PARAMCD, ~PARAM, ~AVAL, ~AVALU, ~VISIT,
      "01-101-1001", "HEIGHT", "Height (cm)", 147, "cm", "SCREENING",
      "01-101-1001", "WSTCIR", "Waist Circumference (cm)", 110, "cm", "SCREENING",
      "01-101-1002", "HEIGHT", "Height (cm)", 163, "cm", "SCREENING",
      "01-101-1002", "WSTCIR", "Waist Circumference (cm)", 120, "cm", "SCREENING"
    )

    expect_error(
      derive_param_ratio(
        input,
        by_vars = exprs(USUBJID, VISIT),
        numerator_code = "WSTCIR",
        denominator_code = "HEIGHT",
        set_values_to = exprs(
          PARAMCD = "WAISTHGT",
          PARAM = "Waist to Height Ratio"
        ),
        constant_numerator = TRUE,
        constant_denominator = TRUE,
        constant_by_vars = exprs(USUBJID)
      )
    )
  }
)

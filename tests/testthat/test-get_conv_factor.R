test_that("get_conv_factor Test 1: Direct conversion factor for length (to cm)", {
  expect_equal(
    get_conv_factor("in", "cm"),
    2.54
  )
})

test_that("get_conv_factor Test 2: Indirect conversion factor for length (via cm)", {
  expect_equal(
    get_conv_factor("ft", "in"),
    12
  )
})

test_that("get_conv_factor Test 3: Inconvertible units", {
  expect_error(
    get_conv_factor("cm", "kg")
  )
})

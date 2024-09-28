test_that("compute_ratio Test 1: Basic", {
  expect_equal(
    compute_ratio(
      x = c(182, 50.25),
      y = c(91, 201)
    ),
    c(2, 0.25)
  )
})

test_that("compute_ratio Test 2: Negatives", {
  expect_equal(
    compute_ratio(
      x = c(-63.9, 100),
      y = c(21.3, -40)
    ),
    c(-3, -2.5)
  )
})

test_that("compute_ratio Test 3: Zeros", {
  expect_equal(
    compute_ratio(
      x = c(0, 95, 0),
      y = c(75, 0, 0)
    ),
    c(0, NA_real_, NA_real_)
  )
})

test_that("compute_ratio Test 4: NAs", {
  expect_equal(
    compute_ratio(
      x = c(44, NA, NA),
      y = c(NA, 140, NA)
    ),
    c(NA_real_, NA_real_, NA_real_)
  )
})

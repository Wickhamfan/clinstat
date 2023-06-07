test_that("round0 Keep the decimal places", {
  expect_equal(round0(1.999, 2), "2.00")
})

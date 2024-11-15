test_that("nondecreasing works", {
  expect_equal(
    make_nondecreasing(c(1, 2, 3, 2.5, 3.5, 4)),
    c(1, 2, 3, 3, 3.5, 4)
  )

  # you should never exceed the original series
  expect_equal(
    make_nondecreasing(c(1, 2, 3, 2.0, 2.5, 2.0, 2.5, 2.0, 2.5)),
    c(1, 2, 3, 3, 3, 3, 3, 3, 3)
  )
})

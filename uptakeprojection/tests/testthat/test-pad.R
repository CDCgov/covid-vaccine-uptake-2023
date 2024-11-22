test_that("pad pads", {
  expect_equal(pad(1:3, 5, 4), c(1, 2, 3, 4, 4))
})

test_that("pad fails", {
  expect_error(pad(1:10, 5, 4))
})

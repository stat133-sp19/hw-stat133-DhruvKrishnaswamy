context("Bin Choose")
library(binomial)
test_that("Bin Choose", {
  expect_equal(bin_choose(5, 3), 10)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
  expect_equal(bin_choose(5, 5), 1)
}
)

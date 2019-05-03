context("Aux skewness")
library(binomial)
test_that("Aux skewness", {
  expect_equal(aux_skewness(2, 0.5), 0)
  expect_equal(aux_skewness(5, 0.5), 0)
  expect_equal(aux_skewness(10, 0.1), 0.843274, tolerance = 10 ^ -7)
}
)

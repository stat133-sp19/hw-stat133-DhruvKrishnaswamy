context("AuxKurtosis")
library(binomial)
test_that("Aux kurtosis",
          {
  expect_equal(aux_kurtosis(2, 0.5), -1)
  expect_equal(aux_kurtosis(5, 0.5), -0.4)
  expect_equal(aux_kurtosis(10, 0.3), -0.1238095, tolerance = 10 ^ -7)
}
)

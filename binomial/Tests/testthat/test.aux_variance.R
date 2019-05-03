context("Aux variance")
library(binomial)
test_that("Aux variance", {
  expect_equal(aux_variance(2, 0.5), 0.5)
  expect_equal(aux_variance(5, 0.5), 1.25)
  expect_equal(aux_variance(6, 0.5), 1.5)
}
)

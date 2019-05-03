context("Aux mean")
library(binomial)
test_that("Aux mean", {
  expect_equal(aux_mean(2, 0.5), 1)
  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_equal(aux_mean(6, 0.5), 3)
}
)

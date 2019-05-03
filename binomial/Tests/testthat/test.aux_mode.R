context("Aux mode")
library(binomial)
test_that("Aux mode", {
  expect_equal(aux_mode(2, 0.5), 1)
  expect_equal(aux_mode(5, 0.5), 3)
  expect_equal(aux_mode(6, 0.5), 3)
}
)

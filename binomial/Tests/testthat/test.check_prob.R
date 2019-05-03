context("Check Probability value")
library(binomial)
test_that("Check Prob",
          {
  expect_equal(check_prob(0.5), T)
  expect_equal(check_prob(1), T)
  expect_error(check_prob(1.1), "invalid prob value")
}
)

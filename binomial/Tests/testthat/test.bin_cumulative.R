context("Bin Cumulative")
library(binomial)
test_that("Bin Cumulative",
          {
  success <- 0:5
  probs <- bin_probability(success, 5, 0.5)
  cumulative <- cumsum(probs)
  result <- structure(data.frame('success' = success, 'probability' = probs, 'cumulative' = cumulative), class = c("bincum", "data.frame"))
  expect_equal(bin_cumulative(5, 0.5), result)
  success <- 0:6
  probs <- bin_probability(success, 6, 0.6)
  cumulative <- cumsum(probs)
  result <- structure(data.frame('success' = success, 'probability' = probs, 'cumulative' = cumulative), class = c("bincum", "data.frame"))
  expect_equal(bin_cumulative(6, 0.6), result)
  success <- 0:7
  probs <- bin_probability(success, 7, 0.7)
  cumulative <- cumsum(probs)
  result <- structure(data.frame('success' = success, 'probability' = probs, 'cumulative' = cumulative), class = c("bincum", "data.frame"))
  expect_equal(bin_cumulative(7, 0.7), result)
})

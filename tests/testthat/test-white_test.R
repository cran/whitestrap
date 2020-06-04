context("test-white_test.R")

several_checks <- function(result, model = 'white') {
  expect_s3_class(result, 'white_test')
  expect_type(unclass(result), 'list')
  expect_type(result$w_stat, 'double')
  expect_type(result$p_value, 'double')
  if (model == 'bootstrap') {
    expect_equal(length(result), 3)
  } else {
    expect_equal(length(result), 2)
  }
}



test_that("White's test works", {
  n <- 1000
  y <- 1:n
  sd <- runif(n, min = 0, max = 4)
  error <- rnorm(n, 0, sd*y)
  X <- y + error
  df <- data.frame(y, X)
  # OLS model
  fit <- lm(y ~ X, data = df)
  res <- white_test(fit)

  several_checks(result = res)
})



test_that("Bootstrapped White's test works", {
  n <- 1000
  y <- 1:n
  sd <- runif(n, min = 0, max = 4)
  error <- rnorm(n, 0, sd*y)
  X <- y + error
  df <- data.frame(y, X)
  # OLS model
  fit <- lm(y ~ X, data = df)
  res <- white_test_boot(fit)
  several_checks(result = res, model = 'bootstrap')

  expect_warning(white_test_boot(fit, 5),
                 "At least 10 bootstrap samples are needed. Setting 'bootstrap_samples' to 10. At least 500 is recommended.")
  expect_equal(white_test_boot(fit, 5)$iters, 10)
})

#' This function performs a White's Test for heteroskedasticity (White, H. (1980))
#'
#' White's test is a statistical test that determines whether the variance of the residuals in a regression model is constant.
#'
#' The approach followed is the one detailed at Wooldridge, 2012, p. 275. The fitted values from the original model are:
#'
#' \deqn{\widehat{y_i} = \widehat{\beta_0} + \widehat{\beta_1}x_{i1} + ... + \widehat{\beta_k}x_{ik}}
#'
#' Heteroscedasticity could be tested as a linear regression of the squared residuals against the fitted values:
#'
#' \deqn{\widehat{u^2} = \delta_0 + \delta_1\widehat{y} + \delta_2\widehat{y^2} + error}
#'
#' The null hypothesis states that \eqn{\delta_1 = \delta_2 = 0} (homoskedasticity). The test statistic
#' is defined as:
#'
#' \deqn{LM = nR^2}
#'
#' where \eqn{R^2} is the R-squared value from the regression of \eqn{u^2}.
#'
#' @param model An object of class \code{\link[stats]{lm}}
#'
#' @return AA list with class \code{white_test} containing:\tabular{ll}{
#'    \code{w_stat} \tab The value of the test statistic \cr
#'    \tab \cr
#'    \code{p_value} \tab The p-value of the test \cr
#'    \tab \cr
#' }
#' @export
#'
#'
#' @references
#' White, H. (1980). A Heteroskedasticity-Consistent Covariance Matrix Estimator
#' and a Direct Test for Heteroskedasticity. Econometrica, 48(4), 817-838.
#'
#' Wooldridge, Jeffrey M., 1960-. (2012). Introductory econometrics : a modern approach. Mason, Ohio :
#' South-Western Cengage Learning,
#'
#' @seealso \code{\link[stats]{lm}}
#'
#' @examples
#' # Define a dataframe with heteroscedasticity
#' n <- 100
#' y <- 1:n
#' sd <- runif(n, min = 0, max = 4)
#' error <- rnorm(n, 0, sd*y)
#' X <- y + error
#' df <- data.frame(y, X)
#' # OLS model
#' fit <- lm(y ~ X, data = df)
#' # White's test
#' white_test(fit)
#' @importFrom graphics hist lines
#' @import stats
white_test <- function(model) {

  # Squared residuals of fitted model
  squared_residuals <- model$residuals^2
  # get fitted values
  .fitted <- fitted(model)
  # auxiliary regression
  aux_r_suared <- summary(lm(squared_residuals ~ .fitted + I(.fitted^2)))$r.squared
  # test statistic
  w_stat <- length(.fitted) * aux_r_suared
  # compute p-value
  p_value <- 1 - pchisq(w_stat, 2)

  output <-
    structure(
      list(
        w_stat = w_stat,
        p_value = p_value
      ),
      class = "white_test"
    )

  output
}



#' Bootstrapped version of the White's test (Jeong, J., Lee, K. (1999))
#'
#' This is a versioned White's test based on a bootstrap procedure that can improve the performance of White’s test,
#' specially in small samples. It was proposed by Jeong, J., Lee, K. (1999) (see references for further details).
#'
#' @param model An object of class \code{\link[stats]{lm}}
#' @param bootstraps Number of bootstrap to be performed. If `bootstraps` is less than 10, it will automatically be set to 10.
#' At least 500 simulations are recommended. Default value is set to 1000.
#'
#' @return A list with class \code{white_test} containing:\tabular{ll}{
#'    \code{w_stat} \tab The value of the test statistic \cr
#'    \tab \cr
#'    \code{p_value} \tab The p-value of the test \cr
#'    \tab \cr
#'    \code{iters} \tab The number of bootstrap samples \cr
#'    \tab \cr
#' }
#' @export
#'
#' @references
#' Jeong, J., & Lee, K. (1999). Bootstrapped White’s test for heteroskedasticity in regression models. Economics Letters, 63(3), 261-267.
#'
#' White, H. (1980). A Heteroskedasticity-Consistent Covariance Matrix Estimator
#' and a Direct Test for Heteroskedasticity. Econometrica, 48(4), 817-838.
#'
#' Wooldridge, Jeffrey M., 1960-. (2012). Introductory econometrics : a modern approach. Mason, Ohio :
#' South-Western Cengage Learning,
#'
#' @details The bootstrapped error term is defined by:
#'
#' \deqn{\widehat{u_i} = \sigma^2 * t_i^{*} (i = 1,...N)}
#'
#' where \eqn{t_i^{*}} follows a distribution satisfying \eqn{E(t) = 0} and \eqn{var(t) = I}.
#'
#' In particular, the selected distribution of \eqn{t} can be found at the bottom of page 196 at Handbook of Computational Econometrics (2009).
#'
#' @examples
#' # Define a dataframe with heteroscedasticity
#' n <- 100
#' y <- 1:n
#' sd <- runif(n, min = 0, max = 4)
#' error <- rnorm(n, 0, sd*y)
#' X <- y + error
#' df <- data.frame(y, X)
#' # OLS model
#' fit <- lm(y ~ X, data = df)
#' # White's test
#' white_test_boot(fit)
#'
white_test_boot <-
  function(model, bootstraps = 1000) {
    if (bootstraps < 10) {
      bootstraps <- 10
      warning("At least 10 bootstrap samples are needed. Setting 'bootstrap_samples' to 10. At least 500 is recommended.")
    }

    # White test with original data
    wt <- white_test(model)


    # Bootstrapped method
    # Paper: Bootstrapped White’s test for heteroskedasticity in regression models
    # Jinook Jeong, Kyoungwoo Lee
    bcount <- 0
    wsb <- vector(mode = 'numeric')
    for (i in seq_len(bootstraps)) {
      .fitted <- fitted(model)
      var_res <- summary(model)$sigma^2

      boot_error <- sample(c(-(sqrt(5)-1)/2, (sqrt(5)+1)/2),
                          size = length(.fitted),
                          prob = c((sqrt(5)+1)/2*sqrt(5), (sqrt(5)-1)/2*sqrt(5)),
                          replace = TRUE)
      # wild_boot <- sample(c(-1, 1), size = length(.fitted), prob = c(.5, .5), replace = TRUE)
      bootstrapped_error <- var_res * boot_error

      # new_y <- model$model[[1]] + bootstrapped_error
      new_y <- .fitted + bootstrapped_error
      aux_m <- lm(as.formula(gsub(".*~","new_y ~", format(model$call$formula))), data = model$model)
      aux_r_suared <- summary(lm(aux_m$residuals^2 ~ fitted(aux_m) + I(fitted(aux_m)^2)))$r.squared
      white_stat_b <- length(new_y) * aux_r_suared

      # compute p-value
      if(abs(white_stat_b) >= abs(wt$w_stat)) {
        bcount <- bcount + 1
      }

      wsb[i] <- white_stat_b
    }


    output <-
      structure(
        list(
          w_stat = round(wt$w_stat, 2),
          p_value = bcount/1000,
          iters = bootstraps
        ),
        class = "white_test"
      )

    output

  }


#' @export
print.white_test <- function(x, ...) {
  if(length(x) == 2) {
    cat("White's test results\n",
        paste0("Null hypothesis: Homoskedasticity of the residuals"),
        paste0("Alternative hypothesis: Heteroskedasticity of the residuals"),
        paste0("Test Statistic: ", round(x$w_stat, 2)),
        paste0("P-value: ", round(x$p_value, 6)),
        sep = "\n")
  } else {
    # x$w_stat <- ifelse(length(x$w_stat) <= 10,
    #                    paste0(round(x$w_stat, 2), collapse = ", "),
    #                    paste0(paste0(round(x$w_stat[1:10], 2), collapse = ", "), ",..."))

    cat("Bootstrapped White's test results\n",
        paste0("Null hypothesis: Homoskedasticity of the residuals"),
        paste0("Alternative hypothesis: Heteroskedasticity of the residuals"),
        paste0("Number of bootstrap samples: ", x$iters),
        paste0("Bootstrapped Test Statistic: ", x$w_stat),
        paste0("P-value: ", round(x$p_value, 6)),
        sep = "\n")
  }

}


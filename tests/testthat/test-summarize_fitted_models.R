context("summarize fitted models")

# Create test data.
test_data <- local({
    set.seed(12345)
    N <- 10L
    d <- data.frame(x = rnorm(N), z = rnorm(N))
    d$y1 <- with(d, 2*x + 3*z + rnorm(N))
    d$y2 <- with(d, 4*x + 5*z + rnorm(N))
    d
})

# Fit models to test data.
expected_fit_y1 <- lm(y1 ~ x + z, test_data)
expected_fit_y2 <- lm(y2 ~ x + z, test_data)

# Create, fit, and summarize models using test data.
actual_summary <- local({
    linear_regression <- function(model, test_data) {
        with(test_data, lm(as.formula(model)))
    }
    models <- create_models(c("y1", "y2"), "x", "z", linear_regression)
    fitted_models <- fit_models(models, test_data)
    summarize_models(fitted_models)
})

test_that("outcome", {
    expect_equal(actual_summary$outcome, c(rep("y1", 3), rep("y2", 3)))
})

test_that("variable", {
    expect_equal(actual_summary$variable, rep(c("(Intercept)", "x", "z"), 2))
})

test_that("nobs", {
    expect_equal(actual_summary$nobs, rep(nobs(expected_fit_y1), nrow(actual_summary)))
})

test_that("beta", {
    beta_y1 <- unname(coef(summary(expected_fit_y1))[, 1])
    beta_y2 <- unname(coef(summary(expected_fit_y2))[, 1])
    expect_equal(actual_summary$beta, c(beta_y1, beta_y2))
})

test_that("se", {
    se_y1 <- unname(coef(summary(expected_fit_y1))[, 2])
    se_y2 <- unname(coef(summary(expected_fit_y2))[, 2])
    expect_equal(actual_summary$se, c(se_y1, se_y2))
})

test_that("lower and upper confidence limits", {
    ci_y1 <- unname(confint.default(expected_fit_y1))
    ci_y2 <- unname(confint.default(expected_fit_y2))
    expect_equal(actual_summary$lcl, c(ci_y1[, 1], ci_y2[, 1]))
    expect_equal(actual_summary$ucl, c(ci_y1[, 2], ci_y2[, 2]))
})

test_that("pvalue", {
    pvalue_y1 <- unname(coef(summary(expected_fit_y1))[, 4])
    pvalue_y2 <- unname(coef(summary(expected_fit_y2))[, 4])
    expect_equal(actual_summary$pvalue, c(pvalue_y1, pvalue_y2))
})

test_that("model", {
    model_y1 <- "y1 ~ x + z  (linear_regression)"
    model_y2 <- "y2 ~ x + z  (linear_regression)"
    expect_equal(actual_summary$model, c(rep(model_y1, 3), rep(model_y2, 3)))
})

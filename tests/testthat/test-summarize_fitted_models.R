context("summarize fitted models")

source("setup.R")

selected_models <- c(
    "y1 ~ x1 + z1  (linear)",
    "y2 ~ x1 + z1  (linear)")

actual <- summarized_models[summarized_models$model %in% selected_models, ]

# Fit models to test data.
expected_fit_y1 <- lm(y1 ~ x1 + z1, test_data)
expected_fit_y2 <- lm(y2 ~ x1 + z1, test_data)

test_that("outcome", {
    expect_equal(actual$outcome, c(rep("y1", 4), rep("y2", 4)))
})

test_that("variable", {
    expect_equal(actual$variable, rep(c("(Intercept)", "x11", "z12", "z13"), 2))
})

test_that("nobs", {
    expect_equal(actual$nobs, rep(nobs(expected_fit_y1), nrow(actual)))
})

test_that("beta", {
    beta_y1 <- unname(coef(summary(expected_fit_y1))[, 1])
    beta_y2 <- unname(coef(summary(expected_fit_y2))[, 1])
    expect_equal(actual$beta, c(beta_y1, beta_y2))
})

test_that("se", {
    se_y1 <- unname(coef(summary(expected_fit_y1))[, 2])
    se_y2 <- unname(coef(summary(expected_fit_y2))[, 2])
    expect_equal(actual$se, c(se_y1, se_y2))
})

test_that("lower and upper confidence limits", {
    ci_y1 <- unname(confint.default(expected_fit_y1))
    ci_y2 <- unname(confint.default(expected_fit_y2))
    expect_equal(actual$lcl, c(ci_y1[, 1], ci_y2[, 1]))
    expect_equal(actual$ucl, c(ci_y1[, 2], ci_y2[, 2]))
})

test_that("pvalue", {
    pvalue_y1 <- unname(coef(summary(expected_fit_y1))[, 4])
    pvalue_y2 <- unname(coef(summary(expected_fit_y2))[, 4])
    expect_equal(actual$pvalue, c(pvalue_y1, pvalue_y2))
})

test_that("model", {
    model_y1 <- "y1 ~ x1 + z1  (linear)"
    model_y2 <- "y2 ~ x1 + z1  (linear)"
    expect_equal(actual$model, c(rep(model_y1, 4), rep(model_y2, 4)))
})

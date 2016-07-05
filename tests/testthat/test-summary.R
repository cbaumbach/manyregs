context("summary")

source("setup.R")

actual <- summary(fitted_models[[1]])

expected_fit <- lm(y1 ~ x1, test_data)

test_that("non-fitted model", {
    expected <- data.frame(model = "y1 ~ x1  (linear)", stringsAsFactors = FALSE)
    expect_equal(summary(models[[1]]), expected)
})

test_that("outcome", {
    expect_equal(actual$outcome, rep("y1", 2))
})

test_that("variable", {
    expect_equal(actual$variable, c("(Intercept)", "x11"))
})

test_that("nobs", {
    expect_equal(actual$nobs, rep(100, 2))
})

test_that("beta", {
    expect_equal(actual$beta, unname(coef(expected_fit)))
})

test_that("se", {
    expect_equal(actual$se, unname(sqrt(diag(vcov(expected_fit)))))
})

test_that("lcl", {
    expect_equal(actual$lcl, actual$beta - qnorm(.975) * actual$se)
})

test_that("ucl", {
    expect_equal(actual$ucl, actual$beta + qnorm(.975) * actual$se)
})

test_that("pvalue", {
    expect_equal(actual$pvalue, 2 * (1 - pt(actual$beta / actual$se, df = actual$nobs - 2)))
})

test_that("model", {
    expect_equal(actual$model, rep("y1 ~ x1  (linear)", 2))
})

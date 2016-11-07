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
    expect_equal(actual$variable, c("(Intercept)", "x1"))
})

test_that("levels", {
    expect_equal(actual$level, c("", "1"))
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

test_that("warning", {
    expect_equal(actual$warning, c(NA_character_, NA_character_))
})

test_that("error", {
    expect_equal(actual$error, c(NA_character_, NA_character_))
})

test_that("if model$fit is NULL, we obtain minimal but useful output with many NAs", {
    data <- create_dataset("y", "x")
    f <- function(model, data) stop("an error")
    model <- create_models("y", "x", f = f)[[1]]
    suppressWarnings(fitted_model <- fit_model(model, data))

    actual <- summary(fitted_model)

    expect_equal(actual$outcome, c("y", "y"))
    expect_equal(actual$variable, c("(Intercept)", "x"))
    expect_equal(actual$level, c(NA_integer_, NA_integer_))
    expect_equal(actual$nobs, c(NA_integer_, NA_integer_))
    expect_equal(actual$beta, c(NA_real_, NA_real_))
    expect_equal(actual$se, c(NA_real_, NA_real_))
    expect_equal(actual$lcl, c(NA_real_, NA_real_))
    expect_equal(actual$ucl, c(NA_real_, NA_real_))
    expect_equal(actual$pvalue, c(NA_real_, NA_real_))
    expect_equal(actual$model, c("y ~ x  (f)", "y ~ x  (f)"))
    expect_equal(actual$warning, c(NA_character_, NA_character_))
    expect_equal(actual$error, c("an error", "an error"))
})

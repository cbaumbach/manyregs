context("summarize_models")

test_that("non-fitted models", {
    f <- function(model, data) { }
    models <- create_models(c("y1", "y2"), "x", "z", f)
    expected <- data.frame(stringsAsFactors = FALSE,
        models = c("y1 ~ x + z  (f)", "y2 ~ x + z  (f)"))
    expect_equal(summarize_models(models), expected)
})

test_that("fitted models", {
    # Create test data.
    set.seed(12345)
    N <- 10L
    data <- data.frame(x = rnorm(N), z = rnorm(N))
    data$y1 <- with(data, rnorm(1) * x + rnorm(1) * z + rnorm(N))
    data$y2 <- with(data, rnorm(1) * x + rnorm(1) * z + rnorm(N))

    # Create, fit, and summarize models.
    linear_regression <- function(model, data) {
        with(data, lm(as.formula(model)))
    }
    models <- create_models(c("y1", "y2"), "x", "z", linear_regression)
    fitted_models <- fit_models(models, data)
    actual_summary <- summarize_models(fitted_models)

    # Construct expected summary for outcome y1.
    expected_fit_y1 <- lm(y1 ~ x + z, data)
    expected_summary_y1 <- data.frame(
        outcome = "y1",
        variable = c("(Intercept)", "x", "z"),
        nobs = N,
        beta = coef(summary(expected_fit_y1))[, 1],
        se = coef(summary(expected_fit_y1))[, 2],
        lcl = confint(expected_fit_y1)[, 1],
        ucl = confint(expected_fit_y1)[, 2],
        pvalue = coef(summary(expected_fit_y1))[, 4],
        model = "y1 ~ x + z  (linear_regression)",
        stringsAsFactors = FALSE)

    # Construct expected summary for outcome y2.
    expected_fit_y2 <- lm(y2 ~ x + z, data)
    expected_summary_y2 <- data.frame(
        outcome = "y2",
        variable = c("(Intercept)", "x", "z"),
        nobs = N,
        beta = coef(summary(expected_fit_y2))[, 1],
        se = coef(summary(expected_fit_y2))[, 2],
        lcl = confint(expected_fit_y2)[, 1],
        ucl = confint(expected_fit_y2)[, 2],
        pvalue = coef(summary(expected_fit_y2))[, 4],
        model = "y2 ~ x + z  (linear_regression)",
        stringsAsFactors = FALSE)

    # Combine expected summary tables.
    expected_summary <- rbind(expected_summary_y1, expected_summary_y2)
    rownames(expected_summary) <- NULL

    expect_equal(actual_summary, expected_summary)
})

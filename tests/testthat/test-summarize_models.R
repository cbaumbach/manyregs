context("summarize_models")

test_that("a \"warning\" column appears if warnings occur", {
    data <- create_dataset("y", "x")
    f <- function(model, data) {
        warning("a warning")
        with(data, lm(as.formula(model)))
    }
    models <- create_models("y", "x", f = f)
    suppressWarnings(fitted_models <- fit_models(models, data))

    actual <- summarize_models(fitted_models)
    expect_equal(actual$warning, rep("a warning", nrow(actual)))
})

test_that("an \"error\" column appears if errors occur", {
    data <- create_dataset("y", "x")
    f <- function(model, data) stop("an error")
    models <- create_models("y", "x", f = f)
    suppressWarnings(fitted_models <- fit_models(models, data))

    actual <- summarize_models(fitted_models)
    expect_equal(actual$error, rep("an error", nrow(actual)))
})

test_that("there are no warning and error columns if no warnings or errors occur", {
    data <- create_dataset("y", "x")
    f <- function(model, data) with(data, lm(as.formula(model)))
    models <- create_models("y", "x", f = f)
    actual <- tryCatch({
        fitted_models <- fit_models(models, data)
    }, error = function(c) {
        fail("There was an unexpected error.")
    }, warning = function(c) {
        fail("There was an unexpected warning.")
    })

    summarize_models(fitted_models)

    expect_null(actual$warning)
    expect_null(actual$error)
})

test_that("non-fitted models are summarized as a 1-column data frame of model strings", {
    models <- create_models("y", c("x", "z"), f = lm)
    actual <- summarize_models(models)
    expected <- data.frame(model = c("y ~ x  (lm)", "y ~ z  (lm)"), stringsAsFactors = FALSE)
    expect_equal(actual, expected)
})

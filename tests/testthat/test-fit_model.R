context("fit_model")

test_that("happy path", {
    f <- function(model, data) "fit"
    model <- new_model("y", "x", "z", f, "f")
    fitted_model <- fit_model(model, NULL)
    expect_equal(fitted_model$fit, "fit")
})

test_that("warnings are shown", {
    model <- new_model("y", "x", "z", warning, "f")
    expect_warning(fit_model(model, NULL))
})

test_that("warnings are saved in the model object", {
    f <- function(model, data) warning("a warning")
    model <- new_model("y", "x", "z", f, "f")
    suppressWarnings(fitted_model <- fit_model(model, NULL))
    expect_equal(fitted_model$warning, "a warning")
})

test_that("errors are converted to warnings", {
    model <- new_model("y", "x", "z", stop, "f")
    expect_warning(fit_model(model, NULL))
})

test_that("errors are saved in the model object", {
    f <- function(model, data) stop("an error")
    model <- new_model("y", "x", "z", f, "f")
    suppressWarnings(fitted_model <- fit_model(model, NULL))
    expect_equal(fitted_model$error, "an error")
})

test_that("the fit is NULL if an error occurs", {
    model <- new_model("y", "x", "z", stop, "f")
    suppressWarnings(fitted_model <- fit_model(model, NULL))
    expect_null(fitted_model$fit)
})

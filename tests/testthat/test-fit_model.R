context("fit_model")

test_that("happy path", {
    f <- function(model, data) as.character(model)
    model <- new_model("y", "x", "z", f, "f")
    fitted_model <- fit_model(model, NULL)
    expect_equal(fitted_model$fit, as.character(model))
})

test_that("warnings are caught, remembered by the model, and displayed", {
    f <- function(model, data) warning("a warning")
    model <- new_model("y", "x", "z", f, "f")
    expect_warning(fitted_model <- fit_model(model, NULL))
    expect_equal(fitted_model$warning, "a warning")
    expect_null(fitted_model$fit)
})

test_that("errors are caught, remembered by the model, and displayed as warnings", {
    f <- function(model, data) stop("an error")
    model <- new_model("y", "x", "z", f, "f")
    expect_warning(fitted_model <- fit_model(model, NULL))
    expect_equal(fitted_model$error, "an error")
    expect_null(fitted_model$fit)
})

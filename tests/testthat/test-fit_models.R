context("fit_models")

f <- function(model, data) {
    as.character(model)
}
models <- create_models(c("y1", "y2"), "x", "z", f)

test_that("one-by-one", {
    fitted_models <- fit_models(models, NULL)
    expect_equal(length(fitted_models), 2L)
    m1 <- models[[1]]
    m2 <- models[[2]]
    fm1 <- fitted_models[[1]]
    fm2 <- fitted_models[[2]]
    expect_equal(fm1$fit, as.character(m1))
    expect_equal(fm2$fit, as.character(m2))
    expect_equal(remove_slots(fm1, "fit"), m1)
    expect_equal(remove_slots(fm2, "fit"), m2)
})

test_that("in parallel", {
    expect_equal(fit_models(models, NULL, cores = 2), fit_models(models, NULL))
})

test_that("error", {
    throw_error <- function(model, data) {
        stop("an error")
    }
    models <- create_models("y", "x", "z", throw_error)
    expect_warning(fm <- fit_models(models, NULL)[[1]])
    expect_match(fm$error, "an error")
    expect_true("fit" %in% names(fm))
    expect_equal(fm$fit, NULL)
})

test_that("warning", {
    throw_warning <- function(model, data) {
        warning("a warning")
    }
    models <- create_models("y", "x", "z", throw_warning)
    expect_warning(fm <- fit_models(models, NULL)[[1]])
    expect_match(fm$warning, "a warning")
    expect_true("fit" %in% names(fm))
    expect_equal(fm$fit, NULL)
})

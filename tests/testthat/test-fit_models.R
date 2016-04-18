context("fit_models")

test_that("one-by-one", {
    f <- function(model, data) {
        as.character(model)
    }
    models <- create_models(c("y1", "y2"), "x", "z", f)
    fitted_models <- fit_models(models, NULL)
    expect_equal(length(fitted_models), 2L)
    expect_equal(fitted_models[[1]]$fit, as.character(models[[1]]))
    expect_equal(fitted_models[[2]]$fit, as.character(models[[2]]))
})

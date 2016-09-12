context("find_xy_ranges")

source("setup.R")

test_that("multiple models", {
    models <- fitted_models[1:2]
    actual <- find_xy_ranges(models)
    x <- summarize_models(models)
    ci <- x[x$variable == "x1" & x$level == "1", c("lcl", "ucl")]
    expected <- list(xlim = c(1 - .5, 2 + .5), ylim = range(c(ci$lcl, ci$ucl)))
    expect_equal(actual, expected)
})

test_that("one model", {
    model <- fitted_models[1]
    actual <- find_xy_ranges(model)
    x <- summarize_models(model)
    ci <- x[x$variable == "x1" & x$level == "1", c("lcl", "ucl")]
    expected <- list(xlim = c(1 - .5, 2 + .5), ylim = range(c(ci$lcl, ci$ucl)))
    expect_equal(actual, expected)
})

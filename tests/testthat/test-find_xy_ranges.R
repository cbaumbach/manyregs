context("find_xy_ranges")

source("setup.R")

test_that("multiple models", {
    xylim <- find_xy_ranges(fitted_models)
    ci <- do.call(rbind, lapply(fitted_models, find_exposure_estimates))
    expect_equal(xylim$xlim, c(1 - .5, 3 + .5))
    expect_equal(xylim$ylim, range(c(ci$lcl, ci$ucl)))
})

test_that("one model", {
    model <- fitted_models[[1]]
    xylim <- find_xy_ranges(list(model))
    ci <- find_exposure_estimates(model)
    expect_equal(xylim$xlim, c(1 - .5, 2 + .5))
    expect_equal(xylim$ylim, range(c(ci$lcl, ci$ucl)))
})

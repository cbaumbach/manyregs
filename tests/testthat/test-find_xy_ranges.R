context("find_xy_ranges")

source("setup.R")

test_that("find_xy_ranges", {
    xylim <- find_xy_ranges(fitted_models)
    ci <- do.call(rbind, lapply(fitted_models, find_exposure_confidence_intervals))
    expect_equal(xylim$xlim, c(1 - .5, 3 + .5))
    expect_equal(xylim$ylim, c(min(ci$lcl), max(ci$ucl)))
})

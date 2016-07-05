context("sort_models")

source("setup.R")

sort_order <- c("exposures", "outcomes", "adjustments")

test_that("reverse order of variables", {
    actual <- sort_models(models, sort_order, rev(outcomes), rev(exposures), rev(adjustments))
    expect_equal(find_outcomes(actual), rev(outcomes))
    expect_equal(find_exposures(actual), rev(exposures))
    expect_equal(find_adjustments(actual), rev(adjustments))
    expect_equal(length(actual), length(models))
})

test_that("reverse order of variables in a subset of models", {
    actual <- sort_models(models, sort_order, c("y2", "y1"), "x1", rev(adjustments))
    expected <- create_models(c("y2", "y1"), "x1", rev(adjustments), linear, by = sort_order)
    expect_equal(actual, expected)
})

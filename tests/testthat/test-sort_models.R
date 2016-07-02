context("sort_models")

outcomes <- c("y1", "y2", "y3")
exposures <- c("x1", "x2")
adjustments <- list("z1", c("z1", "z2"))
f <- function(model, data) { }
test_models <- create_models(outcomes, exposures, adjustments, f)
sort_order <- c("exposures", "outcomes", "adjustments")

test_that("reverse order of variables", {
    actual <- sort_models(test_models, sort_order, rev(outcomes), rev(exposures), rev(adjustments))
    expect_equal(find_outcomes(actual), rev(outcomes))
    expect_equal(find_exposures(actual), rev(exposures))
    expect_equal(find_adjustments(actual), rev(adjustments))
    expect_equal(length(actual), length(test_models))
})

test_that("reverse order of variables in a subset of models", {
    actual <- sort_models(test_models, sort_order, c("y2", "y1"), "x1", rev(adjustments))
    expected <- create_models(c("y2", "y1"), "x1", rev(adjustments), f, by = sort_order)
    expect_equal(actual, expected)
})

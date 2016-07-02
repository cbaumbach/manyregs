context("find_variables")

outcomes <- c("y1", "y2")
exposures <- c("x1", "x2", "x3")
adjustments <- list("z1", c("z1", "z2"))
f <- function(model, data) { }
test_models <- create_models(outcomes, exposures, adjustments, f)

test_that("find_variables", {
    actual <- find_variables(test_models)
    expected <- list(outcomes = outcomes, exposures = exposures,
        adjustments = adjustments)
    expect_equal(actual, expected)
})

test_that("find_outcomes", {
    expect_equal(find_outcomes(test_models), outcomes)
})

test_that("find_exposures", {
    expect_equal(find_exposures(test_models), exposures)
})

test_that("find_adjustments", {
    expect_equal(find_adjustments(test_models), adjustments)
})

context("find_variables")

source("setup.R")

test_that("find_variables", {
    actual <- find_variables(models)
    expected <- list(outcomes = outcomes, exposures = exposures, adjustments = adjustments)
    expect_equal(actual, expected)
})

test_that("find_outcomes", {
    expect_equal(find_outcomes(models), outcomes)
})

test_that("find_exposures", {
    expect_equal(find_exposures(models), exposures)
})

test_that("find_adjustments", {
    expect_equal(find_adjustments(models), adjustments)
})

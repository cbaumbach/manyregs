context("filter_models")

source("setup.R")

test_that("keep all models by using an empty filter", {
    expect_equal(filter_models(models), models)
})

test_that("keep a subset of models", {
    actual <- filter_models(models, "y1", "x2", list(c("z1", "z2")), combine = "and")
    expected <- create_models("y1", "x2", list(c("z1", "z2")), linear)
    expect_equal(actual, expected)
})

test_that("selecting a non-existing variable returns the empty list", {
    expect_equal(filter_models(models, "foobar"), list())
})

test_that("drop a subset of models", {
    actual <- filter_models(models, "y1", "x2", adjustments[2], drop = TRUE, combine = "or")
    expected <- create_models(outcomes[-1], exposures[-2], adjustments[-2], linear)
    expect_equal(actual, expected)
})

test_that("`combine` must be one of 'and' or 'or'", {
    expect_error(filter_models(models, combine = "foobar"),
        "`combine` must be one of \"and\" or \"or\": \"foobar\"")
})

test_that("`combine` must be of length 1", {
    expect_error(filter_models(models, combine = c("and", "or")),
        "`combine` must be of length 1")
})

test_that("a single adjustment can be specified as a character vector", {
    actual <- filter_models(models, "y2", "x1", c("z1", "z2"), combine = "and")
    expected <- create_models("y2", "x1", list(c("z1", "z2")), linear)
    expect_equal(actual, expected)
})

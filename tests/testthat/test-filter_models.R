context("filter_models")

f <- function(model, data) { }
outcomes <- c("y1", "y2", "y3")
exposures <- c("x1", "x2")
adjustments <- list("z1", c("z1", "z2"))
test_models <- create_models(outcomes, exposures, adjustments, f)

test_that("keep all models by using an empty filter", {
    expect_equal(filter_models(test_models), test_models)
})

test_that("keep a subset of models", {
    actual <- filter_models(test_models, "y1", "x2", list(c("z1", "z2")), combine = "and")
    expected <- create_models("y1", "x2", list(c("z1", "z2")), f)
    expect_equal(actual, expected)
})

test_that("selecting a non-existing variable returns the empty list", {
    expect_equal(filter_models(test_models, "foobar"), list())
})

test_that("drop a subset of models", {
    actual <- filter_models(test_models, "y1", "x2", adjustments[2], drop = TRUE, combine = "or")
    expected <- create_models(setdiff(outcomes, "y1"), setdiff(exposures, "x2"), adjustments[1], f)
    expect_equal(actual, expected)
})

test_that("`combine` must be one of 'and' or 'or'", {
    expect_error(filter_models(test_models, combine = "foobar"),
        "`combine` must be one of \"and\" or \"or\": \"foobar\"")
})

test_that("`combine` must be of length 1", {
    expect_error(filter_models(test_models, combine = c("and", "or")),
        "`combine` must be of length 1")
})

test_that("a single adjustment can be specified as a character vector", {
    actual <- filter_models(test_models, "y2", "x1", c("z1", "z2"), combine = "and")
    expected <- create_models("y2", "x1", list(c("z1", "z2")), f)
    expect_equal(actual, expected)
})

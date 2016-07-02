context("find_combinations")

adjustments <- list(NULL, "z1", c("z1", "z2"))

test_that("find_combinations", {
    actual <- find_combinations(c("y1", "y2"), "x1", adjustments)
    expected <- list(
        outcomes = list("y1", "y1", "y1", "y2", "y2", "y2"),
        exposures = list("x1", "x1", "x1", "x1", "x1", "x1"),
        adjustments = c(adjustments, adjustments))
    expect_equal(actual, expected)
})

test_that("outcomes, exposures, and adjustments can be NULL", {
    actual <- find_combinations(NULL, NULL, NULL)
    expected <- list(outcomes = list(), exposures = list(), adjustments = list())
    expect_equal(actual, expected)
})

test_that("outcomes can be NULL", {
    actual <- find_combinations(NULL, "x", "z")
    expected <- list(outcomes = list(NULL), exposures = list("x"), adjustments = list("z"))
    expect_equal(actual, expected)
})

test_that("exposures can be NULL", {
    actual <- find_combinations("y", NULL, "z")
    expected <- list(outcomes = list("y"), exposures = list(NULL), adjustments = list("z"))
    expect_equal(actual, expected)
})

test_that("adjustments can be NULL", {
    actual <- find_combinations("y", "x", NULL)
    expected <- list(outcomes = list("y"), exposures = list("x"), adjustments = list(NULL))
    expect_equal(actual, expected)
})

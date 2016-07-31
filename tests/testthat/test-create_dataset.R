context("create_dataset")

source("setup.R")

data <- create_dataset("non-factor",
    c("factor-with-default-labels", 2),
    c("factor-with-explicit-labels", 3, "LOW", "MID", "HIGH"),
    number_of_rows = 10)

test_that("names", {
    expect_equal(names(data), c("non-factor", "factor-with-default-labels", "factor-with-explicit-labels"))
})

test_that("class", {
    expect_equal(vapply(data, class, character(1), USE.NAMES = FALSE), c("numeric", "factor", "factor"))
})

test_that("nrow", {
    expect_equal(10, nrow(data))
})

test_that("levels", {
    expect_null(levels(data[["non-factor"]]))
    expect_equal(levels(data[["factor-with-default-labels"]]), c("1", "2"))
    expect_equal(levels(data[["factor-with-explicit-labels"]]), c("LOW", "MID", "HIGH"))
})

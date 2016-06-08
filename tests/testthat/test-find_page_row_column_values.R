context("find_page_row_column_values")

test_that("happy path", {
    variables <- list(outcomes = "y", exposures = "x", adjustments = list("z"))
    rows <- "outcomes"
    columns <- "exposures"
    actual <- find_page_row_column_values(variables, rows, columns)
    expected <- list(page = list("z"), row = "y", column = "x")
    expect_equal(actual, expected)
})

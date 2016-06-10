context("find_page_row_column_values")

test_that("happy path", {
    variables <- list(outcomes = "y", exposures = "x", adjustments = list("z"))
    types <- c(pages = "adjustments", rows = "outcomes", columns = "exposures")
    actual <- find_page_row_column_values(variables, types)
    expected <- list(pages = list("z"), rows = "y", columns = "x")
    expect_equal(actual, expected)
})

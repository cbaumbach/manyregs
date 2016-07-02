context("find_pages_rows_columns")

test_that("defaults", {
    x <- find_pages_rows_columns(NULL, NULL)
    expect_equal(x$rows, "outcomes")
    expect_equal(x$columns, "exposures")
    expect_equal(x$pages, "adjustments")
})

test_that("happy path", {
    x <- find_pages_rows_columns("adjustments", "outcomes")
    expect_equal(x$rows, "adjustments")
    expect_equal(x$columns, "outcomes")
    expect_equal(x$pages, "exposures")
})

test_that("identical rows and columns throws error", {
    expect_error(find_pages_rows_columns("outcomes", "outcomes"),
        "Arguments \"rows\" and \"columns\" must not have the same value.")
})

test_that("non-existing rows or column values throw errors", {
    error_message <- paste("Arguments \"rows\" and \"columns\" must be",
        "one of \"outcomes\", \"exposures\", or \"adjustments\".")
    expect_error(find_pages_rows_columns("foo", "outcomes"), error_message)
    expect_error(find_pages_rows_columns("outcomes", "foo"), error_message)
})

test_that("one argument NULL, the other non-NULL throws error", {
    error_message <- paste("Arguments \"rows\" and \"columns\" must either",
        "both be non-NULL or both be NULL.")
    expect_error(find_pages_rows_columns(NULL, "outcomes"), error_message)
    expect_error(find_pages_rows_columns("outcomes", NULL), error_message)
})

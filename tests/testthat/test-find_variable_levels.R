context("find_variable_levels")

f <- find_variable_levels

test_that("non-factor", {
    data <- create_dataset("x")
    expect_null(f("x", data))
})

test_that("factor with default labels", {
    data <- create_dataset(c("x", 2))
    expect_equal(f("x", data), c("1", "2"))
})

test_that("factor with custom labels", {
    data <- create_dataset(c("x", 2, "YES", "NO"))
    expect_equal(f("x", data), c("YES", "NO"))
})

test_that("return NA if data is NULL", {
    expect_equal(f("x", NULL), NA_character_)
})
})

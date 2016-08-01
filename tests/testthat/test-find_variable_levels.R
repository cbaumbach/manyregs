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

test_that("interaction between non-factors", {
    data <- create_dataset("x", "y", "z")
    expect_null(f("x:y:z", data))
})

test_that("interaction between factors", {
    data <- create_dataset(c("x", 2), c("y", 3), c("z", 4))
    expect_equal(f("x:y:z", data), list(c("1", "2"), c("1", "2", "3"), c("1", "2", "3", "4")))
    expect_equal(f("z:y:x", data), rev(f("x:y:z", data)))
})

test_that("interaction between factors and non-factors", {
    data <- create_dataset("w", "x", c("y", 2), c("z", 3))
    expect_equal(f("w:x:y:z", data), list(NULL, NULL, c("1", "2"), c("1", "2", "3")))
})

test_that("you can use x*y or x:y interchangeably", {
    data <- create_dataset(c("x", 2), "y")
    expect_equal(f("x*y", data), f("x:y", data))
})

test_that("non-standard names must be backquoted", {
    data <- create_dataset("x-1", c("x-2", 2), c("x-3", 3))
    expect_null(f("`x-1`", data))
    expect_equal(f("`x-2`", data), c("1", "2"))
    expect_equal(f("`x-1`:`x-2`", data), list(NULL, c("1", "2")))
    expect_equal(f("`x-2`:`x-3`", data), list(c("1", "2"), c("1", "2", "3")))
})

test_that("interaction between an explicit factor and a non-factor", {
    data <- create_dataset(c("x", 2), "y")
    expect_equal(f("factor(x):y", data), list(c("1", "2"), NULL))
    expect_equal(f("y:factor(x)", data), rev(f("factor(x):y", data)))
})

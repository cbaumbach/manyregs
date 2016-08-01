context("find_variable_levels")

test_that("happy path", {
    data <- create_dataset("y", c("w", 2), c("x", 2), c("z", 3, "LOW", "MID", "HIGH"))
    f <- function(model, data) as.character(model)
    model <- new_model("y", "x", c("factor(w)", "z"), f, "f")
    actual <- find_variable_levels(model, data)
    expected <- list(y = NULL, x = c("1", "2"), "factor(w)" = c("1", "2"), z = c("LOW", "MID", "HIGH"))
    expect_equal(actual, expected)
})

test_that("return NULL if data is NULL", {
    f <- function(model, data) as.character(model)
    model <- new_model("y", "x", c("factor(w)", "z"), f, "f")
    expect_null(find_variable_levels(model, NULL))
})

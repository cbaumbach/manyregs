context("find_variable_levels")

test_that("find_variable_levels", {
    data <- create_dataset("y", c("x", 2), c("z", 3, "LOW", "MID", "HIGH"))
    f <- function(model, data) as.character(model)
    model <- new_model("y", "x", "z", f, "f")
    actual <- find_variable_levels(model, data)
    expected <- list(y = NULL, x = c("1", "2"), z = c("LOW", "MID", "HIGH"))
    expect_equal(actual, expected)
})

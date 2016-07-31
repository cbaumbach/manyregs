context("find_estimates_for")

data <- create_dataset("y", c("v", 2), "w", "w2", c("x", 3), c("z", 3, "LOW", "MID", "HIGH"))
models <- create_models("y", "factor(v)", c("w", "w2", "x", "z"), linear)
model <- fit_models(models, data)[[1]]

rows_matching <- function(variables, model) {
    data <- summary(model)
    data[data$variable %in% variables, , drop = FALSE]
}

test_that("variable name with parentheses", {
    actual <- find_estimates_for("factor(v)", model)
    expected <- rows_matching("factor(v)2", model)
    expect_equal(actual, expected)
})

test_that("non-factor", {
    actual <- find_estimates_for("w", model)
    expected <- rows_matching("w", model)
    expect_equal(actual, expected)
})

test_that("factor without explicit labels", {
    actual <- find_estimates_for("x", model)
    expected <- rows_matching(c("x2", "x3"), model)
    expect_equal(actual, expected)
})

test_that("factor with explicit labels", {
    actual <- find_estimates_for("z", model)
    expected <- rows_matching(c("zMID", "zHIGH"), model)
    expect_equal(actual, expected)
})

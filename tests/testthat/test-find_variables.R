context("find_variables")

outcomes <- c("y1", "y2")
exposures <- c("x1", "x2", "x3")
adjustments <- list("z1", c("z1", "z2"))
f <- function(model, data) { }
test_models <- create_models(outcomes, exposures, adjustments, f)

test_that("using NULL defaults", {
    actual <- find_variables(test_models)
    expected <- list(outcomes = outcomes, exposures = exposures,
        adjustments = adjustments)
    expect_equal(actual, expected)
})

test_that("with arguments", {
    actual <- find_variables(test_models, "y1", "x2", list(c("z1", "z2")))
    expected <- list(outcomes = "y1", exposures = "x2", adjustments = list(c("z1", "z2")))
    expect_equal(actual, expected)
})

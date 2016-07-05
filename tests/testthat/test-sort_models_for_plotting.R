context("sort_models_for_plotting")

outcomes <- c("y1", "y2")
exposures <- c("x1", "x2")
adjustments <- list(NULL, "z1", c("z1", "z2"))
f <- function(model, data) { }
test_models <- create_models(outcomes, exposures, adjustments, f)
expected_models <- function(by) {
    create_models(outcomes, exposures, adjustments, f, by)
}

test_that("default: pages - adjustments, rows - outcomes, columns - exposures", {
    sorted_models <- sort_models_for_plotting(test_models)
    expect_equal(sorted_models, expected_models(c("adjustments", "outcomes", "exposures")))
})

test_that("pages - adjustments, rows - exposures, columns - outcomes", {
    sorted_models <- sort_models_for_plotting(test_models, rows = "exposures", columns = "outcomes")
    expect_equal(sorted_models, expected_models(c("adjustments", "exposures", "outcomes")))
})

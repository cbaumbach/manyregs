context("sort_models_for_plotting")

source("setup.R")

expected_models <- function(by) {
    create_models(outcomes, exposures, adjustments, linear, by)
}

test_that("default: pages - adjustments, rows - outcomes, columns - exposures", {
    sorted_models <- sort_models_for_plotting(models)
    expect_equal(sorted_models, expected_models(c("adjustments", "outcomes", "exposures")))
})

test_that("pages - adjustments, rows - exposures, columns - outcomes", {
    sorted_models <- sort_models_for_plotting(models, rows = "exposures", columns = "outcomes")
    expect_equal(sorted_models, expected_models(c("adjustments", "exposures", "outcomes")))
})

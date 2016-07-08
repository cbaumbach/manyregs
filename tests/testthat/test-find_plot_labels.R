context("find_plot_labels")

test_model <- new_model("y", "x", c("z1", "z2"))

test_that("row - outcome, column - exposure, page - adjustment", {
    actual_labels <- find_plot_labels(test_model, rows = "outcomes", columns = "exposures")
    expected_labels <- list(row = "y", column = "x", page = "adjustment: z1, z2")
    expect_equal(actual_labels, expected_labels)
})

test_that("row - adjustment, column - exposures, page - outcome", {
    actual_labels <- find_plot_labels(test_model, rows = "adjustments", columns = "exposures")
    expected_labels <- list(row = "z1, z2", column = "x", page = "outcome: y")
    expect_equal(actual_labels, expected_labels)
})

test_that("row - adjustment, column - outcome, page - exposure", {
    actual_labels <- find_plot_labels(test_model, rows = "adjustments", columns = "outcomes")
    expected_labels <- list(row = "z1, z2", column = "y", page = "exposure: x")
    expect_equal(actual_labels, expected_labels)
})

test_that("with renaming", {
    new_labels <- list("y" = "outcome", "x" = "exposure", "z1" = "adjustment1", "z2" = NULL)
    actual_labels <- find_plot_labels(test_model, rows = "outcomes", columns = "exposures", new_labels)
    expected_labels <- list(row = "outcome", column = "exposure", page = "adjustment: adjustment1")
    expect_equal(actual_labels, expected_labels)
})

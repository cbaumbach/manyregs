context("find_mapping_for_labels")

test_that("find_variable_names_for_labels", {
    data <- create_dataset("y", c("x", 3, c("low", "mid", "high")), c("z", 2), number_of_rows = 30)
    fitted <- fit_model(new_model("y", c("x", "z"), NULL, linear, "linear"), data)
    actual <- find_variable_names_for_labels(c("xhigh", "xlow", "y", "z1", "xmid", "z2", "foobar", "(Intercept)"), fitted)
    expected <- c("x", "x", "y", "z", "x", "z", NA, "(Intercept)")
    expect_equal(actual, expected)
})

test_that("find_levels_for_labels", {
    data <- create_dataset("y", c("x", 3, c("low", "mid", "high")), c("z", 2), number_of_rows = 30)
    fitted <- fit_model(new_model("y", c("x", "z"), NULL, linear, "linear"), data)
    actual <- find_levels_for_labels(c("xhigh", "xlow", "y", "z1", "xmid", "z2", "foobar", "(Intercept)"), fitted)
    expected <- c("high", "low", "", "1", "mid", "2", NA, "")
    expect_equal(actual, expected)
})

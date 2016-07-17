context("find_segments_to_plot")

source("setup.R")

model <- fitted_models[[1]]
model_summary <- summary(model)
expected_beta <- data.frame(
    x0 = c(1, 2), x1 = c(1, 2),
    y0 = c(0, model_summary[model_summary$variable == "x11", "lcl"]),
    y1 = c(0, model_summary[model_summary$variable == "x11", "ucl"]),
    stringsAsFactors = FALSE)

test_that("beta", {
    expected <- expected_beta
    actual <- find_segments_to_plot(model, type = "beta")
    expect_equal(actual, expected)
})

test_that("OR", {
    expected <- expected_beta
    expected$y0 <- exp(expected$y0)
    expected$y1 <- exp(expected$y1)
    actual <- find_segments_to_plot(model, type = "OR")
    expect_equal(actual, expected)
})

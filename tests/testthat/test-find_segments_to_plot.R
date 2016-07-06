context("find_segments_to_plot")

source("setup.R")

test_that("find_segments_to_plot", {
    model <- fitted_models[[1]]
    s <- summary(model)
    expected <- data.frame(
        x0 = c(1, 2), x1 = c(1, 2),
        y0 = c(0, s[s$variable == "x11", "lcl"]),
        y1 = c(0, s[s$variable == "x11", "ucl"]),
        stringsAsFactors = FALSE)
    actual <- find_segments_to_plot(model)
    expect_equal(actual, expected)
})

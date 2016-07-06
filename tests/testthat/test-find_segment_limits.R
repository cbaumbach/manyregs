context("find_segment_limits")

test_that("find_segment_limits", {
    segments <- data.frame(x0 = 1:3, x1 = 3:5, y0 = 1:3, y1 = 5:7)
    actual <- find_segment_limits(segments)
    expect_equal(actual$x, c(1, 5))
    expect_equal(actual$y, c(1, 7))
})

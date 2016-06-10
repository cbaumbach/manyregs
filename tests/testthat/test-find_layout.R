context("find_layout")

test_that("1 x 1", {
    x <- find_layout(1, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0),
        c(0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$heights, c(margin_width(), plot_width(), margin_width()))
})

test_that("2 x 1", {
    x <- find_layout(2, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0),
        c(0, 2, 0),
        c(0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$heights, c(margin_width(), rep(plot_width(), 2), margin_width()))
})

test_that("1 x 2", {
    x <- find_layout(1, 2)
    mat <- rbind(
        c(0, 0, 0, 0),
        c(0, 1, 2, 0),
        c(0, 0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), rep(plot_width(), 2), margin_width()))
    expect_equal(x$heights, c(margin_width(), plot_width(), margin_width()))
})

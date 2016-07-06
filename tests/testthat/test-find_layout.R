context("find_layout")

test_that("1 x 1", {
    x <- find_layout(1, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$heights, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$bottom, 1)
    expect_equal(x$left, 1)
    expect_equal(x$top, 1)
    expect_equal(x$right, 1)
})

test_that("2 x 1", {
    x <- find_layout(2, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0),
        c(0, 2, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$heights, c(margin_width(), rep(plot_width(), 2), margin_width()))
    expect_equal(x$bottom, 2)
    expect_equal(x$left, 1:2)
    expect_equal(x$top, 1)
    expect_equal(x$right, 1:2)
})

test_that("1 x 2", {
    x <- find_layout(1, 2)
    mat <- rbind(
        c(0, 0, 0, 0),
        c(0, 1, 2, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(margin_width(), rep(plot_width(), 2), margin_width()))
    expect_equal(x$heights, c(margin_width(), plot_width(), margin_width()))
    expect_equal(x$bottom, 1:2)
    expect_equal(x$left, 1)
    expect_equal(x$top, 1:2)
    expect_equal(x$right, 2)
})

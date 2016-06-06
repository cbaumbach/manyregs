context("find_layout")

margin_width <- .5
plot_width <- 3

test_that("1 x 1", {
    x <- find_layout(1, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0),
        c(0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(lcm(margin_width), lcm(plot_width), lcm(margin_width)))
    expect_equal(x$heights, c(lcm(margin_width), lcm(plot_width), lcm(margin_width)))
})

test_that("2 x 1", {
    x <- find_layout(2, 1)
    mat <- rbind(
        c(0, 0, 0),
        c(0, 1, 0),
        c(0, 2, 0),
        c(0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(lcm(margin_width), lcm(plot_width), lcm(margin_width)))
    expect_equal(x$heights, c(lcm(margin_width), rep(lcm(plot_width), 2), lcm(margin_width)))
})

test_that("1 x 2", {
    x <- find_layout(1, 2)
    mat <- rbind(
        c(0, 0, 0, 0),
        c(0, 1, 2, 0),
        c(0, 0, 0, 0))
    expect_equal(x$mat, mat)
    expect_equal(x$widths, c(lcm(margin_width), rep(lcm(plot_width), 2), lcm(margin_width)))
    expect_equal(x$heights, c(lcm(margin_width), lcm(plot_width), lcm(margin_width)))
})

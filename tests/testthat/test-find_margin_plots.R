context("find_margin_plots")

test_that("1 x 1", {
    actual <- find_margin_plots(1, 1)
    expected <- list(bottom = 1L, left = 1L, top = 1L, right = 1L)
    expect_equal(actual, expected)
})

test_that("1 x 2", {
    actual <- find_margin_plots(1, 2)
    expected <- list(bottom = 1:2, left = 1L, top = 1:2, right = 2L)
    expect_equal(actual, expected)
})

test_that("2 x 2 by row", {
    actual <- find_margin_plots(2, 2, byrow = TRUE)
    expected <- list(bottom = c(3, 4), left = c(1, 3), # 1 2
        top = c(1, 2), right = c(2, 4))                # 3 4
    expect_equal(actual, expected)
})

test_that("2 x 2 by column", {
    actual <- find_margin_plots(2, 2)
    expected <- list(bottom = c(2, 4), left = c(1, 2), # 1 3
        top = c(1, 3), right = c(3, 4))                # 2 4
    expect_equal(actual, expected)
})

test_that("4 x 3 by row", {
    actual <- find_margin_plots(4, 3, byrow = TRUE)
    expected <- list(
        bottom = c(10, 11, 12),         #  1  2  3
        left = c(1, 4, 7, 10),          #  4  5  6
        top = c(1, 2, 3),               #  7  8  9
        right = c(3, 6, 9, 12))         # 10 11 12
    expect_equal(actual, expected)
})

context("find_layout_info")

test_that("find_layout_info", {
    actual <- find_layout_info(models, rows = "outcomes", columns = "exposures")
    expected <- find_layout(nrow = 2, ncol = 3)
    expect_equal(actual, expected)
})

context("compare_means")

test_data <- read.table(text = "
    x  y  low    high   any
    1  1  TRUE   FALSE  TRUE
    2  2  TRUE   FALSE  TRUE
    3  3  TRUE   FALSE  TRUE
    4  4  FALSE  TRUE   TRUE
    5  5  FALSE  TRUE   TRUE
    6  6  FALSE  TRUE   TRUE
    ", header = TRUE)

test_that("compare two identical groups", {
    actual <- with(test_data, compare_means(c("x", "y"), any, any, test_data))
    expected <- read.table(text = "
        variable pval pvalue
        x        1    1
        y        1    1
        ", header = TRUE, stringsAsFactors = FALSE)
    expect_equal(actual, expected)
})

test_that("compare two groups with different means", {
    actual <- with(test_data, compare_means(c("x", "y"), low, high, test_data))
    expect_equal(actual$variable, c("x", "y"))
    expect_true(all(actual$pvalue < .05))
})

test_that("with digits", {
    actual <- with(test_data, compare_means(c("x", "y"), low, high, test_data, digits = 2))
    pvalue <- with(test_data, t.test(x[low], x[high])$p.value)
    expect_equal(actual$variable, c("x", "y"))
    expect_equal(actual$pval, rep(round(pvalue, 2), 2))
    expect_true(all(actual$pvalue < .05))
})

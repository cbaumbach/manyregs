context("compare_distros")

set.seed(12345)
N <- 100L
test_data <- data.frame(
    sex = sample(c("male", "female"), N, TRUE),
    eyes = sample(c("blue", "brown", "green"), N, TRUE),
    hair = sample(c("brown", "blond", "black", "red"), N, TRUE),
    stringsAsFactors = FALSE)

test_that("without digits", {
    actual <- compare_distros(c("eyes", "hair"), "sex", test_data)
    expect_equal(actual$variable, c("eyes", "hair"))
    expect_equal(actual$by, rep("sex", nrow(actual)))
    expect_true(all(c("pval", "pvalue") %in% names(actual)))
})

test_that("with digits", {
    digits <- 2L
    actual <- compare_distros(c("eyes", "hair"), "sex", test_data, digits)
    expect_equal(nchar(actual$pval), rep(digits + 1 + 1, nrow(actual)))
})

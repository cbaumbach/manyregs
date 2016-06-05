context("compare_means")

test_data <- read.table(text = "
    x     sex  sex_NA   eyes   hair
    1    male    male   blue  brown
    2    male    male   blue  blond
    3    male    male  brown    red
    4    male      NA  brown  black
    1  female  female   blue  brown
    2  female  female   blue  blond
    3  female  female  brown    red
    4  female      NA  brown  black
    ", header = TRUE)
test_data$y <- test_data$x

template <- read.table(text = "
    variable  by  pval pvalue
           x   ?     1      1
           y   ?     1      1
    ", header = TRUE, stringsAsFactors = FALSE)

test_that("compare two identical groups", {
    actual <- with(test_data, compare_means(c("x", "y"), "sex", test_data))
    expected <- template
    expected$by <- "sex"
    expect_equal(actual, expected)
})

test_that("compare two identical groups with missings", {
    actual <- with(test_data, compare_means(c("x", "y"), "sex_NA", test_data))
    expected <- template
    expected$by <- "sex_NA"
    expect_equal(actual, expected)
})

test_that("with digits", {
    digits <- 3L
    actual <- with(test_data, compare_means(c("x", "y"), "eyes", test_data, digits))
    pvalue <- with(test_data, t.test(x[eyes == "blue"], x[eyes == "brown"])$p.value)
    expect_equal(actual$pval, rep(round(pvalue, digits), 2))
})

test_that("error if by-variable doesn't have 2 levels", {
    expect_error(compare_means("x", "hair", test_data),
        "by-variable must have exactly two levels: hair")
})

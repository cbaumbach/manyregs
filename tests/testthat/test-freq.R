context("freq")

x <- c(NA, 2, 1, 1, 2, 1)
test_data <- data.frame(x = as.double(x), y = as.integer(x), z = as.character(x))
template <- read.table(text = "
    variable category N n %
    x        1        6 3 50
    x        2        6 2 33.33
    x        NA       6 1 16.67
    ", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c("character", "character", "integer", "integer", "numeric"))

test_that("without label and without digits", {
    actual <- freq(x)
    expected <- template
    expect_equal(actual, expected)
})

test_that("with label", {
    actual <- freq(x, label = "foobar")
    expected <- template
    expected$variable <- "foobar"
    expect_equal(actual, expected)
})

test_that("with digits", {
    actual <- freq(x, digits = 1)
    expected <- template
    expected$"%" <- round(expected$"%", 1)
    expect_equal(actual, expected)
})

test_that("freqs without digits", {
    actual <- freqs(c("x", "y", "z"), test_data)
    expected <- rbind(template, template, template)
    expected$variable <- rep(c("x", "y", "z"), each = 3)
    expect_equal(actual, expected)
})

test_that("freqs with digits", {
    actual <- freqs(c("x", "y", "z"), test_data, digits = 1)
    expected <- rbind(template, template, template)
    expected$variable <- rep(c("x", "y", "z"), each = 3)
    expected$"%" <- round(expected$"%", 1)
    expect_equal(actual, expected)
})

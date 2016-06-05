context("distro")

x <- c(1:3, NA)
probs <- c(.015, .25, .50, .75)
test_data <- data.frame(x = x, y = x, z = x)
template <- read.table(text = "
    variable N NAs mean sd iqr min p1.5 p25 p50 p75 max
    x        4 1   2    1  1   1   1.03 1.5 2   2.5 3
    ", header = TRUE, stringsAsFactors = FALSE)

test_that("without label and without digits", {
    actual <- distro(x, probs = probs)
    expected <- template
    expect_equal(actual, expected)
})

test_that("with label", {
    actual <- distro(x, label = "foobar", probs = probs)
    expected <- template
    expected$variable <- "foobar"
    expect_equal(actual, expected)
})

test_that("with digits", {
    actual <- distro(x, probs = probs, digits = 1)
    expected <- template
    expected[-1] <- lapply(expected[-1], round, digits = 1)
    expect_equal(actual, expected)
})

test_that("distros without digits", {
    actual <- distros(c("x", "y", "z"), test_data, probs = probs)
    expected <- rbind(template, template, template)
    expected$variable <- c("x", "y", "z")
    expect_equal(actual, expected)
})

test_that("distros with digits", {
    actual <- distros(c("x", "y", "z"), test_data, probs = probs, digits = 1)
    expected <- rbind(template, template, template)
    expected$variable <- c("x", "y", "z")
    expected[-1] <- lapply(expected[-1], round, digits = 1)
    expect_equal(actual, expected)
})

context("manyregs_term")

test_that("quantitative variable", {
    data <- create_dataset("x")
    term <- new_term("x", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
            x        x    ''
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("factor", {
    data <- create_dataset(c("x", 2))
    term <- new_term("x", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
           x1        x     1
           x2        x     2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("interaction between 2 quantitative variables", {
    data <- create_dataset("x", "y")
    term <- new_term("x:y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
          x:y      x:y    ''
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("interaction between a quantitative variable and a factor", {
    data <- create_dataset("x", c("y", 2))
    term <- new_term("x:y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
         x:y1      x:y     1
         x:y2      x:y     2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("interaction between a factor and a quantitative variable", {
    data <- create_dataset(c("x", 2), "y")
    term <- new_term("x:y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
         x1:y      x:y     1
         x2:y      x:y     2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("interaction between 2 factors", {
    data <- create_dataset(c("x", 3), c("y", 2))
    term <- new_term("x:y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
        x1:y1      x:y   1:1
        x2:y1      x:y   2:1
        x3:y1      x:y   3:1
        x1:y2      x:y   1:2
        x2:y2      x:y   2:2
        x3:y2      x:y   3:2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("factor crossing between 2 quantitative variables", {
    data <- create_dataset("x", "y")
    term <- new_term("x*y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
            x        x    ''
            y        y    ''
          x:y      x:y    ''
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("factor crossing between a quantitative variable and a factor", {
    data <- create_dataset("x", c("y", 2))
    term <- new_term("x * y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
            x        x    ''
           y1        y     1
           y2        y     2
         x:y1      x:y     1
         x:y2      x:y     2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("factor crossing between a factor and a quantitative variable", {
    data <- create_dataset(c("x", 2), "y")
    term <- new_term("x*y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
           x1        x     1
           x2        x     2
            y        y    ''
         x1:y      x:y     1
         x2:y      x:y     2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

test_that("factor crossing between 2 factors", {
    data <- create_dataset(c("x", 3), c("y", 2))
    term <- new_term("x*y", data)
    actual <- summary(term)
    expected <- read.table(text = "
        label variable level
           x1        x     1
           x2        x     2
           x3        x     3
           y1        y     1
           y2        y     2
        x1:y1      x:y   1:1
        x2:y1      x:y   2:1
        x3:y1      x:y   3:1
        x1:y2      x:y   1:2
        x2:y2      x:y   2:2
        x3:y2      x:y   3:2
    ", header = TRUE, colClasses = "character")
    expect_equal(actual, expected)
})

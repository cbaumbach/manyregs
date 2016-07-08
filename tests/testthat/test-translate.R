context("translate")

from_to <- c("a" = "A")

test_that("happy path", {
    expect_equal(translate(c("a", "b"), from_to), c("A", "b"))
})

test_that("NULL translates to NULL", {
    expect_equal(translate(NULL, from_to), NULL)
})

test_that("NULL dictionary keeps defaults", {
    expect_equal(translate("a", NULL), "a")
})

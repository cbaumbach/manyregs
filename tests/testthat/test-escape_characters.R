context("escape_characters")

test_that("no characters to escape", {
    expect_equal(escape_characters("x", "z"), "x")
})

test_that("happy path", {
    expect_equal(escape_characters("factor(x)", "[()]"), "factor\\(x\\)")
})

test_that("escape a and ]", {
    expect_equal(escape_characters("a]", "[]a]"), "\\a\\]")
})

test_that("escape a, -, and z", {
    expect_equal(escape_characters("a-z", "[-az]"), "\\a\\-\\z")
})

test_that("escape ^ and a", {
    expect_equal(escape_characters("a^", "[a^]"), "\\a\\^")
})

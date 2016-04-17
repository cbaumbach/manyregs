context("create_models")

foo <- function() {
    # empty
}

test_that("one model", {
    models <- create_models("y", "x", list(c("z1", "z2")), foo)
    expect_equal(1, length(models))
    m <- models[[1]]
    expect_true(all(c("outcome", "exposure", "adjustment", "f", "fname") %in% names(m)))
    expect_equal("y", m$outcome)
    expect_equal("x", m$exposure)
    expect_equal(c("z1", "z2"), m$adjustment)
    expect_equal(foo, m$f)
    expect_equal("foo", m$fname)
})

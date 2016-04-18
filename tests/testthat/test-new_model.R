context("new_model")

test_that("from scratch", {
    foo <- function(model, data) { }
    m <- new_model("outcome", "exposure", "adjustment", foo, "foo")
    expect_equal(m$outcome, "outcome")
    expect_equal(m$exposure, "exposure")
    expect_equal(m$adjustment, "adjustment")
    expect_equal(m$f, foo)
    expect_equal(m$fname, "foo")
})

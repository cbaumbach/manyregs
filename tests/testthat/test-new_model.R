context("new_model")

foo <- function(model, data) { }
template <- new_model("outcome", "exposure", "adjustment", foo, "foo")

test_that("from scratch", {
    m <- template
    expect_equal(m$outcome, "outcome")
    expect_equal(m$exposure, "exposure")
    expect_equal(m$adjustment, "adjustment")
    expect_equal(m$f, foo)
    expect_equal(m$fname, "foo")
})

test_that("copy from template", {
    expect_equal(new_model(template = template), template)
})
context("new_model")

foo <- function(model, data) { }
template <- new_model("outcome", "exposure", "adjustment", foo, "foo")
without_members <- function(model, members) {
    x <- model[setdiff(names(model), members)]
    class(x) <- class(model)
    x
}

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

test_that("add slots to template", {
    m <- new_model(template = template, extra_slots = list(extra = "extra"))
    expect_equal(m$extra, "extra")
    expect_equal(without_members(m, "extra"), template)
})

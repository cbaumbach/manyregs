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

test_that("add slots to template", {
    m <- new_model(template = template, extra_slots = list(extra = "extra"))
    expect_equal(m$extra, "extra")
    expect_equal(remove_slots(m, "extra"), template)
})

test_that("modify slots in template", {
    m <- new_model(template = template, extra_slots = list(outcome = "changed"))
    expect_equal(m$outcome, "changed")
})

test_that("delete slots in template", {
    m <- new_model(template = template, extra_slots = list(outcome = NULL))
    expect_false("outcome" %in% names(m))
    expect_equal(m$outcome, NULL)
})

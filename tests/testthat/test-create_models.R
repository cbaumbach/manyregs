context("create_models")

foo <- function() {
    # empty
}

test_that("1 outcome, 1 exposure, 1 adjustment", {
    outcome <- "y"
    exposure <- "x"
    adjustment <- c("z1", "z2")
    models <- create_models(outcome, exposure, list(adjustment), foo)
    expect_equal(1, length(models))
    m <- models[[1]]
    expect_equal(outcome, m$outcome)
    expect_equal(exposure, m$exposure)
    expect_equal(adjustment, m$adjustment)
    expect_equal(foo, m$f)
    expect_equal("foo", m$fname)
})

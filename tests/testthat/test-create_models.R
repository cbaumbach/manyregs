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

test_that("2 outcomes, 1 exposure, 1 adjustment", {
    outcomes <- c("y1", "y2")
    exposure <- "x"
    adjustment <- c("z1", "z2")
    models <- create_models(outcomes, exposure, list(adjustment), foo)
    expect_equal(2, length(models))
    # Model 1
    m1 <- models[[1]]
    expect_equal(outcomes[1], m1$outcome)
    expect_equal(exposure, m1$exposure)
    expect_equal(adjustment, m1$adjustment)
    expect_equal(foo, m1$f)
    expect_equal("foo", m1$fname)
    # Model 2
    m2 <- models[[2]]
    expect_equal(outcomes[2], m2$outcome)
    expect_equal(exposure, m2$exposure)
    expect_equal(adjustment, m2$adjustment)
    expect_equal(foo, m2$f)
    expect_equal("foo", m2$fname)
})


test_that("1 outcome, 1 exposure, 0 adjustment", {
    outcome <- "y"
    exposure <- "x"
    models <- create_models(outcome, exposure, f = foo)
    expect_equal(1, length(models))
    m <- models[[1]]
    expect_equal(outcome, m$outcome)
    expect_equal(exposure, m$exposure)
    expect_equal(NULL, m$adjustment)
    expect_equal(foo, m$f)
    expect_equal("foo", m$fname)
})

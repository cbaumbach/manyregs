context("create_models")

foo <- function() {
    # empty
}

# Return TRUE if, and only if, one of the models matches in terms of
# outcome, exposure, and so on.
has_model_with <- function(models, outcome, exposure, adjustment, f, fname) {
    is_match <- function(m) {
        outcome == m$outcome &&
            exposure == m$exposure &&
            all(adjustment == m$adjustment) &&
            identical(f, m$f) &&
            fname == m$fname
    }
    any(vapply(models, is_match, logical(1)))
}

test_that("1 outcome, 1 exposure, 1 adjustment", {
    models <- create_models("y", "x", list(c("z1", "z2")), foo)
    expect_equal(1, length(models))
    expect_true(has_model_with(models, "y", "x", c("z1", "z2"), foo, "foo"))
})

test_that("a single adjustment can be specified as character vector", {
    models <- create_models("y", "x", c("z1", "z2"), foo)
    expect_equal(1, length(models))
    expect_true(has_model_with(models, "y", "x", c("z1", "z2"), foo, "foo"))
})

test_that("2 outcomes, 2 exposures, 2 adjustment", {
    models <- create_models(c("y-1", "y-2"), c("x-1", "x-2"), list("z-1", "z-2"), foo)
    expect_equal(8, length(models))
    expect_true(has_model_with(models, "y-1", "x-1", "z-1", foo, "foo"))
    expect_true(has_model_with(models, "y-1", "x-1", "z-2", foo, "foo"))
    expect_true(has_model_with(models, "y-1", "x-2", "z-1", foo, "foo"))
    expect_true(has_model_with(models, "y-1", "x-2", "z-2", foo, "foo"))
    expect_true(has_model_with(models, "y-2", "x-1", "z-1", foo, "foo"))
    expect_true(has_model_with(models, "y-2", "x-1", "z-2", foo, "foo"))
    expect_true(has_model_with(models, "y-2", "x-2", "z-1", foo, "foo"))
    expect_true(has_model_with(models, "y-2", "x-2", "z-2", foo, "foo"))
})

test_that("1 outcome, 1 exposure, 0 adjustment", {
    models <- create_models("y", "x", f = foo)
    expect_equal(1, length(models))
    expect_true(has_model_with(models, "y", "x", NULL, foo, "foo"))
})

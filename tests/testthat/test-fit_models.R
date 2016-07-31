context("fit_models")

data <- create_dataset("y", "x1", "x2", "z")
f <- function(model, data) as.character(model)
models <- create_models("y", c("x1", "x2"), "z", f)

discard_closures <- function(x) {
    structure(Filter(Negate(is.function), x), class = class(x))
}

test_that("fitting many models at once gives the same results as separate fitting", {
    actual <- fit_models(models, NULL)
    for (i in seq_along(models))
        expect_identical(actual[[i]], fit_model(models[[i]], NULL))
})

test_that("sequential and parallel execution yield the same result", {
    sequential <- fit_models(models, NULL)
    parallel <- fit_models(models, NULL, cores = 2)
    # The f-slots of the models, that contain the function used to fit
    # the models, are not identical after sequential and parallel
    # execution.
    for (i in seq_along(models)) {
        expect_true(identical(f, sequential[[i]]$f))
        expect_false(identical(f, parallel[[i]]$f))
    }
    # Parallel execution works by forking and the R process makes
    # copies of itself.  I guess that's the problem.  So I remove
    # closures before comparison.
    sequential <- lapply(sequential, discard_closures)
    parallel <- lapply(parallel, discard_closures)
    expect_identical(sequential, parallel)
})

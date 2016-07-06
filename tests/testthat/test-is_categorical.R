context("is_categorical")

test_that("categorical variable", {
    model <- filter_models(fitted_models, exposure = "x1", combine = "and")[[1]]
    expect_true(is_categorical("x1", model))
})

test_that("continuous variable", {
    model <- filter_models(fitted_models, exposure = "x3", combine = "and")[[1]]
    expect_false(is_categorical("x3", model))
})

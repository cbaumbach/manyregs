context("create_models")

test_that("happy path", {
    create_models("y", "x", list("z"), foo)
})

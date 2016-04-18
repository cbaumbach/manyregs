context("formula")

test_that("with adjustment", {
    expect_equal(formula(new_model("y", "x", "z", NULL, "f")), y ~ x + z)
})

test_that("without adjustment", {
    expect_equal(formula(new_model("y", "x", NULL, NULL, "f")), y ~ x)
})

context("as.character")

test_that("with adjustment", {
    model <- new_model("y", "x", c("z1", "z2"), NULL, "f")
    expect_equal(as.character(model), "y ~ x + z1 + z2  (f)")
})

test_that("without adjustment", {
    model <- new_model("y", "x", NULL, NULL, "f")
    expect_equal(as.character(model), "y ~ x  (f)")
})

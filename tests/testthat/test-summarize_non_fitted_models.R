context("summarize non-fitted models")

test_that("summarize_models", {
    f <- function(model, test_data) { }
    models <- create_models(c("y1", "y2"), "x", "z", f)
    expected <- data.frame(stringsAsFactors = FALSE,
        models = c("y1 ~ x + z  (f)", "y2 ~ x + z  (f)"))
    expect_equal(summarize_models(models), expected)
})

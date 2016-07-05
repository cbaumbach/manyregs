context("summarize non-fitted models")

expected <- data.frame(
    model = c(
        "y1 ~ x1  (linear)",
        "y1 ~ x1 + z1  (linear)",
        "y1 ~ x1 + z1 + z2  (linear)",
        "y1 ~ x2  (linear)",
        "y1 ~ x2 + z1  (linear)",
        "y1 ~ x2 + z1 + z2  (linear)",
        "y1 ~ x3  (linear)",
        "y1 ~ x3 + z1  (linear)",
        "y1 ~ x3 + z1 + z2  (linear)",
        "y2 ~ x1  (linear)",
        "y2 ~ x1 + z1  (linear)",
        "y2 ~ x1 + z1 + z2  (linear)",
        "y2 ~ x2  (linear)",
        "y2 ~ x2 + z1  (linear)",
        "y2 ~ x2 + z1 + z2  (linear)",
        "y2 ~ x3  (linear)",
        "y2 ~ x3 + z1  (linear)",
        "y2 ~ x3 + z1 + z2  (linear)"
    ), stringsAsFactors = FALSE)

test_that("summarize_models", {
    expect_equal(summarize_models(models), expected)
})

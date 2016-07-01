context("string conversion of adjustments")

adjustments <- list(NULL, "z1", c("z1", "z2"))
adjustment_string <- c("NULL", "z1", sprintf("z1%sz2", adjustment_separator))

test_that("adjustment_to_string", {
    expect_equal(adjustment_string, adjustment_to_string(adjustments))
})

test_that("adjustment_from_string", {
    expect_equal(adjustments, adjustment_from_string(adjustment_string))
})

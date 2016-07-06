context("find_number_of")

source("setup.R")

test_that("find_number_of", {
    expect_equal(find_number_of("outcomes", models), 2L)
    expect_equal(find_number_of("exposures", models), 3L)
    expect_equal(find_number_of("adjustments", models), 3L)
})

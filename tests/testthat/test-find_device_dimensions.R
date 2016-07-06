context("find_device_dimensions")

source("setup.R")

test_that("find_device_dimensions", {
    dimension <- find_device_dimensions(models, rows = "exposures", columns = "outcomes")
    expected_width_in_cm <- length(outcomes) * cm_to_double(plot_width()) + 2 * cm_to_double(margin_width())
    expected_height_in_cm <- length(exposures) * cm_to_double(plot_width()) + cm_to_double(margin_width()) + outer_margin_in_cm()
    expect_equal(dimension$width, cm_to_inches(expected_width_in_cm))
    expect_equal(dimension$height, cm_to_inches(expected_height_in_cm))
})

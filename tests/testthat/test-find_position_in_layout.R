source("setup.R")

expect_positions <- function(model_numbers, true_positions, layout_info) {
    all_positions <- c("bottom", "left", "top", "right")
    false_positions <- setdiff(all_positions, true_positions)
    for (model_number in model_numbers) {
        position <- find_position_in_layout(model_number, layout_info)
        for (pos in true_positions) {
            if (!position[[pos]]) {
                fail(paste0("Model ", model_number, " should be in position \"", pos, "\""))
                return()
            }
        }
        for (pos in false_positions) {
            if (position[[pos]]) {
                fail(paste0("Model ", model_number, " shouldn't be in position \"", pos, "\""))
                return()
            }
        }
    }
    succeed()
}

context("find_position_in_layout")

test_that("find_position_in_layout", {
    sorted_models <- sort_models_for_plotting(models, rows = "exposures", columns = "outcomes")
    layout_info <- find_layout(find_number_of("exposures", sorted_models), find_number_of("outcomes", models))

    # page 1:  1 2   page 2:   7  8   page 3:  13 14
    #          3 4             9 10            15 16
    #          5 6            11 12            17 18

    expect_positions(c(1, 7, 13), c("left", "top"), layout_info)
    expect_positions(c(2, 8, 14), c("right", "top"), layout_info)
    expect_positions(c(3, 9, 15), c("left"), layout_info)
    expect_positions(c(4, 10, 16), c("right"), layout_info)
    expect_positions(c(5, 11, 17), c("left", "bottom"), layout_info)
    expect_positions(c(6, 12, 18), c("right", "bottom"), layout_info)
})

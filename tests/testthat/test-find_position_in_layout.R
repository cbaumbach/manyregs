context("find_position_in_layout")

sorted_models <- sort_models_for_plotting(models, rows = "exposures", columns = "outcomes")
layout_info <- find_layout(find_number_of("exposures", sorted_models), find_number_of("outcomes", models))

expect_positions_helper <- function(position, true_positions) {
    all_positions <- c("bottom", "left", "top", "right")
    false_positions <- setdiff(all_positions, true_positions)
    for (pos in true_positions)
        expect_true(position[[pos]])
    for (pos in false_positions)
        expect_false(position[[pos]])
}

expect_positions <- function(model_numbers, true_positions) {
    for (model_number in model_numbers) {
        position <- find_position_in_layout(model_number, layout_info)
        expect_positions_helper(position, true_positions)
    }
}

# page 1:  1 2   page 2:   7  8   page 3:  13 14
#          3 4             9 10            15 16
#          5 6            11 12            17 18

test_that("find_position_in_layout", {
    expect_positions(c(1, 7, 13), c("left", "top"))
    expect_positions(c(2, 8, 14), c("right", "top"))
    expect_positions(c(3, 9, 15), c("left"))
    expect_positions(c(4, 10, 16), c("right"))
    expect_positions(c(5, 11, 17), c("left", "bottom"))
    expect_positions(c(6, 12, 18), c("right", "bottom"))
})

#' Find layout for plots
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @return A list with elements "mat", "widths", "heights" that can be
#'     used as arguments to \code{\link[graphics]{layout}}.  There are
#'     also elements "bottom", "left", "top", "right" that are integer
#'     vectors containing the numbers of the subplots at the bottom,
#'     left, top, and right margin, respectively.
#'
find_layout <- function(nrow, ncol) {
    m <- matrix(seq_len(nrow * ncol), nrow = nrow, byrow = TRUE)
    mat <- cbind(0, rbind(0, m), 0)
    widths <- c(margin_width(), rep_len(plot_width(), ncol), margin_width())
    heights <- c(margin_width(), rep_len(plot_width(), nrow), margin_width())
    list(mat = mat, widths = widths, heights = heights,
        bottom = m[nrow,], left = m[,1], top = m[1,], right = m[,ncol])
}

margin_width <- function() {
    lcm(.5)
}

plot_width <- function() {
    lcm(3)
}

#' Convenience wrapper around \code{find_layout}
#'
#' @param models A list of models
#' @param rows One of "outcomes", "exposures", or "adjustments"
#' @param columns One of "outcomes", "exposures", or "adjustments"
#' @return A list as returned by \code{\link{find_layout}}.
find_layout_info <- function(models, rows, columns) {
    find_number_of <- function(dimension) {
        length(eval(parse(text = sprintf("find_%s(models)", dimension))))
    }
    find_layout(find_number_of(rows), find_number_of(columns))
}

#' Find outcomes, exposures, adjustments from a list of models
#'
#' @param models List of model objects
#' @return A list with elements "outcomes", "exposures", and
#'     "adjustments" representing the outcomes, exposures, and
#'     adjustments found among the models.
#'
find_variables <- function(models) {
    list(outcomes = find_outcomes(models),
        exposures = find_exposures(models),
        adjustments = find_adjustments(models))
}

find_outcomes <- function(models) {
    find_outcomes_or_exposures(models, "outcome")
}

find_exposures <- function(models) {
    find_outcomes_or_exposures(models, "exposure")
}

find_outcomes_or_exposures <- function(models, type) {
    unique(vapply(models, `[[`, character(1), type))
}

find_adjustments <- function(models) {
    unique_list <- function(x) {
        x[!duplicated(x)]
    }
    unique_list(lapply(models, `[[`, "adjustment"))
}

#' Match values of variables to different parts of a layout
#'
#' @param variables List with the same structure as the return value
#'     of \code{\link{find_variables}}.
#' @param types Character vector with named elements "pages", "rows",
#'     "columns" where every element is one of "outcomes",
#'     "exposures", "adjustments" and no two elements are the same.
#' @return The layout consists of pages, rows, and columns.  Every
#'     part of the layout represents one type of variable: outcomes,
#'     exposures, adjustments.  The function takes the values of the
#'     different types of variables (`variables`) as well as the
#'     correspondence between parts of the layout and the types of
#'     variables (`types`) and returns a list with elements "page",
#'     "row", "column" where every element contains the values of the
#'     type of variable that matches the corresponding part of the
#'     layout.
#'
find_page_row_column_variables <- function(variables, types) {
    setNames(variables[types], names(types))
}

#' Map outcomes, exposures, and adjustments to pages, rows, and columns
#'
#' @param rows One of "outcomes", "exposures", or "adjustments"
#' @param columns One of "outcomes", "exposures", or "adjustments"
#' @details The \code{rows} and \code{columns} argument must either
#'     both be NULL or both be non-NULL.  If \code{rows} and
#'     \code{columns} are non-NULL, they must not have the same value.
#' @return List with elements "pages", "rows", and "columns".
#'
find_pages_rows_columns <- function(rows = NULL, columns = NULL) {
    if (length(Filter(is.null, list(rows, columns))) == 1L)
        stop("Arguments \"rows\" and \"columns\" must either both be non-NULL or both be NULL.")
    if (is.null(rows))
        rows <- "outcomes"
    if (is.null(columns))
        columns <- "exposures"
    if (identical(rows, columns))
        stop("Arguments \"rows\" and \"columns\" must not have the same value.")
    variable_types <- c("outcomes", "exposures", "adjustments")
    if (!all(c(rows, columns) %in% variable_types))
        stop("Arguments \"rows\" and \"columns\" must be one of \"outcomes\", \"exposures\", or \"adjustments\".")
    pages <- setdiff(variable_types, c(rows, columns))
    list(pages = pages, rows = rows, columns = columns)
}

#' Sort a list of models
#'
#' @param models List of models
#' @param by Character vector defining how ties are broken
#' @param outcomes Character vector giving order of outcome variables
#' @param exposures Character vector giving order of exposure
#'     variables
#' @param adjustments List of character vectors giving order of
#'     adjustments
#'
#' @details The \code{by} argument defines the order in which
#'     outcomes, exposures, and adjustments are use to sort the list
#'     of models.  For example, if \code{by} is \code{c("outcomes",
#'     "exposures", "adjustments")}, models are first sorted by
#'     outcomes.  Any ties are broken by sorting by exposures.  Any
#'     remaining ties are broken by sorting by adjustments.
#'
#'     The sort order within outcomes (exposures, adjustments) is
#'     defined by the argument of the same name.  By default the sort
#'     order within outcomes (exposures, adjustments) is defined by
#'     the order of appearance of outcomes (exposures, adjustments) in
#'     the list of models.  By specifying only a subset of outcomes
#'     (exposures, adjustments) it is possible to obtain a sorted
#'     subset of models.
#'
#' @return A list of sorted models.
#'
#' @export
sort_models <- function(models, by, outcomes = NULL, exposures = NULL, adjustments = NULL) {
    models <- filter_models(models, outcomes, exposures, adjustments)
    variables <- find_selected_variables(models, outcomes, exposures, adjustments)
    x <- find_combinations(variables$outcomes, variables$exposures, variables$adjustments, by)
    extract_model <- function(outcome, exposure, adjustment) {
        filter_models(models, outcome, exposure, adjustment, combine = "and")[[1]]
    }
    Map(extract_model, x$outcomes, x$exposures, x$adjustments)
}

#' Sort models for plotting in a layout of pages, rows, and columns
#'
#' @param models List of models
#' @param rows One of "outcomes", "exposures", or "adjustments"
#' @param columns One of "outcomes", "exposures", or "adjustments"
#' @details The \code{rows} and \code{columns} arguments must not have
#'     the same value.
#' @return A list of models sorted such that we can step through the
#'     list plotting one model after the other and every model will
#'     appear on the correct page in the correct row and column.
sort_models_for_plotting <- function(models, rows = NULL, columns = NULL) {
    sort_models(models, find_pages_rows_columns(rows, columns))
}

#' Find row, column, and page labels for model
#'
#' @param model A model object
#' @param rows One of "outcomes", "exposures", or "adjustments"
#' @param columns One of "outcomes", "exposures", or "adjustments"
#' @return A list with elements "row", "column", and "page" containing
#'     the row, column, and page labels for the model.
find_plot_labels <- function(model, rows, columns) {
    x <- find_pages_rows_columns(rows, columns)
    find_label_for <- function(dimension) {
        paste(model[[switch(dimension, outcomes = "outcome",
            exposures = "exposure", adjustments = "adjustment")]],
            collapse = ", ")
    }
    list(row = find_label_for(x$rows),
        column = find_label_for(x$columns),
        page = sprintf("%s: %s", sub("s$", "", x$pages), find_label_for(x$pages)))
}

#' Is variable categorical in model?
#'
#' @param variable Name of covariate in \code{model}
#' @param model A model object
#' @return Returns \code{TRUE} if \code{variable} is categorical in
#'     \code{model}, otherwise (if \code{variable} is continuous)
#'     returns \code{FALSE}.
is_categorical <- function(variable, model) {
    ! variable %in% summary(model)$variable
}

#' Find segments to plot (as confidence intervals)
#'
#' @param model A fitted model object
#' @return A data frame with columns "x0", "x1", "y0", and "y1" that
#'     can be used as arguments to \code{\link[graphics]{segments}}.
find_segments_to_plot <- function(model) {
    x <- find_exposure_confidence_intervals(model)
    if (is_categorical(model$exposure, model))
        x <- rbind(c(0, 0), x)
    data.frame(x0 = seq_len(nrow(x)), x1 = seq_len(nrow(x)),
        y0 = x$lcl, y1 = x$ucl)
}

find_exposure_confidence_intervals <- function(model) {
    x <- summary(model)
    pattern <- escape_characters(sprintf("^%s\\d*$", model$exposure), "[()]")
    x[grep(pattern, x$variable), c("lcl", "ucl"), drop = FALSE]
}

#' Escape characters from character class
#'
#' @param x Character vector in which to escape characters
#' @param character_class Length-1 character vector defining a
#'     "character class" (see \code{\link[base]{regex}})
#' @return Character vector \code{x} in which the characters in
#'     \code{character_class} are escaped.
escape_characters <- function(x, character_class) {
    gsub(paste0("(", character_class, ")"), "\\\\\\1", x)
}

#' Find x and y ranges of segments
#'
#' @param segment Data frame of segments as returned by
#'     \code{\link{find_segments_to_plot}}
#' @return A list with elements "x" and "y" giving the x and y ranges
#'     of the segments in \code{segments}.
find_segment_limits <- function(segment) {
    list(x = range(c(segment$x0, segment$x1)),
        y = range(c(segment$y0, segment$y1)))
}

#' Find x and y ranges for confidence intervals for a list of models
#'
#' @param models A list of models
#' @return A list with elements "xlim" and "ylim" containing the x and
#'     y ranges needed for plotting confidence intervals for the
#'     exposures of a list of models.
find_xy_ranges <- function(models) {
    segments <- lapply(models, find_segments_to_plot)
    xylims <- lapply(segments, find_segment_limits)
    xylim <- Reduce(function(maximum, current) {
        list(xlim = range(c(maximum$x, current$x)),
            ylim = range(c(maximum$y, current$y)))
    }, xylims)
    xylim$x <- xylim$x + .5 * c(-1, +1)
    xylim
}
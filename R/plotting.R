#' Plot model.
#'
#' @param x Model to be printed
#' @param \dots Ignored
#' @param labels Named character vector or named list.
#' @details If a variable in the model names one of the elements of
#'     \code{labels}, its name will be replaced by that element when
#'     used as a label.
#' @return None.
#'
#' @export
plot.manyregs_model <- function(x, ..., labels = NULL) {
    plot_models(list(x), labels = labels)
}

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
    heights <- c(margin_width(), rep_len(plot_width(), nrow))
    list(mat = mat, widths = widths, heights = heights,
        bottom = m[nrow,], left = m[,1], top = m[1,], right = m[,ncol])
}

margin_width <- function() {
    lcm(1)
}

plot_width <- function() {
    lcm(3)
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
#' @param labels Character vector with named elements (same as
#'     \code{from_to} argument of \code{\link{translate}})
#' @return A list with elements "row", "column", and "page" containing
#'     the row, column, and page labels for the model.
find_plot_labels <- function(model, rows, columns, labels = NULL) {
    x <- find_pages_rows_columns(rows, columns)
    find_label_for <- function(dimension) {
        default_labels <- model[[switch(dimension, outcomes = "outcome",
            exposures = "exposure", adjustments = "adjustment")]]
        new_labels <- translate(default_labels, labels)
        paste(Filter(Negate(is.null), new_labels), collapse = ", ")
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
#' @param type Confidence intervals to plot ("beta" or "OR")
#' @return A data frame with columns "x0", "x1", "y0", "y1", and
#'     "midpoints" that can be used as arguments to
#'     \code{\link[graphics]{segments}}.  The midpoints correspond to
#'     position of the effect estimate within the confidence interval.
find_segments_to_plot <- function(model, type = "beta") {
    x <- find_estimates_for(model$exposure, model)
    if (is_categorical(model$exposure, model))
        x <- rbind(0, x)
    segments <- data.frame(
        x0 = seq_len(nrow(x)),
        x1 = seq_len(nrow(x)),
        y0 = x$lcl,
        y1 = x$ucl,
        midpoints = x$beta)
    if (type == "OR") {
        cols <- c("y0", "y1", "midpoints")
        segments[cols] <- lapply(segments[cols], exp)
    }
    attr(segments, "reference_line") <- switch(type, beta = 0, OR = 1)
    segments
}

find_estimates_for <- function(variable, model) {
    summary_table <- summary(model)
    fmt <- if (is_categorical(variable, model)) "^%s" else "^%s$"
    pattern <- escape_characters(sprintf(fmt, variable), "[()]")
    summary_table[grep(pattern, summary_table$variable), , drop = FALSE]
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
#' @param type Confidence intervals to plot ("beta" or "OR")
#' @return A list with elements "xlim" and "ylim" containing the x and
#'     y ranges needed for plotting confidence intervals for the
#'     exposures of a list of models.
find_xy_ranges <- function(models, type = "beta") {
    segments <- lapply(models, find_segments_to_plot, type = type)
    xylims <- lapply(segments, find_segment_limits)
    xylim <- Reduce(function(maximum, current) {
        list(xlim = range(c(maximum$x, current$x)),
            ylim = range(c(maximum$y, current$y)))
    }, xylims, xylims[[1]])
    xylim$xlim <- xylim$xlim + .5 * c(-1, +1)
    xylim
}

#' Find the position of a model in a layout
#'
#' @param model_number Index of model in the list of models that is
#'     used for plotting
#' @param layout_info Layout information as returned by
#'     \code{\link{find_layout}}
#' @return A list with elements "bottom", "left", "top", and "right"
#'     where an element is \code{TRUE} if the model is located in the
#'     respective part of the layout, otherwise the element is
#'     \code{FALSE}.
find_position_in_layout <- function(model_number, layout_info) {
    models_per_page <- sum(layout_info$mat != 0L)
    model_number <- (model_number - 1L) %% models_per_page + 1L
    list(bottom = model_number %in% layout_info$bottom,
        left = model_number %in% layout_info$left,
        top = model_number %in% layout_info$top,
        right = model_number %in% layout_info$right)
}

#' Find dimensions of device region for plotting models
#'
#' @param models A list of models
#' @param rows One of "outcomes", "exposures", or "adjustments"
#' @param columns One of "outcomes", "exposures", or "adjustments"
#' @return A list with elements "width" and "height" containing the
#'     recommended width and height (in inches) for plotting the
#'     models according to \code{rows} and \code{columns} with devices
#'     such as \code{\link[grDevices]{pdf}} or
#'     \code{\link[grDevices]{jpeg}}.  Note that you need to specify
#'     \code{units = "in"} when using \code{\link[grDevices]{jpeg}}.
find_device_dimensions <- function(models, rows = "outcomes", columns = "exposures") {
    layout_info <- find_layout(find_number_of(rows, models), find_number_of(columns, models))
    width_in_cm <- sum(cm_to_double(layout_info$widths))
    height_in_cm <- sum(cm_to_double(layout_info$heights)) + outer_margin_in_cm()
    list(width = cm_to_inches(width_in_cm),
        height = cm_to_inches(height_in_cm))
}

cm_to_double <- function(x) {
    as.double(sub(" cm", "", x))
}

cm_to_inches <- function(x) {
    x / 2.54
}

#' Plot fitted models
#'
#' @param models List of models
#' @param rows One of "outcomes", "exposures", "adjustments"
#' @param columns One of "outcomes", "exposures", "adjustments"
#' @param labels Character vector with named elements (same as
#'     \code{from_to} argument of \code{\link{translate}})
#' @param type Confidence intervals to plot ("beta" or "OR")
#' @details The `rows` and `columns` arguments define which of
#'     outcomes, exposures, or adjustments occupy the rows and columns
#'     of the plot, respectively.  Conceptually there is a third
#'     parameter, `page`, that is automatically set depending on the
#'     values of `rows` and `columns`.  Each of `rows`, `columns`, and
#'     `page` must correspond to one of "outcomes", "exposures", or
#'     "adjustments", and no two may have the same value.  If, for
#'     example, `rows = "outcomes"`, `columns = "exposures"`, and
#'     `page = "adjustments"`, then the function creates as many pages
#'     of plots as there are adjustments.  Every page corresponds to
#'     one adjustment and contains a plot made up of several subplots
#'     where subplots in rows correspond to different outcomes and
#'     subplots in columns correspond to different exposures.
#' @return None.
plot_models <- function(models, rows = "outcomes", columns = "exposures", labels = NULL, type = "beta")
{
    layout_info <- find_layout(find_number_of(rows, models), find_number_of(columns, models))
    set_layout(layout_info)
    xylim <- find_xy_ranges(models, type)
    sorted_models <- sort_models_for_plotting(models, rows, columns)
    for (model_number in seq_along(sorted_models)) {
        position <- find_position_in_layout(model_number, layout_info)
        plot_a_model(sorted_models[[model_number]], rows, columns, xylim, position, labels, type)
    }
}

#' Create plots with confidence intervals of exposure variable
#'
#' @param filename Output filename
#' @param models List of models
#' @param rows One of "outcomes", "exposures", "adjustments"
#' @param columns One of "outcomes", "exposures", "adjustments"
#' @param ppi Number of pixels per inch
#' @param labels Character vector with named elements (same as
#'     \code{from_to} argument of \code{\link{translate}})
#' @param type Confidence intervals to plot ("beta" or "OR")
#' @return None.
#'
#' @export
create_pdf <- function(filename, models, rows = NULL, columns = NULL, labels = NULL, type = "beta") {
    dimensions <- find_device_dimensions(models, rows, columns)
    pdf(filename, dimensions$width, dimensions$height, paper = "special")
    tryCatch(plot_models(models, rows, columns, labels, type), finally = dev.off())
}

create_bitmap <- function(bitmap, filename, models, rows = NULL, columns = NULL, labels = NULL, type = "beta", ppi = 200) {
    filename <- maybe_insert_format_string(filename, models, rows, columns)
    dimensions <- find_device_dimensions(models, rows, columns)
    bitmap(filename, dimensions$width, dimensions$height, units = "in", res = ppi)
    tryCatch(plot_models(models, rows, columns, labels, type), finally = dev.off())
}

maybe_insert_format_string <- function(filename, models, rows, columns) {
    pages <- find_pages_rows_columns(rows, columns)$pages
    if (find_number_of(pages, models) > 1) {
        replacement <- sprintf("\\1_%s_%%d.\\2", pages)
        filename <- sub("(.*)\\.(.*)", replacement, filename)
    }
    filename
}

#' @rdname create_pdf
#' @export
create_jpeg <- function(filename, models, rows = NULL, columns = NULL, labels = NULL, type = "beta", ppi = 200) {
    create_bitmap(jpeg, filename, models, rows, columns, labels, type, ppi)
}

#' @rdname create_pdf
#' @export
create_png <- function(filename, models, rows = NULL, columns = NULL, labels = NULL, type = "beta", ppi = 200) {
    create_bitmap(png, filename, models, rows, columns, labels, type, ppi)
}

#' Find number of outcomes, exposures, or adjustments
#'
#' @param type One of "outcomes", "exposures", or "adjustments"
#' @param models List of models
#' @return Number of variables of type \code{type}.
find_number_of <- function(type, models) {
    length(eval(parse(text = sprintf("find_%s(models)", type))))
}

set_layout <- function(layout_info) {
    layout(layout_info$mat, layout_info$widths, layout_info$heights)
    par(mar = c(0, 0, 0, 0))
    par(omi = c(cm_to_inches(outer_margin_in_cm()), 0, 0, 0))
}

outer_margin_in_cm <- function() {
    1
}

plot_a_model <- function(model, rows, columns, xylim, position, labels, type = "beta") {
    plot_segments(model, xylim$xlim, xylim$ylim, type)
    plot_labels(model, rows, columns, position, labels)
}

plot_segments <- function(model, xlim, ylim, type = "beta") {
    segment <- find_segments_to_plot(model, type)
    plot(1, xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE, type = "n")
    segments(segment$x0, segment$y0, segment$x1, segment$y1)
    pt <- find_point_settings(nrow(segment))
    abline(h = attr(segment, "reference_line"), lty = "dashed")
    points(segment$x0, segment$midpoints, pch = pt$pch, cex = pt$cex, bg = pt$bg)
    box()
}

find_point_settings <- function(number_of_segments) {
    default_cex <- 1.5
    if (number_of_segments == 1L) {     # continuous case
        cex <- default_cex
        pch <- 21L
        bg <- "white"
    } else {              # categorical case (with reference category)
        cex <- c(default_cex, rep_len(default_cex, number_of_segments - 1))
        point_symbols <- c(21, 24, 22, 25, 23)
        point_backgrounds <- rep(c("white", "black"), each = length(point_symbols))
        pch <- c(4, rep_len(point_symbols, number_of_segments - 1))
        bg <- c("white", rep_len(point_backgrounds, number_of_segments - 1))
    }
    list(cex = cex, pch = pch, bg = bg)
}

plot_labels <- function(model, rows, columns, position, labels) {
    plot_labels <- find_plot_labels(model, rows, columns, labels)
    cex <- sqrt(1/2)
    if (position$left)
        mtext(plot_labels$row, side = 2, line = 1, xpd = NA, cex = cex)
    if (position$right)
        axis(4, las = 1)
    if (position$top)
        mtext(plot_labels$column, side = 3, line = 1, xpd = NA, cex = cex)
    if (position$top && position$left)  # only for 1st plot of page
        mtext(plot_labels$page, side = 1, line = 1, outer = TRUE, cex = cex)
}

#' Translate strings
#'
#' @param x Character vector
#' @param from_to Named character vector or named list
#' @details If an element of \code{x} names one of the elements of
#'     \code{from_to}, it is replaced by that element.
#' @return A character vector of the same length as \code{x} with
#'     elements translated according to \code{from_to}.
translate <- function(x, from_to) {
    if (is.null(x) || is.null(from_to))
        return(x)
    y <- unname(from_to[x])
    has_translation <- x %in% names(from_to)
    y[!has_translation] <- x[!has_translation]
    y
}

new_term <- function(variable, data) {
    parts <- NULL
    if (is_interaction_term(variable)) {
        vars <- split_interaction_term(variable)
        levels_ <- expand.grid(
            levels(new_term(vars[1], data)),
            levels(new_term(vars[2], data)),
            stringsAsFactors = FALSE)
        labels <- paste0(vars[1], levels_[[1]], ":", vars[2], levels_[[2]])
        if (is.factor(data[[vars[1]]]) && is.factor(data[[vars[2]]]))
            levels <- paste0(levels_[[1]], ":", levels_[[2]])
        else if (is.factor(data[[vars[1]]]))
            levels <- levels_[[1]]
        else if (is.factor(data[[vars[2]]]))
            levels <- levels_[[2]]
        else
            levels <- ""
    } else if (is_factor_crossing(variable)) {
        vs <- split_factor_crossing(variable)
        parts <- list(
            new_term(vs[1], data),
            new_term(vs[2], data),
            new_term(paste0(vs[1], ":", vs[2]), data)
        )
    } else if (is.factor(data[[variable]])) {
        levels <- levels(data[[variable]])
        labels <- paste0(variable, levels)
    } else {
        levels <- ""
        labels <- variable
    }
    structure(
        list(parts = parts, variable = variable, labels = labels,
            levels = levels
        ), class = "manyregs_term"
    )
}

is_interaction_term <- function(x) {
    grepl(":", x, fixed = TRUE)
}

is_factor_crossing <- function(x) {
    grepl("*", x, fixed = TRUE)
}

split_interaction_term <- function(term) {
    split_term(term, ":")
}

split_factor_crossing <- function(term) {
    split_term(term, "\\s*\\*\\s*")
}

split_term <- function(term, sep) {
    unlist(strsplit(term, sep, perl = TRUE), use.names = FALSE)
}

#' @export
summary.manyregs_term <- function(object, ...) {
    if (is.null(object$parts)) {
        data.frame(stringsAsFactors = FALSE,
            label = object$labels,
            variable = object$variable,
            level = object$levels)
    } else {
        do.call(rbind, lapply(object$parts, summary))
    }
}

levels.manyregs_term <- function(x) {
    summary(x)$level
}

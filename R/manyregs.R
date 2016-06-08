#' Create models from ingredients.
#'
#' @param outcomes Character vector with names of outcome variables
#' @param exposures Character vector with names of exposure variables
#' @param adjustments List of character vectors with names of adjustment
#'     variables
#' @param f Function for fitting a model to a dataset.
#' @return List of models where every model is an object of S3 class
#'     "manyregs_model".
#'
#' @export
create_models <- function(outcomes, exposures, adjustments = NULL, f) {
    models <- NULL
    if (is.null(adjustments))
        adjustments <- list(adjustments)
    fname <- deparse(substitute(f))
    for (o in outcomes) {
        for (e in exposures) {
            for (a in adjustments) {
                models <- c(list(new_model(o, e, a, f, fname)), models)
            }
        }
    }
    rev(models)
}

#' Create a new model object of S3 class "manyregs_model".
#'
#' @param outcome Name of outcome variable
#' @param exposure Name of exposure variable
#' @param adjustment Character vector with names of adjustment
#'     variables
#' @param f Function for fitting the model
#' @param fname Name of fitting function
#' @param template Model to use as template
#' @param extra_slots List of additional slots for model (see Details)
#'
#' @details The "extra_slots" argument is a list of key-value pairs.
#'     It adds flexibility by allowing slots other than "outcome",
#'     "exposure", "adjustment", "f", and "fname" to be included in
#'     the model.
#'
#' @return Model object of S3 class "manyregs_model".
new_model <- function(outcome, exposure, adjustment, f, fname, template = NULL, extra_slots = NULL) {
    model <- if (is.null(template)) list() else template
    args <- match.call()
    for (argname in names(args)) {
        if (argname %in% c("template", "extra_slots"))
            next
        arg <- args[[argname]]
        if (is.language(arg))
            arg <- eval.parent(arg)
        model[[argname]] <- arg
    }
    if (!is.null(extra_slots)) {
        for (slot in names(extra_slots)) {
            model[[slot]] <- extra_slots[[slot]]
        }
    }
    class(model) <- "manyregs_model"
    model
}

#' Coerce a model to type character.
#'
#' @param x Model object to be coerced.
#' @param \dots Ignored.
#' @return Character representation of model.
#'
#' @export
as.character.manyregs_model <- function(x, ...) {
    sprintf("%s  (%s)", model_to_formula_string(x), x$fname)
}

#' Convert model to a formula-like string.
#'
#' @param x Model to be converted
#' @return Formula-like character string representing the model.
model_to_formula_string <- function(x) {
    if (is.null(x$adjustment)) {
        sprintf("%s ~ %s", x$outcome, x$exposure)
    } else {
        adjustment <- paste(x$adjustment, collapse = " + ")
        sprintf("%s ~ %s + %s", x$outcome, x$exposure, adjustment)
    }
}

#' Print model.
#'
#' @param x Model to be printed
#' @param \dots Ignored
#' @return Returns `x' invisibly.
#'
#' @export
print.manyregs_model <- function(x, ...) {
    cat(as.character(x), "\n", sep = "")
    invisible(x)
}

#' Convert model to formula.
#'
#' @param x Model to be converted to formula
#' @param env Environment for formula
#' @param \dots Ignored
#' @return An object of class "formula" representing the model.
#'
#' @export
formula.manyregs_model <- function(x, env = parent.frame(), ...) {
    formula(model_to_formula_string(x), env = env)
}

#' Fit models.
#'
#' @param models List of model objects
#' @param data Dataset to which models should be fitted
#' @param cores Number of cores used to parallelize computation
#' @return List of fitted models.
#'
#' @export
fit_models <- function(models, data, cores = 1L) {
    parallel::mclapply(models, mc.cores = cores, function(m) {
        tryCatch({
            m$fit <- m$f(m, data)
        }, warning = function(c) {
            m$warning <<- conditionMessage(c)
            m["fit"] <<- list(NULL)
            warning(m$warning)
        }, error = function(c) {
            m$error <<- conditionMessage(c)
            m["fit"] <<- list(NULL)
            warning(m$error)
        })
        m
    })
}

#' Remove slots from a model.
#'
#' @param model Model from which to remove slots
#' @param slots Character vector of slot names
#' @return Model from which named slots were removed.
remove_slots <- function(model, slots) {
    reduced_model <- model[setdiff(names(model), slots)]
    class(reduced_model) <- class(model)
    reduced_model
}

#' Summarize a list of models.
#'
#' @param models Models to be summarized
#' @return foobar
#'
#' @export
summarize_models <- function(models) {
    if (all(is_fitted(models))) {
        summarize_fitted_models(models)
    } else {
        summarize_non_fitted_models(models)
    }
}

#' Are models fitted?
#'
#' @param models List of models
#' @return TRUE for fitted models, FALSE for non-fitted models.
is_fitted <- function(models) {
    vapply(models, function(m) {
        "fit" %in% names(m)
    }, logical(1L), USE.NAMES = FALSE)
}

#' Summarize a list of fitted models.
#'
#' @param models List of models
#' @return A single data frame summarizing the fitted models.
summarize_fitted_models <- function(models) {
    do.call(rbind, lapply(models, function(m) {
        x <- find_estimates(m$fit)
        data.frame(
            outcome = m$outcome,
            variable = x$variable,
            nobs = nobs(m$fit),
            beta = x$beta,
            se = x$se,
            lcl = x$lcl,
            ucl = x$ucl,
            pvalue = x$pvalue,
            model = as.character(m),
            stringsAsFactors = FALSE)
    }))
}

find_estimates <- function(fit) {
    x <- coef(summary(fit))
    d <- data.frame(variable = rownames(x),
        beta = x[, 1], se = x[, 2], pvalue = x[, 4],
        stringsAsFactors = FALSE)
    suppressMessages(ci <- confint(fit))
    d$lcl <- ci[, 1]
    d$ucl <- ci[, 2]
    d
}

#' Summarize a list of non-fitted models.
#'
#' @param models List of models
#' @return A data frame containing the printed representation of the
#'     models.
summarize_non_fitted_models <- function(models) {
    data.frame(models = vapply(models, as.character, character(1L)),
        stringsAsFactors = FALSE)
}

#' Tabulate a categorical variable.
#'
#' @param x Categorical variable to tabulate
#' @param label Label to use for categorical variable
#' @param digits Number of decimal digits to use in "\%" column
#' @return A data frame with columns "variable", "category", "N", "n",
#'     "\%", where "variable" is the name of the variable, "category"
#'     is the level of the variable, "N" is the total number of
#'     observations for the variable, "n" is the number of
#'     observations per category of the variables, and "\%" is the
#'     percentage of observations that fall into a given category.
#'
#' @export
freq <- function(x, label = NULL, digits = 2L) {
    if (is.null(label))
        label <- deparse(substitute(x))
    counts <- table(x, useNA = "ifany")
    d <- data.frame(
        variable = label,
        category = names(counts),
        N = length(x),
        n = as.integer(counts),
        stringsAsFactors = FALSE)
    d$`%` <- with(d, round(100*n/N, digits))
    d
}

#' Tabulate categorical variables from data frame
#'
#' @param column_names Character vector with names of categorical variables
#' @param data Data frame containing categorical variables
#' @param digits Number of decimal digits to use for "\%" column
#' @return A data frame with columns "variable", "category", "N", "n",
#'     "\%".  The "variable" column contains the column names of the
#'     categorical variables from `data`.  The "category" column
#'     contains the levels of the variables.  The "N" column contains
#'     the total number of observations per variable.  The "n" column
#'     contains the number of observations per category of each
#'     variable.  The "\%" column contains the percentage of
#'     observations that fall into a given category.
#'
#' @export
freqs <- function(column_names, data, digits = 2L) {
    d <- do.call(rbind, Map(freq, data[column_names], column_names, digits))
    rownames(d) <- NULL
    d
}

#' Summarize distribution of a continuous variable
#'
#' @param x Values of a continuous variable
#' @param label Label to use for continuous variable
#' @param probs Percentages for which to include percentiles
#' @param digits Number of decimal digits to use
#' @return A data frame with columns "variable", "N", "NAs", "mean",
#'     "sd", "iqr", "min", percentiles, "max".  The "variable" column
#'     contains the name of the continuous variable.  The "N" columns
#'     contains the number of observations for the variable (including
#'     missing values).  The "NAs" column contains the number of
#'     missing values.  The "mean", "sd", "iqr", "min", and "max"
#'     columns contain the mean, standard deviation, inter-quartile
#'     range, minimum, and maximum of the variable, respectively.  The
#'     names of the percentile columns between the "min" and the "max"
#'     column all start with "p" followed by the percentage
#'     corresponding to the percentile.  The column containing the 5%
#'     percentile would be called "p05".  The column with the 50%
#'     percentile, or median, would be called "p50".
#'
#' @export
distro <- function(x, label = NULL, probs = NULL, digits = 2L) {
    if (is.null(label))
        label <- deparse(substitute(x))
    if (is.null(probs))
        probs <- c(.01, .05, .1, .25, .5, .75, .9, .95, .99)
    number_of_observations <- length(x)
    number_of_missings <- sum(is.na(x))
    x <- x[!is.na(x)]
    d <- data.frame(
        variable = label,
        N = number_of_observations,
        NAs = number_of_missings,
        mean = mean(x),
        sd = sd(x),
        iqr = diff(quantile(x, c(.25, .75))),
        min = min(x),
        stringsAsFactors = FALSE)
    for (p in probs) {
        column_name <- paste0("p", 100 * p)
        d[[column_name]] <- quantile(x, p)
    }
    d$max <- max(x)
    d[-(1:3)] <- lapply(d[-(1:3)], round, digits = digits)
    rownames(d) <- NULL
    d
}

#' Summarize distribution of a continuous variable
#'
#' @param column_names Character vector with names of continuous variables
#' @param data Data frame containing variables `column_names`
#' @param probs Percentages for which to include percentiles
#' @param digits Number of decimal digits to use
#' @return A data frame with columns "variable", "N", "NAs", "mean",
#'     "sd", "iqr", "min", percentiles, "max".  The "variable" column
#'     contains the `column_names`.  The "N" column contains the total
#'     number of observations per variable.  The "NAs" column contains
#'     the number of missing observations for each variable.  The
#'     "mean", "sd", "iqr", "min", and "max" variables contain the
#'     mean, standard deviation, inter-quartile range, minimum, and
#'     maximum of each variable, respectively.  The names of the
#'     percentile columns between the "min" and the "max" column all
#'     start with "p" followed by the percentage corresponding to the
#'     percentile.  The 5% percentile column would be called "p5",
#'     while the 50% percentile, or median, column column would be
#'     called "p50".
#'
#' @export
distros <- function(column_names, data, probs = NULL, digits = 2L) {
    d <- do.call(rbind, Map(distro, data[column_names], column_names, list(probs), digits))
    rownames(d) <- NULL
    d
}

#' Compare the means of two groups using a t-test
#'
#' @param column_names Column names of variables
#' @param by Name of a grouping variable with exactly two levels
#' @param data Data frame containing variables `column_names` and `by`
#' @param digits Number of decimal digits in `pval` column
#' @return A data frame with columns "variable", "by", "pval",
#'     "pvalue".  The "variable" column contains the name of the
#'     variables whose means were compared between the two levels of
#'     the `by` variable whose name is contained in the "by" column.
#'     The "pval" column contains the p-value resulting from the
#'     comparison using a t-test, rounded to `digits` decimal digits
#'     for better readability.  The "pvalue" column contains the exact
#'     p-value.
#'
#' @export
compare_means <- function(column_names, by, data, digits = 4L) {
    grp <- data[[by]]
    values <- names(table(grp))
    if (length(values) != 2L) {
        stop("by-variable must have exactly two levels: ", by)
    }
    pvalues <- vapply(column_names, function(column) {
        x <- data[[column]]
        t.test(x[!is.na(grp) & grp == values[1]],
            x[!is.na(grp) & grp == values[2]])$p.value
    }, double(1), USE.NAMES = FALSE)
    data.frame(
        variable = column_names,
        by = by,
        pval = round(pvalues, digits),
        pvalue = pvalues,
        stringsAsFactors = FALSE)
}

#' Compare distributions using a Chi-Squared test
#'
#' @param column_names Column names of variables
#' @param by Name of a grouping variable
#' @param data Data frame containing variables `column_names` and `by`
#' @param digits Number of decimal digits in `pval` column
#' @return A data frame with columns "variable", "by", "pval",
#'     "pvalue".  The "variable" column contains the name of the
#'     variables whose distributions are compared between the levels
#'     of the `by` variable whose name is contained in the "by"
#'     column.  The "pval" column contains the p-value resulting from
#'     the comparison using Pearson's Chi-squared test, rounded to
#'     `digits` decimal digits for better readability.  The "pvalue"
#'     column contains the exact p-value.
#'
#' @export
compare_distros <- function(column_names, by, data, digits = 4L) {
    pvalues <- vapply(column_names, function(column_name) {
        chisq.test(data[[column_name]], data[[by]])$p.value
    }, double(1), USE.NAMES = FALSE)
    data.frame(
        variable = column_names,
        by = by,
        pval = round(pvalues, digits),
        pvalue = pvalues,
        stringsAsFactors = FALSE)
}

#' Extract models with selected outcomes, exposures, and adjustments.
#'
#' @param models List of models
#' @param outcomes Character vector with names of outcome variables
#' @param exposures Character vector with names of exposure variables
#' @param adjustments List of character vectors with names of
#'     adjustment variables
#' @param drop Drop matching models if TRUE (default FALSE)
#' @param combine If "and" (default) models must match all of
#'     `outcomes`, `exposures`, and `adjustments`.  If "or" a single
#'     match suffices.
#' @return A list of models whose outcome, exposure, and adjustment
#'     match match `outcomes`, `exposures`, and `adjustments`.  The
#'     exact nature of the match depends on the values of `drop` and
#'     `combine`.
#'
#' @export
filter_models <- function(models, outcomes = NULL, exposures = NULL,
    adjustments = NULL, drop = FALSE, combine = NULL)
{
    if (is.null(combine))
        combine <- "and"
    if (length(combine) != 1L)
        stop("`combine` must be of length 1")
    if (! combine %in% c("and", "or"))
        stop("`combine` must be one of \"and\" or \"or\": \"", combine, "\"")
    combine_fn <- switch(combine, and = `&&`, or = `||`, `&&`)
    Filter(function(model) {
        has_outcome <- if (is.null(outcomes)) TRUE else model$outcome %in% outcomes
        has_exposure <- if (is.null(exposures)) TRUE else model$exposure %in% exposures
        has_adjustment <- if (is.null(adjustments)) TRUE else {
            Position(function(adjustment) {
                identical(adjustment, model$adjustment)
            }, adjustments, nomatch = 0L) != 0L
        }
        does_match <- Reduce(combine_fn, list(has_outcome, has_exposure, has_adjustment))
        if (drop)
            !does_match
        else
            does_match
    }, models)
}

#' Find layout for plots
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @return A list with elements "mat", "widths", "heights" that can be
#'     used as arguments to \code{\link[graphics]{layout}}.
#'
find_layout <- function(nrow, ncol) {
    margin_width <- lcm(.5)
    plot_width <- lcm(3)
    mat <- rbind(rep(0, 2 + ncol),
        cbind(rep(0, nrow), matrix(seq_len(nrow * ncol), ncol = ncol, byrow = TRUE), rep(0, nrow)),
        rep(0, 2 + ncol))
    widths <- c(margin_width, rep_len(plot_width, ncol), margin_width)
    heights <- c(margin_width, rep_len(plot_width, nrow), margin_width)
    list(mat = mat, widths = widths, heights = heights)
}

#' Find the numbers of plots at the margins
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param byrow If FALSE (default) subplots are filled by columns,
#'     otherwise by rows.
#' @return A list with elements "bottom", "left", "top", and "right".
#'     Every element contains the numbers of the plots at the
#'     corresponding margin.
#'
find_margin_plots <- function(nrow, ncol, byrow = FALSE) {
    m <- matrix(seq_len(nrow * ncol), nrow = nrow, byrow = byrow)
    list(bottom = m[nrow,], left = m[,1], top = m[1,], right = m[,ncol])
}

#' Find outcomes, exposures, adjustments from a list of models
#'
#' @param models List of model objects
#' @param outcomes Character vector of names of outcome variables
#' @param exposures Character vector of names of exposures variables
#' @param adjustments List with character vectors of names of
#'     adjustments variables
#' @return A list with elements "outcomes", "exposures", and
#'     "adjustments".  If the argument of the same name as the element
#'     is non-NULL, the value of the element will be the same as that
#'     of the argument.  Otherwise the list element contains the
#'     unique values of its "type" that are found in the `models`,
#'     e.g., the element named "outcomes" will contain a character
#'     vector of the unique outcomes that are found in the list of
#'     models.
#'
find_variables <- function(models, outcomes = NULL, exposures = NULL, adjustments = NULL) {
    unique_list <- function(x) {
        x[!duplicated(x)]
    }
    if (is.null(outcomes))
        outcomes <- unique(vapply(models, `[[`, character(1), "outcome"))
    if (is.null(exposures))
        exposures <- unique(vapply(models, `[[`, character(1), "exposure"))
    if (is.null(adjustments))
        adjustments <- unique_list(lapply(models, `[[`, "adjustment"))
    list(outcomes = outcomes, exposures = exposures, adjustments = adjustments)
}

#' Match values of variables to different parts of a layout
#'
#' @param variables List with the same structure as the return value
#'     of \code{\link{find_variables}}.
#' @param rows One of "outcomes", "exposures", "adjustments" but not
#'     the same as `columns`
#' @param columns One of "outcomes", "exposures", "adjustments" but
#'     not the same as `rows`
#' @return The layout consists of pages, rows, and columns.  Every
#'     part of the layout represents one type of variable: outcomes,
#'     exposures, adjustments.  The function takes the values of the
#'     different types of variables (`variables`) as well as the
#'     correspondence between parts of the layout and the types of
#'     variables (`rows` and `columns`) and returns a list with
#'     elements "page", "row", "column" where every element contains
#'     the values of the type of variable that matches the
#'     corresponding part of the layout.
#'
find_page_row_column_values <- function(variables, rows, columns) {
    pages <- setdiff(names(variables), c(rows, columns))
    list(page = variables[[pages]], row = variables[[rows]], column = variables[[columns]])
}

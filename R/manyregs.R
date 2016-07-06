#' Create models from ingredients.
#'
#' @param outcomes Character vector with names of outcome variables
#' @param exposures Character vector with names of exposure variables
#' @param adjustments List of character vectors with names of adjustment
#'     variables
#' @param f Function for fitting a model to a dataset.
#' @param by Character vector defining how ties are broken
#' @return List of models where every model is an object of S3 class
#'     "manyregs_model".
#'
#' @export
create_models <- function(outcomes, exposures, adjustments = NULL, f, by = NULL) {
    fname <- deparse(substitute(f))
    if (is.null(adjustments) || !is.list(adjustments))
        adjustments <- list(adjustments)
    if (is.null(by))
        by <- c("outcomes", "exposures", "adjustments")
    x <- find_combinations(outcomes, exposures, adjustments, by)
    Map(function(outcome, exposure, adjustment) {
        new_model(outcome, exposure, adjustment, f, fname)
    }, x$outcomes, x$exposures, x$adjustments)
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
    covariates <- paste(c(x$exposure, x$adjustment), collapse = covariate_separator)
    sprintf("%s ~ %s", x$outcome, covariates)
}

#' String separating names of covariates in formula string.
covariate_separator <- " + "

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
#'
#' @return The result depends on whether all models are fitted or not.
#'     If at least one of the models is not fitted, a data frame with
#'     a single column "model" will be returned that contains a
#'     character representations of every model in \code{models}.  If
#'     all models are fitted, a data frame with the following columns
#'     is returned:
#'
#' \itemize{
#'   \item outcome: Name of outcome variable
#'   \item variable: Name of covariate
#'   \item nobs: Number of observations
#'   \item beta: Effect estimate
#'   \item se: Standard error of effect estimate
#'   \item lcl: Lower 95\% confidence limit for effect estimate
#'   \item ucl: Upper 95\% confidence limit for effect estimate
#'   \item pvalue: P-Value
#'   \item model: Character representation of statistical model
#' }
#'
#'     Note that confidence limits are based on the assumption of
#'     asymptotic normality of the effect estimates.  This assumption
#'     might be violated in small samples.
#'
#' @export
summarize_models <- function(models) {
    do.call(rbind, lapply(models, summary))
}

#' Summarize model.
#'
#' @param object Model to be summarized
#' @param \dots Ignored
#' @return A data frame summarizing the model.
#'
#' @export
summary.manyregs_model <- function(object, ...) {
    if ("fit" %in% names(object))
        summarize_fitted_model(object)
    else
        summarize_non_fitted_model(object)
}

summarize_fitted_model <- function(model) {
    x <- find_estimates(model$fit)
    data.frame(
        outcome = model$outcome,
        variable = x$variable,
        nobs = nobs(model$fit),
        beta = x$beta,
        se = x$se,
        lcl = x$lcl,
        ucl = x$ucl,
        pvalue = x$pvalue,
        model = as.character(model),
        stringsAsFactors = FALSE)
}

summarize_non_fitted_model <- function(model) {
    data.frame(model = as.character(model),
        stringsAsFactors = FALSE)
}


#' Extract estimates from fitted model
#'
#' @param fit Fitted model
#' @return A data frame with columns "beta", "se", "pvalue", "lcl",
#'     and "ucl" denoting the effect estimate, its standard error, the
#'     p-value, and the lower and upper 95\% confidence limits.
#'
#'     Note that the confidence limits are based on the assumption of
#'     asymptotic normality of the effect estimates.  This assumption
#'     might be violated in small samples.
find_estimates <- function(fit) {
    beta <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    ci <- confint.default(fit)
    pvalue <- find_pvalue(coef(summary(fit)))
    variable <- names(beta)
    data.frame(variable, beta, se, pvalue, lcl = ci[,1], ucl = ci[,2],
        stringsAsFactors = FALSE)
}

find_pvalue <- function(x) {
    pvalue_column <- grep("^Pr\\(.*\\)$", colnames(x))
    if (length(pvalue_column) == 1)
        x[, pvalue_column]
    else
        rep_len(NA, nrow(x))
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
#' @param combine If "and" models must match all of `outcomes`,
#'     `exposures`, and `adjustments`.  If "or" (default) a single
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
        combine <- "or"
    if (length(combine) != 1L)
        stop("`combine` must be of length 1")
    if (! combine %in% c("and", "or"))
        stop("`combine` must be one of \"and\" or \"or\": \"", combine, "\"")
    combine_fn <- switch(combine, and = `&&`, or = `||`)
    selected_variables <- find_selected_variables(models, outcomes, exposures, adjustments)
    known_variables <- find_variables(models)
    if (contains_unknown_variable(selected_variables, known_variables))
        return(list())
    Filter(function(model) {
        has_outcome <- model$outcome %in% selected_variables$outcomes
        has_exposure <- model$exposure %in% selected_variables$exposures
        has_adjustment <- Position(function(adjustment) {
            identical(adjustment, model$adjustment)
        }, selected_variables$adjustments, nomatch = 0L) != 0L
        does_match <- Reduce(combine_fn, list(has_outcome, has_exposure, has_adjustment))
        if (drop)
            !does_match
        else
            does_match
    }, models)
}

find_selected_variables <- function(models, outcomes, exposures, adjustments) {
    if (is.null(outcomes))
        outcomes <- find_outcomes(models)
    if (is.null(exposures))
        exposures <- find_exposures(models)
    if (is.null(adjustments))
        adjustments <- find_adjustments(models)
    if (!is.list(adjustments))
        adjustments <- list(adjustments)
    list(outcomes = outcomes, exposures = exposures, adjustments = adjustments)
}

contains_unknown_variable <- function(selected_variables, known_variables) {
    set_of_selected_variables <- unique(unlist(selected_variables, use.names = FALSE))
    set_of_known_variables <- unique(unlist(known_variables, use.names = FALSE))
    ! all(set_of_selected_variables %in% set_of_known_variables)
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

#' Find all combination of outcomes, exposures, and adjustments
#'
#' @param outcomes Character vector of outcome variable names
#' @param exposures Character vector of exposures variable names
#' @param adjustments List of character vectors with adjustment
#'     variable names
#' @param by Character vector defining how ties are broken
#' @return List with sublists "outcomes", "exposures", "adjustments"
#'     of equal length.  Each triplet of corresponding elements of the
#'     three sublists forms a possible outcome-exposure-adjustment
#'     combination.
#'
find_combinations <- function(outcomes = NULL, exposures = NULL, adjustments = NULL, by = NULL) {
    if (is.null(by))
        by <- c("outcomes", "exposures", "adjustments")
    if (is.null(Find(Negate(is.null), list(outcomes, exposures, adjustments))))
        return(list(outcomes = list(), exposures = list(), adjustments = list()))
    if (is.null(outcomes))
        outcomes <- list(NULL)
    if (is.null(exposures))
        exposures <- list(NULL)
    find_combinations_helper(outcomes, exposures, adjustments, by)
}

find_combinations_helper <- function(outcomes, exposures, adjustments, by) {
    by2 <- sub("^(adjustments)$", "adjustment_to_string(\\1)", by)
    combinations <- lapply(setNames(eval(parse(text = sprintf(
        "expand.grid(%s, stringsAsFactors = FALSE)",
        paste(rev(by2), collapse = ", ")))), rev(by)), as.list)
    combinations$adjustments <- adjustment_from_string(
        unlist(combinations$adjustments, use.names = FALSE))
    combinations[c("outcomes", "exposures", "adjustments")]
}

#' Convert a list of adjustment variables to a character vector
#'
#' @param adjustments List of character vectors with names of
#'     adjustment variables
#' @return A character vector representing the list of adjustments.
#'
adjustment_to_string <- function(adjustments) {
    if (is.null(adjustments))
        adjustments <- list(NULL)
    vapply(adjustments, function(xs) {
        if (is.null(xs))
            "NULL"
        else
            paste(xs, collapse = adjustment_separator)
    }, character(1))
}

#' String used in character representation of a list of adjustments
adjustment_separator <- covariate_separator

#' Convert character representation of a list of adjustments to list
#'
#' @param adjustment_string A character vector representing the list
#'     of adjustments
#' @return A list of character vectors with names of adjustment
#'     variables
adjustment_from_string <- function(adjustment_string) {
    xs <- strsplit(adjustment_string, adjustment_separator, fixed = TRUE)
    lapply(xs, function(x) {
        if (identical(x, "NULL"))
            NULL
        else
            x
    })
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

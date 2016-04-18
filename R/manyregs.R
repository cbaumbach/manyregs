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
#' @return Character representation of model.
#'
#' @export
as.character.manyregs_model <- function(x) {
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
#' @return Returns `x' invisibly.
#'
#' @export
print.manyregs_model <- function(x) {
    cat(as.character(x), "\n", sep = "")
    invisible(x)
}

#' Convert model to formula.
#'
#' @param object Model to be converted to formula
#' @param env Environment for formula
#' @return An object of class "formula" representing the model.
#'
#' @export
formula.manyregs_model <- function(x, env = parent.frame()) {
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
        new_model(template = m, extra_slots = list(fit = m$f(m, data)))
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

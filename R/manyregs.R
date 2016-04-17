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
#' @return Model object of S3 class "manyregs_model".
new_model <- function(outcome, exposure, adjustment, f, fname) {
    model <- list(outcome = outcome,
        exposure = exposure,
        adjustment = adjustment,
        f = f, fname = fname)
    class(model) <- "manyregs_model"
    model
}

#' Coerce a model to type character.
#'
#' @param x Model object to be coerced.
#' @export
as.character.manyregs_model <- function(x) {
    if (is.null(x$adjustment)) {
        sprintf("%s ~ %s  (%s)", x$outcome, x$exposure, x$fname)
    } else {
        adjustment <- paste(x$adjustment, collapse = " + ")
        sprintf("%s ~ %s + %s  (%s)", x$outcome, x$exposure, adjustment, x$fname)
    }
}

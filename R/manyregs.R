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
create_models <- function(outcomes, exposures, adjustments, f) {
    m <- new_model(outcomes[1L], exposures[1L], adjustments[[1L]], f, deparse(substitute(f)))
    list(m)
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
    list(outcome = outcome,
        exposure = exposure,
        adjustment = adjustment,
        f = f, fname = fname)
}

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
    m <- list(
        outcome = outcomes[1L],
        exposure = exposures[1L],
        adjustment = adjustments[[1L]],
        f = f, fname = deparse(substitute(f)))
    list(m)
}


test_data <- local({
    set.seed(12345)
    N <- 100L
    data.frame(
        y1 = rnorm(N),
        y2 = rnorm(N),
        x1 = factor(as.integer(cut(rnorm(N), 2)) - 1L),
        x2 = factor(as.integer(cut(rnorm(N), 3))),
        x3 = rnorm(N),
        z1 = factor(as.integer(cut(rnorm(N), 3))),
        z2 = rnorm(N))
})

linear <- function(model, data) {
    with(data, lm(as.formula(model)))
}
outcomes <- c("y1", "y2")
exposures <- c("x1", "x2", "x3")
adjustments <- list(NULL, "z1", c("z1", "z2"))

models <- create_models(outcomes, exposures, adjustments, linear)
fitted_models <- fit_models(models, test_data)
summarized_models <- summarize_models(fitted_models)

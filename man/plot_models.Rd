% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_models}
\alias{plot_models}
\title{Plot fitted models}
\usage{
plot_models(models, rows = "outcomes", columns = "exposures",
  labels = NULL, type = "beta")
}
\arguments{
\item{models}{List of models}

\item{rows}{One of "outcomes", "exposures", "adjustments"}

\item{columns}{One of "outcomes", "exposures", "adjustments"}

\item{labels}{Character vector with named elements (same as
\code{from_to} argument of \code{\link{translate}})}

\item{type}{Confidence intervals to plot ("beta" or "OR")}
}
\value{
None.
}
\description{
Plot fitted models
}
\details{
The `rows` and `columns` arguments define which of
    outcomes, exposures, or adjustments occupy the rows and columns
    of the plot, respectively.  Conceptually there is a third
    parameter, `page`, that is automatically set depending on the
    values of `rows` and `columns`.  Each of `rows`, `columns`, and
    `page` must correspond to one of "outcomes", "exposures", or
    "adjustments", and no two may have the same value.  If, for
    example, `rows = "outcomes"`, `columns = "exposures"`, and
    `page = "adjustments"`, then the function creates as many pages
    of plots as there are adjustments.  Every page corresponds to
    one adjustment and contains a plot made up of several subplots
    where subplots in rows correspond to different outcomes and
    subplots in columns correspond to different exposures.
}


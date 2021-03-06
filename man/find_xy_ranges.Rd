% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{find_xy_ranges}
\alias{find_xy_ranges}
\title{Find x and y ranges for confidence intervals for a list of models}
\usage{
find_xy_ranges(models, type = "beta")
}
\arguments{
\item{models}{A list of models}

\item{type}{Confidence intervals to plot ("beta" or "OR")}
}
\value{
A list with elements "xlim" and "ylim" containing the x and
    y ranges needed for plotting confidence intervals for the
    exposures of a list of models.
}
\description{
Find x and y ranges for confidence intervals for a list of models
}


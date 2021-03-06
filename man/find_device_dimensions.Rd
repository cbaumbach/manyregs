% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{find_device_dimensions}
\alias{find_device_dimensions}
\title{Find dimensions of device region for plotting models}
\usage{
find_device_dimensions(models, rows = "outcomes", columns = "exposures")
}
\arguments{
\item{models}{A list of models}

\item{rows}{One of "outcomes", "exposures", or "adjustments"}

\item{columns}{One of "outcomes", "exposures", or "adjustments"}
}
\value{
A list with elements "width" and "height" containing the
    recommended width and height (in inches) for plotting the
    models according to \code{rows} and \code{columns} with devices
    such as \code{\link[grDevices]{pdf}} or
    \code{\link[grDevices]{jpeg}}.  Note that you need to specify
    \code{units = "in"} when using \code{\link[grDevices]{jpeg}}.
}
\description{
Find dimensions of device region for plotting models
}


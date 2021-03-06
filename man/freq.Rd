% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manyregs.R
\name{freq}
\alias{freq}
\title{Tabulate a categorical variable.}
\usage{
freq(x, label = NULL, digits = 2L)
}
\arguments{
\item{x}{Categorical variable to tabulate}

\item{label}{Label to use for categorical variable}

\item{digits}{Number of decimal digits to use in "\%" column}
}
\value{
A data frame with columns "variable", "category", "N", "n",
    "\%", where "variable" is the name of the variable, "category"
    is the level of the variable, "N" is the total number of
    observations for the variable, "n" is the number of
    observations per category of the variables, and "\%" is the
    percentage of observations that fall into a given category.
}
\description{
Tabulate a categorical variable.
}


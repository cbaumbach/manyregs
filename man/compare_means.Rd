% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manyregs.R
\name{compare_means}
\alias{compare_means}
\title{Compare the means of two groups using a t-test}
\usage{
compare_means(column_names, by, data, digits = 4L)
}
\arguments{
\item{column_names}{Column names of variables}

\item{by}{Name of a grouping variable with exactly two levels}

\item{data}{Data frame containing variables `column_names` and `by`}

\item{digits}{Number of decimal digits in `pval` column}
}
\value{
A data frame with columns "variable", "by", "pval",
    "pvalue".  The "variable" column contains the name of the
    variables whose means were compared between the two levels of
    the `by` variable whose name is contained in the "by" column.
    The "pval" column contains the p-value resulting from the
    comparison using a t-test, rounded to `digits` decimal digits
    for better readability.  The "pvalue" column contains the exact
    p-value.
}
\description{
Compare the means of two groups using a t-test
}


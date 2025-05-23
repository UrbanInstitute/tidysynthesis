% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ks_distance.R
\name{ks_distance}
\alias{ks_distance}
\alias{ks_distance.data.frame}
\alias{ks_distance_vec}
\title{Kolmogorov-Smirnov distance}
\usage{
ks_distance(data, ...)

\method{ks_distance}{data.frame}(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

ks_distance_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
}
\arguments{
\item{data}{A \code{data.frame} containing the columns specified by the truth and
estimate arguments.}

\item{...}{Not currently used.}

\item{truth}{The column identifier for the true results (that is numeric).
This should be an unquoted column name although this argument is passed by
expression and supports quasiquotation (you can unquote column names). For
\verb{⁠_vec(})⁠ functions, a numeric vector.}

\item{estimate}{The column identifier for the predicted results (that is
also numeric). As with truth this can be specified different ways but the
primary method is to use an unquoted variable name. For \verb{⁠_vec(})⁠ functions,
a numeric vector.}

\item{na_rm}{A \code{logical} value indicating whether \code{NA} values should be
stripped before the computation proceeds.}

\item{case_weights}{This is a placeholder for now and will be added when
case_weights are added to tidysynthesis.}
}
\value{
For \code{ks_distance_vec()}, a single numeric value (or \code{NA}).
}
\description{
Kolmogorov-Smirnov distance
}

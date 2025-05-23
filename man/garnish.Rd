% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/garnish.R
\name{garnish}
\alias{garnish}
\title{Invert outcome variable transformations after prediction}
\usage{
garnish(object, predictions)
}
\arguments{
\item{object}{A fit model object}

\item{predictions}{A data frame of predictions with a variable .pred}
}
\value{
A vector of transformed predictions
}
\description{
Invert outcome variable transformations after prediction
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synth_component_utils.R
\name{.validate_custom_components}
\alias{.validate_custom_components}
\title{Validate custom component specification in \code{synth_spec}}
\usage{
.validate_custom_components(custom_components, component_name)
}
\arguments{
\item{custom_components}{A named list of lists}

\item{component_name}{String, one of the names in \code{names(.name_to_inspect)}}
}
\description{
Raises error if custom component improperly specified for \verb{custom_*} argument.
}

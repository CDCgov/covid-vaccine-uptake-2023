% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_projections_for_plot.R
\name{prepare_projections_for_plot}
\alias{prepare_projections_for_plot}
\title{Prepares tibble of uptake projection outputs for plotting}
\usage{
prepare_projections_for_plot(
  obs_slopes,
  ref_data,
  estimate,
  dates,
  method = c("rate_matched", "date_matched"),
  trajectory = c("estimate", "lci", "uci")
)
}
\arguments{
\item{obs_slopes}{df containing initial slope generated from fit_slope.R,
with one column of \code{trajectories} containing \code{estimate}, \code{lci},  \code{uci}, and
one column of \code{values}}

\item{ref_data}{tibble of reference year data}

\item{estimate}{vector of the current year's observations}

\item{dates}{lubridate::Date object specifying dates of the observations in estimate}

\item{method}{character specifying vectors of projection methods to use}

\item{trajectory}{character used for accessing obs_slope values}
}
\value{
A tibble for plotting
}
\description{
Prepares tibble of uptake projection outputs for plotting
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_core.R
\name{fuzzy_join_core}
\alias{fuzzy_join_core}
\title{Perform a Fuzzy-Join With an Arbitrary Distance Metric}
\usage{
fuzzy_join_core(
  a,
  b,
  by,
  join_func,
  mode,
  block_by = NULL,
  similarity_column = NULL,
  ...
)
}
\arguments{
\item{a, b}{The two dataframes to join.}

\item{by}{A named vector indicating which columns to join on. Format should
be the same as dplyr: \code{by = c("column_name_in_df_a" = "column_name_in_df_b")}, but two columns must be specified in each dataset
(x column and y column). Specification made with \code{dplyr::join_by()} are
also accepted.}

\item{join_func}{the joining function responsible for performing the join.}

\item{mode}{the dplyr-style type of join you want to perform}

\item{block_by}{A named vector indicating which columns to 'block' (perform exact joining) on. Format should
be the same as dplyr: \code{by = c("column_name_in_df_a" = "column_name_in_df_b")}, but two columns must be specified in each dataset
(x column and y column). Specification made with \code{dplyr::join_by()} are
also accepted.}

\item{similarity_column}{An optional character vector. If provided, the data
frame will contain a column with this name giving the similarity
between the two fields. Extra column will not be present if anti-joining.}

\item{...}{Other parameters to be passed to the joining function}
}
\description{
Code used by zoomerjoin to perform dplyr-style joins. Users wishing to write
their own joining functions can extend zoomerjoin's functionality by writing
joining functions to use with \code{fuzzy_join_core}.
}

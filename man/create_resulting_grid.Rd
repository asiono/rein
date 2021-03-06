% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_resulting_grid.R
\name{create_resulting_grid}
\alias{create_resulting_grid}
\title{create_resulting_grid}
\usage{
create_resulting_grid(grid, solution_space_combined, verbose = 0)
}
\arguments{
\item{grid}{List, data frame containing information of grid to be optimized.}

\item{solution_space_combined}{Selected solution based on optimization result}

\item{verbose}{Value greater than zero to display step by step of reinforcement}
}
\value{
rebuilt grid but not yet calculated
}
\description{
This function create resulting grid from optimization result.
}

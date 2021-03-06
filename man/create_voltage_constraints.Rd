% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_voltage_constraints.R
\name{create_voltage_constraints}
\alias{create_voltage_constraints}
\title{create_voltage_constraints}
\usage{
create_voltage_constraints(solution_space, big_M, allowed_voltage, iteration,
  oltc.trigger, verbose = 0)
}
\arguments{
\item{solution_space}{dataframe containing possible cable and transformer types in the grid and its specifications}

\item{big_M}{big M value for voltage limit optimization}

\item{allowed_voltage}{allowed voltage deviatipn limit}

\item{iteration}{counter for reinforcement iteration.}

\item{oltc.trigger}{indication for OLTC transformator usage.}

\item{verbose}{Value greater than zero to display step by step of reinforcement.}
}
\value{
This function creates the side conditions and objective function for the optimization problem. 
The ouput is a list. That contains A, b and c.
}
\description{
creates the constraints given by the voltage
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial.R
\name{bin_variable}
\alias{bin_variable}
\title{Bin Variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{number of trials (numeric)}

\item{prob}{probability of success (numeric)}
}
\value{
list of class binvar that contains the trials and probability values
}
\description{
takes in trials and probability values and returns a list with both of the values
}
\examples{
bin_variable(trials = 10, p = 0.3)
}

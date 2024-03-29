% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_sample_n.R
\name{cal_sample_n}
\alias{cal_sample_n}
\title{Calculate Sample Number}
\usage{
cal_sample_n(power, alpha, sd, mde)
}
\arguments{
\item{power}{The probability of rejecting a false null hypothesis (domain: 0 ~ 1)}

\item{alpha}{The probability of a type I error (domain: 0 ~ 1)}

\item{sd}{The standard deviation of the outcome (domain: positive)}

\item{mde}{The difference between the mean of the control and treatment groups}
}
\value{
The number of necessary samples calculated based on the arguments
}
\description{
Calculate the sample number based on power, alpha, standard deviation(SD) and Minimum Detectable Effect(MDE)
}
\examples{
n <- cal_sample_n(0.8, 0.05, 500, 100)
}

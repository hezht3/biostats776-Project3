% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Exp.R
\name{Exp}
\alias{Exp}
\title{Exponential transformation}
\usage{
Exp(x, k)
}
\arguments{
\item{x}{numeric, a single number to be exponentiated.}

\item{k}{integer, the number of terms to be used in the series expansion beyond the constant 1. The value
of \code{k} is always $\geq 1$.}
}
\value{
exponential of the number x.
}
\description{
A function that computes the exponential of a number using the truncated series expansion.
}
\details{
This function computes the exponential to the number to be exponentiated and the number of terms
to be used in the series expansion beyond the constant 1, passed as arguments to the function.
}
\examples{
Exp(2, 5)
## [1] 7.266667

}

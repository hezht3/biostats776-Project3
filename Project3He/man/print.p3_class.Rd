% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.p3_class.R
\name{print.p3_class}
\alias{print.p3_class}
\title{Print \code{p3_class}}
\usage{
\method{print}{p3_class}(x)
}
\arguments{
\item{x}{p3_class, a \code{p3_class} pbject.}
}
\value{
a \code{p3_class} object.
}
\description{
Print number of observations of a \code{p3_class} (Project 3 class).
}
\details{
This function prints number of observations of a \code{p3_class} object.
}
\examples{
set.seed(1234)
x <- rnorm(100)
p3 <- make_p3_class(x)
print(p3)         # explicitly using the print() method
## a p3_class with 100 observations
p3                # using autoprinting
## a p3_class with 100 observations

}

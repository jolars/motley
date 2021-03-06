% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sample_disk}
\alias{sample_disk}
\title{Spread points across a disk using Vogel's method}
\usage{
sample_disk(x = 0, y = 0, r = 1, n = 250)
}
\arguments{
\item{x}{X coordinate for the center of the disk.}

\item{y}{Y coordinate for the center of the disk.}

\item{r}{Radius.}

\item{n}{Number of points to generate.}
}
\value{
A matrix of \code{x} and \code{y} coordinates.
}
\description{
Spreads points evenly across a disk of any radius using the golden
angle. Defaults to sampling 250 points on the
\href{https://en.wikipedia.org/wiki/Unit_disk}{unit disk}.
}
\details{
The function is vectorized.
}
\examples{
pp <- sample_disk(x = 10, y = -5, r = 5, n = 800)
head(pp)
plot(pp, asp = 1, pch = 19)
}


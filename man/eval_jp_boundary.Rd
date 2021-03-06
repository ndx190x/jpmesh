% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{eval_jp_boundary}
\alias{eval_jp_boundary}
\title{Check include mesh areas}
\usage{
eval_jp_boundary(longitude = NULL, latitude = NULL, ...)
}
\arguments{
\item{longitude}{longitude that approximately to .120.0 to 154.0 (\code{double})}

\item{latitude}{latitude that approximately to 20.0 to 46.0 (\code{double})}

\item{...}{other parameters}
}
\description{
It roughly judges whether the given coordinates are within
the mesh area.
}
\examples{
eval_jp_boundary(139.71471056, 35.70128943)
}

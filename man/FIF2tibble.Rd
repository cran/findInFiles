% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findInFiles.R
\name{FIF2tibble}
\alias{FIF2tibble}
\title{Output of `findInFiles` as a tibble}
\usage{
FIF2tibble(fif)
}
\arguments{
\item{fif}{the output of \code{\link{findInFiles}} used with the
option \code{output = "viewer+tibble"}}
}
\value{
The results of \code{\link{findInFiles}} in a tibble.
}
\description{
Returns the results of \code{\link{findInFiles}} in a
  tibble, when the option \code{output = "viewer+tibble"} is used.
}
\examples{
folder <- system.file("example", package = "findInFiles")
fif <- findInFiles("R", "function", root = folder, output = "viewer+tibble")
FIF2tibble(fif)
fif
}

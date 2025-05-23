% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorial_plan.R
\name{fp_augment_axial}
\alias{fp_augment_axial}
\title{Augment to a central composite design}
\usage{
fp_augment_axial(dm, rep = 1)
}
\arguments{
\item{dm}{A factorial plan table, with central points.}

\item{rep}{The number of replications.}
}
\value{
A central composite design (a \code{factorial.plan} object).
}
\description{
Adds the axial points to a $2^n$ centered factorial plan.
}
\examples{
fp_design_matrix(3) \%>\%
  fp_augment_center(rep=4) \%>\%
  fp_augment_axial()
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracker.R
\name{package_assignment_email}
\alias{package_assignment_email}
\title{Generate the package assignments email given code to run}
\usage{
package_assignment_email(pkgs = unassigned_packages(...),
  code = assign_new_packages(pkgs, ...), date = Sys.Date(), ...)
}
\arguments{
\item{code}{code to run, output of \code{\link{assign_new_packages}()}}

\item{date}{date to title the email}

\item{additional}{arguments passed to \code{\link{assign_new_packages}()}}
}
\description{
Generate the package assignments email given code to run
}


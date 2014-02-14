\name{clean}
\alias{clean}
\alias{makeBins}
\alias{emailExistingUser}
\alias{emailNewUser}
\alias{emailNewSvnAccount}

\title{Convenience functions for managing behind the scenes operations
  involved in adding packages to Bioconductor Repositories.}

\description{
  There are several convenience functions in this package that are
  useful for different things. I will enumerate them below
}

\usage{
  clean(tarball, svnDir="~/proj/Rpacks/", copyToSvnDir=TRUE,
        svnAccountExists=FALSE)
  makeBins(tarball)
  emailExistingUser(tarball)
  emailNewUser(tarball)
  emailNewSvnAccount(tarball, sendMail=FALSE)
}

\arguments{
  \item{tarball}{}
  \item{svnDir}{}
  \item{copyToSvnDir}{}
  \item{svnAccountExists}{}
  \item{sendMail}{}
}

% \details{
%   \item{clean}{used for removing cruft that is added to tarball by R CMD
%   build.  This is run before adding a new tarball to the svn repository.}

%   \item{makeBins}{used }
%   \item{emailExistingUser}{used }
%   \item{emailNewUser}{used }
%   \item{emailNewSvnAccount}{used }
% }

\value{
   This varies with the function.  I will list them here:
}
  

\author{Marc Carlson}

\examples{
  library(BiocContributions)
  tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz",
                         package="BiocContributions")
  clean(tarball, copyToSvnDir=FALSE)



}

\keyword{manip}


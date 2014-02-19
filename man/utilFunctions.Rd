\name{clean}
\alias{clean}
\alias{makeBins}
\alias{emailExistingUser}
\alias{emailNewUser}
\alias{emailNewSvnAccount}
\alias{existingSvnUsers}
\alias{getPackageTotals}
\alias{plotPackageTotals}
\alias{installDeps}

\title{Convenience functions for managing behind the scenes operations
  involved in adding packages to Bioconductor Repositories.}

\description{
  There are several convenience functions in this package that are
  useful for different things. I will enumerate them below

  clean: used for removing cruft that is added to tarball by R CMD
  build.  This is normally run before adding a new tarball to the svn
  repository.

  makeBins: used for making tarballs that don't contain source code into
  mac and windows binaries.

  emailExistingUser: used to send an instructional email to a user who
  already has an existing svn account.

  emailNewUser: used to print out a customized instructional email for a
  user who has a new svn account.  Since there is currently no way to
  get the credentials, these will have to be pasted in, but at least the
  rest of the message will be put together.

  emailNewSvnAccount: used to send an email to Carl (with a custom guess
  for the proposed username) so that he can make a new svn account.  If
  a user like this already exists, then you can refuse to send the email
  and it will instead print a message out so that you can just change
  one character manually and then send it.  Again, this could be
  improved if we had access to a local copy of the the svn credentials
  file.

  existingSvnUsers: used to list all existing svn accounts that look
  like the user you would expect to generate based on a taball.  It
  searches the maintainer field of a tarball, generates a user name and
  then looks at all the svn users to see if any are similar to that.

  getPackageTotals: looks at the manifests and gives package counts for
  each. This may need to be updated to reflect new manifests...

  plotPackageTotals: makes a simple plot of package growth over time.

  installDeps: Given a tarball, this will install all its dependencies
  using biocLite().  This works even for tarballs that are not yet
  available themselves via biocLite().
}

\usage{
  clean(tarball, svnDir="~/proj/Rpacks/", copyToSvnDir=TRUE,
        svnAccountExists=FALSE)
  makeBins(tarball)
  emailExistingUser(tarball)
  emailNewUser(tarball)
  emailNewSvnAccount(tarball, sendMail=TRUE)
  existingSvnUsers(tarball)
}

\arguments{
  \item{tarball}{The tarball (as build by R CMD build}
  \item{svnDir}{The dir where the svn repos is located}
  \item{copyToSvnDir}{TRUE/FALSE do we copy the cleaned dir to svnDir?}
  \item{svnAccountExists}{TRUE/FALSE does the maintainer of this tarball
  have an svn account alread?}
  \item{sendMail}{TRUE/FALSE do we call sendmail or do we print out a
  file to send later?}
}

\details{
In order to use the email functions you should put a line in your .Rprofile
file that looks like this:

options( fromEmail = 'emailAddress@someplace.com')
}

\value{
   This varies with the function.  
}
  

\author{Marc Carlson}

\examples{
  library(BiocContributions)
  tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz",
                         package="BiocContributions")

  ## interactive() here is to protect me from example spam etc.
  if(interactive()){
    ## clean up a tarball and copy it to the repos
    clean(tarball)

    ## email someone who has an account
    emailExistingUser(tarball)

    ## generate an email for someone who needs svn credentials
    emailNewUser(tarball)

    ## send an email to Carl asking for a new account.
    emailNewSvnAccount(tarball)


    ## make mac and windows binaries for a package.
    tarball <- system.file("testpackages", "hgu95av2.db_2.10.1.tar.gz",
                           package="BiocContributions");
    makeBins(tarball)

    ## list existing svn users that are similar to this tarballs maintainers
    existingSvnUsers(tarball)

    ## list how many packages are in each known release of Bioconductor
    getPackageTotals()

    ## Install all the dependencies for a tarball you need to review
    ## (but that isn't yet in biocLite itself)
    installDeps(tarball)
  }

}

\keyword{manip}


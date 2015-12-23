\name{clean}
\alias{clean}
\alias{cleanDataPkg}
\alias{makeBins}
\alias{emailExistingUser}
\alias{emailNewUser}
\alias{requestNewSvnAccountFromScicomp}
\alias{svnUserMatches}
\alias{existingSvnUsers}
\alias{getPackageTotals}
\alias{plotPackageTotals}
\alias{getPackageDeltas}
\alias{installDeps}
\alias{generatePermissionEdits}
\alias{rebuildIssueTarball}
\alias{removeDeadTrackerIssue}
\alias{filterIssues}
\alias{coneOfShame}
\alias{readyToAdd}
\alias{creditworthy}
\alias{getPackageRange}
\alias{getEmailAddressesToWarnPkgMaintainers}
\alias{compareReleases}
\alias{getDescriptions}
\alias{preacceptedToAccepted}
\alias{failmail}

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

  getPackageDeltas: looks at the manifests and gives package the deltas
  for each manifest transition.  So you can see how many packages were
  added with each release.
  
  plotPackageTotals: makes a simple plot of package growth over time.

  installDeps: Given a tarball or package directory, 
  this will install all its dependencies
  using biocLite().  This works even for tarballs/directories that are not yet
  available themselves via biocLite().

  generatePermissionEdits: Takes a vector of tarball paths, and prints
  output that can be pasted (after some scrutiny) into the permissions file.

  rebuildIssueTarball: A helper so that you don't have to bother Dan to
  rebuild a tarball when the tracker fails to do so for some reason.
  This function does require that you have access to habu

  removeDeadTrackerIssue: Another helper that will remove an issue
  completely from the tracker.  Use this with caution!  This also
  requires access to habu

  filterIssues: This function will get issue IDs and even tarballs that
  match certain date and status criteria.  So you want to know what
  packages were accepted in march of 2014?  Or maybe just which packages
  are classified as 'new-package' status from last month?  This function
  should help with that.
  
  coneOfShame: This function is named after the 'cone of shame' from
  UP.  Because sometimes you might get a false positive of the end user
  just writes 'thank you' and has not bounced the ball back into your
  court... But its still a useful funtion to seeing which packages are
  getting neglected and by how much.  The hope is that this should spare
  all of us the embarrassment of packages getting too far behind.

  readyToAdd: This is just a convenience for listing those packages
  that are accepted in the tracker but still not yet in the latest bioc
  manifest.  IOW this is to list out packages that need to be added so
  that we don't miss any.  We need to use this because that has happened
  before!  One issue is that this function has to rely on the package
  name in the title being accurate.  If those values are wrong, then
  this will give false positives.
 
  creditworthy: gives credit to core Seattle members and finds out whose
  done what in the last 30 days with respect to packages
 
  getPackageRange: Every 6 months, core Seattle members are given
  a range and they need to ensure that all packages in that range pass without
  any errors and warnings. This helper function takes in the name of reviewer
  and gives back the number of packages assigned to them, number of packages
  with errors and number of packages with warnings. 
  
  getEmailAddressesToWarnPkgMaintainers: just a convenience wrapper that
  calls getPackageRange and produces a list of emails addresses to
  bother with a form message.  Like getPackageRange, it takes a userName
  argument that it then uses to restrict which authors it will bother...

  compareReleases: given an 'old' and 'new' release, what packages were
  added and what packages were removed?

  getDescriptions: given a list of packages (maybe returned by
  compareReleases), get the Description fields of each and return
  as a string suitable for pasting into a release announcement.

  preacceptedToAccepted: finds the packages on the tracker with pre-accepted
  status and checks if the build report for them has any warnings or errors

  failmail: Sends email to maintainers whose packages are broken in
  the build system. All you have to provide is the package name,
  though you are able to customize the email that's sent.
}

\usage{
  clean(tarball, svnDir="~/proj/Rpacks/", copyToSvnDir=TRUE,
        svnAccountExists=FALSE)
  cleanDataPkg(tarball,
               svnDir1="~/proj/experiment/pkgs",
               svnDir2="~/proj/experiment/data_store",
               copyToSvnDir=TRUE,
               svnAccountExists=FALSE)
  makeBins(tarball)
  emailExistingUser(tarball, sendMail=FALSE)
  emailNewUser(tarball)
  requestNewSvnAccountFromScicomp(tarball, sendMail=FALSE)
  existingSvnUsers(tarballsPath=".", suffix=".tar.gz$")
  svnUserMatches(names)
  generatePermissionEdits(tarballsPath=".", suffix=".tar.gz$")

  rebuildIssueTarball(issueNumber,tarballUrlPath)
  removeDeadTrackerIssue(issueNumber)
  filterIssues(status=c('new-package'),datePrefix='2015',getUserFiles=FALSE)
  coneOfShame(daysNeglected=14, userName=NULL, daysToForget=30,lastTouchedFilter=TRUE)
  readyToAdd(datePrefix='2015',svnDir="~/proj/Rpacks/",
             svnDir1="~/proj/experiment/pkgs/", getUserFiles=FALSE)
  creditworthy(creditDays=30, userName)
  getPackageRange(userName = "sarora", biocVersion = "3.1")
  getEmailAddressesToWarnPkgMaintainers(userName)
  compareReleases(path = "~/proj/Rpacks/", oldRel="3.0", newRel="3.1")
  getDescriptions(path = "~/proj/Rpacks/", pkgs)
  preacceptedToAccepted()
  failmail(package, software=TRUE, 
    from=getOption("fromEmail", "dtenenba@fredhutch.org"),
    sig=getOption("mail.sig", "Dan"), subject=sprintf("\%s build problem", package),
    preview=TRUE, bccme=TRUE)
}

\arguments{
  \item{tarball}{The tarball (as built by R CMD build). In \code{installDeps},
  this can also be a directory.} 
  \item{svnDir}{The dir where the svn repos is located}
  \item{svnDir1}{The dir where the pkgs repos if located (data pkgs)}
  \item{svnDir2}{The dir where the data repos is located (data pkgs)}
  \item{copyToSvnDir}{TRUE/FALSE do we copy the cleaned dir to svnDir?}
  \item{svnAccountExists}{TRUE/FALSE does the maintainer of this tarball
    have an svn account alread?}
  \item{sendMail}{TRUE/FALSE do we call sendmail or do we print out a
    file to send later?}
  \item{names}{A character vector of strings that you think might be
    user names.}
  \item{tarballsPath}{The path to a group of tarballs to process}
  \item{suffix}{The suffix to use for recognizing tarball files
    (normally '.tar.gz' - so no need to specify it)}
  \item{issueNumber}{The roundup tracker issue number}
  \item{tarballUrlPath}{The url for a tarball that is to be checked}
  \item{status}{Any of the following tracker based states:
  'new-package', 'preview-in-progress', 'sent-back',
  'modified-package', 'review-in-progress', 'accepted', 'rejected'}
  \item{datePrefix}{Tracker dates are formatted like this:
  'YYYY-MM-DD timestamp', so provide a string that is formatted to match
  the amount of date that you intend to exactly match.  For example
  '2007-05' would match things from may of 2007.  It probably won't be
  useful to be more specific than that.}
  \item{getUserFiles}{TRUE/FALSE get the tarball names associated with
  issues that match?}
  \item{daysNeglected}{Number of days to wait before including in a
  cone of shame list}
  \item{userName}{username for a person whos activity you want to see}
  \item{daysToForget}{Number of days before something is old enough that
  we always want to exclude if from a cone of shame list}
  \item{lastTouchedFilter}{ TRUE/FALSE Filter based on whether the
  author was the last person to touch it?}
  \item{creditDays}{ Number of days for which you want to find out which
    reviewer has done what}
  \item{biocVersion}{What version of BiocConductor}
  \item{oldRel}{old release}
  \item{newRel}{new release}
  \item{pkgs}{a character vector of package names}
  \item{package}{The package to email the maintainer about.}
  \item{software}{TRUE for software packages, FALSE for experiment data packages.}
  \item{from}{From address for maintainer email.}
  \item{sig}{String to sign maintainer email with.}
  \item{subject}{Subject of maintainer email.}
  \item{preview}{Show a preview of maintainer email before sending?}
  \item{bccme}{If TRUE, copy maintainer email to 'from' address via BCC.}

}

\details{
In order to use the email functions you should put a line in your .Rprofile
file that looks like this:

\code{options( fromEmail = 'emailAddress@someplace.com')}

The 'failmail' function works a bit differently and requires
that you have something like this in your .Rprofile:

\code{options(email.options=list(smtpServer="mx", smtpPort=25))}
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

    ## clean up a data tarball and copy it to the svn repositories.
    cleanDataPkg(tarball)

    ## email someone who has an account
    emailExistingUser(tarball)

    ## generate an email for someone who needs svn credentials
    emailNewUser(tarball)

    ## send an email to Scicomp asking for a new account.
    requestNewSvnAccountFromScicomp(tarball)


    ## make mac and windows binaries for a package.
    tarball <- system.file("testpackages", "hgu95av2.db_2.10.1.tar.gz",
                           package="BiocContributions");
    makeBins(tarball)

    ## list existing svn users that are similar to tarball maintainers
    ## for all the tarballs in the working directory
    existingSvnUsers()

    ## list svn users like 'carlson'
    svnUserMatches('carlson')

    ## list how many packages are in each known release of Bioconductor
    getPackageTotals()

    ## Install all the dependencies for a tarball you need to review
    ## (but that isn't yet in biocLite itself)
    installDeps(tarball)

    ## Generate permission edits for a directory of new tarballs
    generatePermissionEdits()

    ## rebuild tarball issue 558 (URL indicates which tarball to rebuild)
    rebuildIssueTarball(558,
          'https://tracker.bioconductor.org/file4845/spbtest_0.99.0.tar.gz')

    ## remove issue 1114 from the tracker
    removeDeadTrackerIssue('1114')

    ## get issues that are from 2015 and that are either new or in
    ## preview
    filterIssues(status=c('new-package','preview-in-progress'),
                 datePrefix='2015')

    ## get similar issues that are from December of 2014
    filterIssues(status=c('new-package','preview-in-progress'),
                 datePrefix='2014-12')

    ## list users who may be currently behind on their package reviews
    ## by two weeks but not more than a month.
    coneOfShame()
    ## use coneOfShame to only see which issues mcarlson has not touched for
    ## more than one week
    coneOfShame(7, 'mcarlson')
    ## use coneOfShame to just see all issues assigned to mcarlson period:
    coneOfShame(1, 'mcarlson', lastTouchedFilter=FALSE)

    ## list packages that may be ready to be added to the repos.
    readyToAdd('2015')

    ## give credit to people for what they've worked on in the past 30 days
    df <- creditworth()

    diff <- compareReleases()
    descs <- getDescriptions(pkgs<-diff$added)
  }

  \dontrun{
  failmail("xps")
  }

}

\keyword{manip}

 
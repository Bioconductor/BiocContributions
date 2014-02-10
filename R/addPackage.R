## This is where code that is used for cleaning up a package and
## adding it to svn will live.

## I want to follow BiocCheck's lead here and do a cleanup command like:
## R CMD cleanup

## cleanup will then do the following:
## drop and lines from DESCRIPTION thet start with "packaged"
## rm -rf any build directories or  /inst/doc


## cp -r the package to the svn repos
## put the package into the most recent manifest



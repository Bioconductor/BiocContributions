## I plan to put email generating functions here.  I need to send
## emails to Carl, new/older authors, as well as prospective authors.
## These should all be functions if for no other reason than to allow
## their contents to be edited by all team members via svn.


## lets start with a function to send an email when the user already has an account.

.extractEmail <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    DESC[,grepl("Maintainer",colnames(DESC)),drop=FALSE]
}

.scrubOutEmailAddress <-function(rawEmail){
    as.character(sub(">$","",sub("^.*<","", rawEmail)))
}

.scrubOutNameFromEmail <-function(rawEmail){
    sub("\\s.?<.*$","", rawEmail, perl=TRUE)
}

.makeExistingUserMsg <- function(authorName, packageName){
    existingMsg <- paste(
                         "Hi ",
                         authorName,
                     ",

Your Subversion account information is the same as before, but
permissions have now been extended to your new package

You now have read/write access to the ",packageName," package:

  URL: https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/",packageName,"

Please let me know if you have any question or problem with your svn
access.


There is also an RSS feed maintained made for new software and data
packages.  You can find the rss feed for software packages at:

http://bioconductor.org/rss/build/packages/",packageName,".rss

And for data packages it will be at:

http://bioconductor.org/rss/build/data/packages/",packageName,".rss


For more general questions or advice about the development/maintenance of
your package, please use the Bioc-devel mailing list (our primary channel
for communication between developers and for important announcements like
release schedules, build system news, etc...).

Also, there is a page on our website that describes how to get set up
using svn (if you are unfamiliar).

http://www.bioconductor.org/developers/source-control/

And if you prefer to use git and Github instead of Subversion, you can
use the Bioconductor Git-svn bridge which is documented at:

http://www.bioconductor.org/developers/how-to/git-svn/



If you change email addresses, please make sure the Maintainer field
in the DESCRIPTION file of your package contains your current email
address. We may need to reach you if there are issues building your
package (this could happen as a result of changes to R or to packages
you depend on). If we can't reach you, we may have to drop your
package from Bioconductor.  If you have multiple developers on your
project who wish to help maintain it, we allow multiple names (with
addresses) in the maintainer field.  Let us know if you need more
accounts to facilitate this.

If you no longer want to maintain your package, please let us know and
we will remove it from Bioconductor, or (with your permission) find a
new maintainer for it.

Thanks for contributing to the Bioconductor project!

  Marc", sep="")
    ## then return
    existingMsg 
}

    
emailExisting <- function(tarball){
    require("sendmailR")
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    email <- .extractEmail(dir)
    ## clean the email out
    cleanEmail <- .scrubOutEmailAddress(email)
    ## extract name
    name <- .scrubOutNameFromEmail(email)
    ## format msg
    msg <- .makeExistingUserMsg(authorName=name, packageName=dir)
    ## use sendmail to send a message
    ##  sendmail(from='mcarlson@fhcrc.org', to=cleanEmail,
    ##           subject="Congratulations", body=existingMsg)
}




##  library(BiocContributions); tarball <- system.file("testpackages", "savR_0.99.1.tar.gz", package="BiocContributions");


## example of how sendmail should work:
##  sendmail(from='mcarlson@fhcrc.org', to='mrjc42@gmail.com', subject="Congratulations", body=existingMsg)

## the above will give me a 451 error.  To work around that I probably
## need to pass some args as a list to the control parameter...






##########################################################################
##########################################################################
## email for NEW users.  This one will also create an email from the
## tarball, but this time we can't email them since we have to still
## put the email credentials in...


## 1st we need our new user greeting:
.makeNewUserMsg <- function(authorName, packageName){
    existingMsg <- paste(
                         "Hi ",
                         authorName,
                     ",


Congrats on your package being accepted to Bioconductor.  The
following information is to help you in your new role as a package
maintainer.

Information about your svn account
--------------------------------------------------------

Your Subversion account is:




You now have read/write access to the ",packageName," package:

  URL: https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/",packageName,"

Please let me know if you have any question or problem with your svn
access.

Also, there is a page on our website that describes how to get set up
using svn (if you are unfamiliar).

http://www.bioconductor.org/developers/source-control/

And if you prefer to use git and Github instead of Subversion, you can
use the Bioconductor Git-svn bridge which is documented at:

http://www.bioconductor.org/developers/how-to/git-svn/



How we build, test and publish the Bioconductor packages
--------------------------------------------------------

Please also keep an eye on the build/check daily reports for the
Bioconductor devel packages:

  http://bioconductor.org/checkResults/

A package is made publicly available only if it passes the CHECK test
with no error (it can have warnings though). Then the easiest (and
recommended) way to install it is with biocLite():

  > source('http://bioconductor.org/biocLite.R')
  > biocLite('mypackage')

However, if your package is already published and you make changes
to it, the new version will not replace the old version unless
you have bumped z in the version number x.y.z. DON'T FORGET TO
BUMP Z! or the last version of your package will not be pushed
to the public repository.


There is also an RSS feed maintained made for new software and data
packages.  You can find the rss feed for software packages at:

http://bioconductor.org/rss/build/packages/",packageName,".rss

And for data packages it will be at:

http://bioconductor.org/rss/build/data/packages/",packageName,".rss


Mailing lists
-------------

For more general questions or advice about the development/maintenance
of your package, please use the Bioc-devel mailing list (our primary
channel for communication between developers and for important
announcements like release schedules, build system news, etc...).

If you have not already done so, we urge you to also subscribe to both
the bioc-devel AND the bioconductor mailing lists. The former for your
own benefit and the latter so that you can answer questions from users
of your package and respond to bug reports promptly.

You can subscribe to our mailing lists here:

http://www.bioconductor.org/help/mailing-list/

Also, when your package will pass the CHECK test for the first time,
we strongly encourage you to send a note to Bioc-devel to announce
its public availability (with a short description) so other people
can start to test it.


Package Maintainer field
------------------------

If you change email addresses, please make sure the Maintainer field
in the DESCRIPTION file of your package contains your current email
address. We may need to reach you if there are issues building your
package (this could happen as a result of changes to R or to packages
you depend on). If we can't reach you, we may have to drop your
package from Bioconductor.  If you have multiple developers on your
project who wish to help maintain it, we allow multiple names (with
addresses) in the maintainer field.  Let us know if you need more
accounts to facilitate this.

If you no longer want to maintain your package, please let us know and
we will remove it from Bioconductor, or (with your permission) find a
new maintainer for it.

Thanks for contributing to the Bioconductor project!



  Marc", sep="")
    ## then return
    existingMsg 
}


emailExisting <- function(tarball){
    require("sendmailR")
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    email <- .extractEmail(dir)
    ## clean the email out
    cleanEmail <- .scrubOutEmailAddress(email)
    ## extract name
    name <- .scrubOutNameFromEmail(email)
    ## format msg
    msg <- .makeNewUserMsg(authorName=name, packageName=dir)
    ## write the result to a file for convenience.
    con <- file(paste(dir,"_cngrtsEml_<",cleanEmail,">_.txt",sep=""))
    writeLines(text=msg,con=con)
}



########################################################
## TODO: deal with multiple maintainers more elegantly!
## TODO: fix the 451 smtp email bug

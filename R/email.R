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

.makeExistingMsg <- function(authorName, packageName){
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
    cleanEmail <- as.character(sub(">$","",sub("^.*<","",email)))
    ## extract name
    name <- sub("\\s.?<.*$","", email, perl=TRUE)
    ## format msg
    msg <- .makeExistingMsg(authorName=name, packageName=dir)
    ## use sendmail to send a message
    ##  sendmail(from='mcarlson@fhcrc.org', to=cleanEmail,
    ##           subject="Congratulations", body=existingMsg)
}




##  library(BiocContributions); tarball <- system.file("testpackages", "savR_0.99.1.tar.gz", package="BiocContributions");


## example of how sendmail should work:
##  sendmail(from='mcarlson@fhcrc.org', to='mrjc42@gmail.com', subject="Congratulations", body=existingMsg)

## the above will give me a 451 error.  To work around that I probably
## need to pass some args as a list to the control parameter...

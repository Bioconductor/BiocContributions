
########################################################
##
## TODO: deal with multiple maintainers more elegantly!
## TODO: write email function to request svn account from Carl.
##
########################################################



## I need to send emails to Carl, new/older authors, as well as
## prospective authors.  These should all be functions if for no other
## reason than to allow their contents to be edited by all team
## members via svn.

####################################################################
## lets start with a function to send an email when the user already
## has a svn account.

## This just gets the contents for the Maintainer field
## (There might be multiple emails returned from here)
.extractEmails <- function(dir){
    dirPath <- file.path(dir, "DESCRIPTION")
    DESC <- read.dcf(dirPath)
    rawEmail <- DESC[,grepl("Maintainer",colnames(DESC)),drop=FALSE]
    ## if there are multiples, clean them up
    emails <- unlist(strsplit(rawEmail, "> ?"))
    ## remove newlines and reattach ">'s", then return character() 
    paste(sub("\n"," ", emails),">",sep="")  
}

## This just extracts email adresses from a Maintainer field.
.scrubOutEmailAddresses <-function(rawEmail){
    as.character(sub(">$","",sub("^.*<","", rawEmail)))
}

## This just extracts names from a Maintainer field
.scrubOutNamesFromEmails <-function(rawEmail){
    sub("\\s.?<.*$","", rawEmail, perl=TRUE)
}

## Email wrapper so that I don't have to do this more than once
## NOTE: for sendmailR (or even command line mail) to run, you must
## have set /etc/mailname.  Mine was set to: gamay.fhcrc.org
.sendEmailMessage <- function(email, msg, subject){
    require("sendmailR")
    fromEmail = getOption("fromEmail")
    sendmail(from=fromEmail, to=email,
             subject=subject, msg=msg)
}

## And for when we want to send multiple messages:
.sendEmailMessages <- function(emails, msgs, subject){
    for(i in seq_along(emails)){
        .sendEmailMessage(emails[i], msgs[i], subject)
    }
}

## and code to define a single message template
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

## General purpose multiplier for functions that take authorName, packageName and that also have a function to define the message based on that.
.makeMessages <- function(authorNames, packageName, FUN){
    msgs <- character()
    for(i in seq_along(authorNames)){
        msgs[i] <- FUN(authorName=authorNames[i], packageName)
    }
    msgs
}

emailExistingUser <- function(tarball){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)    
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## format msgs
    msgs <- .makeMessages(authorName=names, packageName=dir,
                          FUN=.makeExistingUserMsg)
    ## subject
    subject <- paste("Congratulations.  Package",dir,
                     "has been added to the repository.")
    ## send an email at this time.
    .sendEmailMessages(email=cleanEmails, msg=msgs, subject=subject)
    ## cleanup
    unlink(dir, recursive=TRUE)
}


##############################################
##  example
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## emailExistingUser(tarball)








##########################################################################
##########################################################################
## email for NEW users.  This one will also create an email from the
## tarball, but this time we can't email them since we have to still
## put the email credentials in...


## 1st we need our new user greeting:
.makeNewUserMsg <- function(authorName, packageName){
    newUserMsg <- paste(
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
recommended) way to install it is with biocLite().  You can learn more
about how to use biocLite() here:

http://www.bioconductor.org/install/

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
    newUserMsg 
}

.writeOutEmailTemplates <- function(paths, msgs){
    for(i in seq_along(paths)){
        con <- file(paths[i])
        writeLines(text=msgs[i], con=con)
        close(con)
    }
}

emailNewUser <- function(tarball){
    require("sendmailR")
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## format msg
    msgs <- .makeMessages(authorName=names, packageName=dir,
                          FUN=.makeNewUserMsg)
    ## write the result to a file for convenience.
    paths <- paste(dir,"_cngrtsEml_<",cleanEmails,">_.txt",sep="")
    ## now make connections and write results out.
    .writeOutEmailTemplates(paths, msgs)
    ## cleanup
    unlink(dir, recursive=TRUE)
}


##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## emailNewUser(tarball)

## works





##########################################################################
##########################################################################
## email for new svn accounts.  This one takes a tarball and sends an
## email to Carl at scicomp regarding new accounts.

.makeNewSvnUserRequestMsg <- function(emailsAndUserNames){
    msg <- paste("Hi Carl,

Could you please create a new svn account on hedgehog for

",emailsAndUserNames,"

Thanks!

    Marc", sep="")
    ## then return
    msg 
}

.generateProposedUsername <- function(names){
    res <- character()
    for(i in seq_along(names)){
        firstName <- unlist(strsplit(names[i]," "))[1]
        init <- tolower(substr(firstName,1,1))
        numNames <- length(unlist(strsplit(names[i]," ")))
        lastName <- tolower(unlist(strsplit(names[i]," "))[numNames])
        res[i] <- paste(init, lastName, sep=".") 
    }
    res
}

## TODO: maybe I should modify this to take a SERIES of tarballs...
## BUT 1st I need to refactor my functions that access svn logs.
emailNewSvnAccount <- function(tarball, sendMail=TRUE){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## clean the email out
    cleanEmails <- .scrubOutEmailAddresses(emails)    
    ## extract name
    names <- .scrubOutNamesFromEmails(emails)
    ## make a proposed username.
    usernames <- .generateProposedUsername(names)

    ## generate emails and UserNames
    emailsAndUserNames <- paste(
                                paste(emails,
                                      "\n\n  proposed username:",
                                      usernames,
                                      "\n"),
                                collapse="\n\n AND \n\n")
    
    ## format msgs
    msg <- .makeNewSvnUserRequestMsg(emailsAndUserNames)
    if(sendMail){
        ## send an email at this time.
        ## .sendEmailMessage(email="scicomp@fhcrc.org", msg=msg,
        ##                   subject="new svn account")
        email = getOption("fromEmail")
        .sendEmailMessage(email=email, msg=msg,
                          subject="new svn account")
    }else{
        con <- file(paste(dir,"_svnRequest_<scicomp@fhcrc.org>_.txt",sep=""))
        writeLines(text=msg, con=con)
        close(con)
    }
    ## cleanup
    unlink(dir, recursive=TRUE)
}



##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## emailNewSvnAccount(tarball)

## works


## emailNewSvnAccount(tarball, sendMail=FALSE)





##############################################################################
## I need a tool for getting latest svn perms 

## Problem: the above requires a passphrase to access the content.
## I am going to email scicomp to see if they can help me square that away.


## Helper to read in 'bioconductor.authz'
.extractUsernamesFromAuthz <- function(){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}    
    ## Just get the latest file  (this will require you
    ## to enter your passphrase
    permFile = getOption("permFile")
    cmd <- paste0('rsync ',permFile,' .')
    system(cmd)
    
    if(file.exists('bioconductor.authz')){
        con <- file('bioconductor.authz')
        res <- readLines(con)
        close(con)
        cats <- c("^bioconductor-readers =","^bioconductor-write0 =")
        res <- res[ grepl(cats[1], res) | grepl(cats[2], res)  ]
        res <- unlist(strsplit(res, ","))
        res <- unique(sub(" ","",sub(cats[2],"",sub(cats[1],"",res))))
    }
    unlink("bioconductor.authz")
    res
}

####################################################################
## Check if a username exists in svn
## I need this to be a public and private way of looking at whether an
## svn user exists for Bioconductor.
## So all the above emails should use this check 1) make sure that a user exists


## These return TRUE or FALSE
.svnUserExists <- function(name){
    names <- .extractUsernamesFromAuthz()
    ## now grep
    any(grepl(name, names))
}

.svnUsersExist <- function(names){
    unlist(lapply(names, .svnUserExists))
}


## these returns matches (so you can think about it better)
.svnUserMatcher <- function(name){
    names <- .extractUsernamesFromAuthz()
    ## now grep
    names[grepl(name, names)]
}

svnUserMatches <- function(names){
    unlist(lapply(names, .svnUserMatcher))
}




## Check if a tarball is in svn yet or not.
## (for quickly assessing - a standalone function)
existingSvnUsers <- function(tarball){
    ## untar
    untar(tarball)
    ## get dir
    dir <- .getShortPkgName(tarball)
    ## extract email from DESCRIPTION file
    emails <- .extractEmails(dir)
    ## extract names
    names <- .scrubOutNamesFromEmails(emails)
    ## make a proposed username.
    usernames <- .generateProposedUsername(names)
    ## get the answer
    res <- svnUserMatches(usernames)
    ## cleanup
    unlink(dir, recursive=TRUE)
    if(length(res) == 0){
        message("No matching users found...  Please consider: \n\n",
                emails,"\n\n",
                "proposed username: ", usernames, "\n")
    }else{
        message("FOUND THE FOLLOWING MATCHES: ", res,"\n",
                "MATCHES HAVE THESE EMAILS: ", emails, "\n")
    }
}



##############################################
##  example
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## existingSvnUsers(tarball)




## TODO: make use of the above helpers in the other email functions (but ONLY after we get better access to the .authz file)

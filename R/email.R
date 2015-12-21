
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
.extractEmails <- function(description){
    as.person(description$Maintainer)
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
.makeExistingUserMsg <- function(authorName, packageName, senderName = "Jim"){
    template <-
"Hi {{authorName}},

Your package has been added to the Bioconductor repository.
Some helpful notes to help you as a package maintainer:

a) SVN Information
Your Subversion account information is the same as before, but
permissions have now been extended to your new package
You now have read/write access to the {{packageName}} package:
URL: https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/{{packageName}}
Please let me know if you have any questions or issues with your SVN
access.
SVN docs at: http://bioconductor.org/developers/how-to/source-control/

b) Build Report:
Your package's build reports are accessible at:
http://bioconductor.org/checkResults/{{biocVersion}}/bioc-LATEST/{{packageName}}/
Please keep an eye on the daily build/check reports and fix any Warnings or
Errors that occur.

c) RSS Feeds:
You can find the RSS feed for your software packages at:
http://bioconductor.org/rss/build/packages/{{packageName}}.rss

d) Stay connected using Bioc-devel mailing list and support site
http://bioconductor.org/help/support/#bioc-devel

e) Bioconductor Git mirrors
If you prefer to use Git and GitHub instead of Subversion, you can
use the Bioconductor Git mirrors which are documented at:
http://bioconductor.org/developers/how-to/git-mirror/

f) Add Maintainer or Removal of Package from Bioconductor:
Please email us at packages@bioconductor.org with your requests
for new maintainers, please clearly state their name and email address
and CC them on the request email sent to us.

g) Permanent URL of your package.
Your package has a permanent URL:

http://bioconductor.org/packages/{{packageName}}/

This will redirect to the release landing page of your package
(and until it's released, the devel landing page). Therefore this
is the URL that should be used (in publications, etc.) to refer
to your package. For convenience, you can also refer specifically to
the devel version, the release version, or a specific numbered
version of Bioconductor:

http://bioconductor.org/packages/devel/{{packageName}}/
http://bioconductor.org/packages/release/{{packageName}}/
http://bioconductor.org/packages/{{biocVersion}}/{{packageName}}/

Thanks for contributing to the Bioconductor project!

<<senderName>>"

  whisker::whisker.render(template,
                 list(packageName = packageName,
                      authorName = authorName,
                      biocVersion = BiocInstaller:::BIOC_VERSION,
                      senderName = senderName))
}

## General purpose multiplier for functions that take authorName, packageName and that also have a function to define the message based on that.
.makeMessages <- function(authorNames, packageName, FUN, ...){
    msgs <- character()
    for(i in seq_along(authorNames)){
        msgs[i] <- FUN(authorName=authorNames[i], packageName, ...)
    }
    msgs
}

emailExistingUser <- function(tarball, sendMail=FALSE){
    description <- readDESCRIPTION(tarball)

    package <- description[[1, "Package"]]


    ## extract email from DESCRIPTION file
    emails <- .extractEmails(description)

    msgs <- .makeMessages(authorName=emails$given, packageName=package,
                          FUN=.makeExistingUserMsg)
    ## subject
    subject <- paste("Congratulations.  Package", package,
                     "has been added to the repository.")
    ## either send emails OR write out messages
    if (sendMail){
        ## send an email at this time.
        .sendEmailMessages(email=emails$email, msg=msgs, subject=subject)
    } else {
        paths <- paste(package, "_congratsEmail_existing_<", emails$email, ".txt", sep="")
        ## now make connections and write results out.
        .writeOutEmailTemplates(paths, msgs)
    }
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
.makeNewUserMsg <- function(authorName, packageName, userId = "<user.name>", password = "<password>", senderName = "Jim"){
    template <-
"Hi {{authorName}},

Congrats on your package being accepted to Bioconductor.  The following
information is to help you in your new role as a package maintainer.

Every package in Bioconductor gets its own landing page. Contents from your
DESCRIPTION file are pulled out to populate this page. Your package's
permanent URL is:  http://bioconductor.org/packages/{{packageName}}/

This will redirect to the release landing page of your package (and
until it's released, the devel landing page). Therefore this is the
URL that should be used (in publications, etc.) to refer to your package.
For convenience, you can also refer specifically to the devel version,
the release version, or a specific numbered version of Bioconductor:

http://bioconductor.org/packages/devel/{{packageName}}/
http://bioconductor.org/packages/release/{{packageName}}/
http://bioconductor.org/packages/{{biocVersion}}/{{packageName}}/

Maintaining your package:

1) Bioconductor Branches - Release and Devel

Bioconductor has two versions of EACH package - release and devel.

Release - http://bioconductor.org/packages/release/bioc/html/{{packageName}}.html

The release branch is the stable branch of your package which is
constant - Every 6 months during the Bioconductor release,
whatever is in your devel becomes the release branch. Currently
since your package has not gone through a release cycle, you do
not have a release branch.

Devel - http://bioconductor.org/packages/devel/bioc/html/{{packageName}}.html

Your package has been added to the devel branch. All changes that you need to
make will be done to the devel branch.
During the next Biocondctor Release, http://bioconductor.org/developers/release-schedule/
whatever is in your devel branch will be added to the release and only then
you will have two versions of your package.

2) Updating your package in Bioconductor:
You will need to use subversion to update your package inside Bioconductor.
SVN GUIDE: http://bioconductor.org/developers/how-to/source-control/

Your subversion account credentials are as follows.

Subversion user ID: {{userId}}
Password: {{password}}

These credentials give you read access to the whole Bioconductor
repository and WRITE permissions only to your package.

To update your package in the devel branch, you need to do the following steps:
a) Install subversion(svn) on your machine, if it is not already installed.
b) svn co --username {{userId}} --password {{password}} https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/{{packageName}}
To checkout your packages files from the Bioconductor subversion repository.
c) Make the nessesary changes to your package.
d) Bump the version from x.y.z to x.y.(z+1) in your package's DESCRIPTION file.
   If the version is not properly changed your changes will not be pushed to the
   public repository.
e) R CMD build {{packageName}}
f) R CMD check {{packageName}}_x.y.(z+1).tar.gz
g) Fix any Warnings or Errors from step (e) and (f)
h) svn ci {{packageName}}
g) Check the build report next day (see point 3 for details)
Please let me know if you have any questions or issues with your SVN access.

3) Build report:
As stated in 1), all changes are made to devel branch of your package.
When you make a change to the devel branch of your package, please remember
to bump the version of your package in the DESCRIPTION FILE.
Everyday at around 5pm PST, the build system takes a snapshot of all the
packages inside Bioconductor and then the next day after 12 noon PST,
http://bioconductor.org/checkResults/ is created containing the
output of R CMD build and check on all platforms for each package.
Your package's build reports are accessible at:
http://bioconductor.org/checkResults/{{biocVersion}}/bioc-LATEST/{{packageName}}/

When reading the above, please pay attention to the date displayed
next to - “Snapshot Date:” and “This page was generated on”.
Please  keep an eye on the build/check daily reports for the Bioconductor
devel packages:  http://bioconductor.org/checkResults/ and get rid of any
warnings or errors from your packages build report.

4) RSS feeds:
You can find the RSS feed for software packages at:
http://bioconductor.org/rss/build/packages/{{packageName}}.rss

5) Bioc-devel mailing list
The Bioc-devel mailing list is used for communication between developers and
for important announcements like release schedules, build system news, etc...
Please check emails from the list as they are our primary communication method
to package developers.

Also, after your package has passed the build report's CHECK test for the first
time, we strongly encourage you to send a note to Bioc-devel to announce its
public availability (with a short description) so other people can start to
test it.

6) Adding Maintainers for your package:
If for some reason, your email address changes, please update the
maintainer field in your DESCRIPTION file. We may need to reach you
if there are issues building your package (this could happen as a
result of changes to R or to packages you depend on). If we are unable to
contact you for a period of time, we may be forced to remove your package from
Bioconductor.

If you want to add a new maintainer or transfer responsibility to
someone else,  please email us at packages@bioconductor.org
and clearly state the new maintainers name, email address and CC them on the
email.

7) Support Site:
Please respond to bug reports promptly at https://support.bioconductor.org/
We recommend that you 'follow' tags that match your own package (such
as your package name) so that you will be notified when someone is
asking a question that pertains to your work.

8) Removal of your package:
If you no longer want to maintain your package, please let us know and we will
remove it from Bioconductor, or (with your permission) find a new maintainer
for it. See: http://bioconductor.org/developers/package-end-of-life/

9) Helpful things to know about Bioconductor:

Bioconductor Newsletter: http://bioconductor.org/help/newsletters/
Upcoming Courses: http://bioconductor.org/help/events/
Course Material from past courses: http://bioconductor.org/help/course-materials/
YouTube channel: https://www.youtube.com/user/bioconductor/
Twitter: https://twitter.com/Bioconductor

Thanks for contributing to the Bioconductor project!

{{senderName}}"


  whisker::whisker.render(template,
                 list(packageName = packageName,
                      authorName = authorName,
                      biocVersion = BiocInstaller:::BIOC_VERSION,
                      userId = userId,
                      password = password,
                      senderName = senderName))
}

.writeOutEmailTemplates <- function(paths, msgs){
    for(i in seq_along(paths)){
        con <- file(paths[i])
        writeLines(text=msgs[i], con=con)
        close(con)
    }
}

emailNewUser <- function(tarball, userId = "user.id", password = "password", senderName = "Jim"){
    description <- readDESCRIPTION(tarball)

    package <- description$Package

    ## extract email from DESCRIPTION file
    emails <- .extractEmails(description)

    msgs <- .makeMessages(authorName=emails$given, packageName=package,
                          userId = userId,
                          password = password,
                          senderName = senderName,
                          FUN=.makeNewUserMsg)
    ## write the result to a file for convenience.
    paths <- paste(package, "_congratsEmail_<", emails$email, ">.txt", sep="")
    ## now make connections and write results out.
    .writeOutEmailTemplates(paths, msgs)
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

.generateProposedUsername <- function(given, family){
    tolower(paste(substr(given, 1, 1), family, sep = "."))
}

## TODO: maybe I should modify this to take a SERIES of tarballs...
## BUT 1st I need to refactor my functions that access svn logs.
requestNewSvnAccountFromScicomp <- function(tarball, sendMail=FALSE){
    description <- readDESCRIPTION(tarball)

    emails <- .extractEmails(description)
    usernames <- .generateProposedUsername(emails$given, emails$family)

    ## generate emails and UserNames
    emailsAndUserNames <- paste(
                                paste(emails$email,
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
}



##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## requestNewSvnAccountFromSciComp(tarball)

## works


## requestNewSvnAccountFromSciComp(tarball, sendMail=FALSE)





##############################################################################
## I need a tool for getting latest svn perms 

## Problem: the above requires a passphrase to access the content.
## I am going to email scicomp to see if they can help me square that away.


## this (old) extractor is for when you only want to know if someone has
## access to bioconductor or not. ## TODO; if we ever start to use
## this we will want to also load it to the zzz.R file etc.
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

## This extractor is final word for knowing if an svn account exists at all...
## This is generally the conservative choice for most testing.
.extractUsernamesFromUsers <- function(){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}    
    ## Just get the latest file  (this will require you
    ## to enter your passphrase
    ## usersFile = getOption("usersFile")
    ## cmd <- paste0('rsync ',usersFile,' .')
    ## system(cmd)
    tempDir <- get('tempDir', BiocContributions:::stash)
    usersFile <- file.path(tempDir, 'users') 
    
    if(file.exists(usersFile)){
        con <- file(usersFile)
        res <- readLines(con)
        close(con)
        res <- strsplit(res, ":")
        res <- unique(unlist(lapply(res, function(x){x[1]})))
    }
    ## unlink("users")
    res
}

## TODO/Bug fix: change the arrangement so that the file above is
## extracted ONCE per call of the highest level function (and then the
## file handle is passed down).  This will get rid of the bug where we
## have to type in the passphrase every time that we have a new user
## name...  Once call per functions should really be more than enough.  In fact,
## better would be to call it only once when we first load the package!

##
## TODO: make helper for extracting data from getOption("userDbFile")
## This will allow checking to see if the email in the package is the
## same as the one we have on record.
##


####################################################################
## Check if a username exists in svn
## I need this to be a public and private way of looking at whether an
## svn user exists for Bioconductor.
## So all the above emails should use this check 1) make sure that a user exists


## These return TRUE or FALSE
.svnUserExists <- function(name){
    names <- .extractUsernamesFromUsers()
    ## now grep
    any(grepl(name, names))
}

.svnUsersExist <- function(names){
    unlist(lapply(names, .svnUserExists))
}


## these returns matches (so you can think about it better)
.svnUserMatcher <- function(name){
    names <- .extractUsernamesFromUsers()
    ## now grep
    names[grepl(name, names)]
}

svnUserMatches <- function(names){
    unlist(lapply(names, .svnUserMatcher))
}




## Check if a tarball is in svn yet or not.
## (for quickly assessing - a standalone function)
.existingSvnUsers <- function(tarball){
    description <- readDESCRIPTION(tarball)

    ## extract email from DESCRIPTION file
    emails <- .extractEmails(description)

    usernames <- .generateProposedUsername(emails$given, emails$family)

    res <- svnUserMatches(usernames)
    structure(list(
                   package = description$Package,
                   people = emails,
                   svn = usernames,
                   matches = res
                   ),
              class = "svn_match")
}

existingSvnUsers <- function(path = ".", pattern = ".tar.gz$"){
    res <- lapply(dir(path = path, pattern = pattern, full.names = TRUE), .existingSvnUsers)
    class(res) <- "svn_matches"
    res
}

print.svn_match <- function(x, ...) {
    message("Package: ", x$package, "\n",
            "Maintainer: ", x$people, "\n",
            "Username: ", paste(x$svn, collapse = ", "), "\n",
            "Matches: ", paste(x$matches, collapse = ", "), "\n")
}

print.svn_matches <- function(x, ...) {
    lapply(x, print)
    invisible()
}

as.logical.svn_match <- function(x, ...) {
    length(x$matches) > 0
}

as.logical.svn_matches <- function(x, ...) {
    vapply(x, as.logical, logical(1))
}

##############################################
##  example
##  library(BiocContributions); tarball <- system.file("testpackages", "AnnotationHub_1.3.18.tar.gz", package="BiocContributions");

## existingSvnUsers()




## TODO: make use of the above helpers in the other email functions (but ONLY after we get better access to the .authz file)






##############################################################################
## helper for generating the stuff we put into the permissions file
## 1st: lets do the really annoying part at the end.
## then do the middle part, but don't worry about the 1st part.

## things to bear in mind:
## This will tell you what version you are using
## biocVersion()  ## BiocInstaller:::BIOC_VERSION
## You need to use this string to format the tedious part later

## You need to also make sure we are using devel in order to even try
## to use this function.  (non-devel is not permitted)
## this will tell if you are using devel or not
## isDevel <- function(){packageVersion("BiocInstaller")$minor %% 2 == 1}

## Helper to retrieve userName and packageName
.getPkgNameAndUser <- function(tarball){

    description <- readDESCRIPTION(tarball)

    emails <- .extractEmails(description)

    usernames <- .generateProposedUsername(emails$given, emails$family)

    finalUserNames <- paste(usernames, collapse=", ")
    ## get the answer
    res <- svnUserMatches(usernames)
    finalUserNames <-  paste(res, collapse=", ")
    ## Combine and return
    names(finalUserNames) <- description$Package
    finalUserNames
}

## helper for ONLY getting tarballs (used instead of dir())
.getTars <- function(path=".",suffix=".tar.gz$"){
    if(grepl(suffix,path)){
        stop("You need to supply a path that contains tarballs: not an actual tarball...")
    }
    res <- dir(path)
    res[grepl(suffix,res)]
}

.printAssociations <- function(elem){
    paste0(names(elem), " = " , elem, "\n")
}

.printTediousStuff <- function(elem){
    pkg <- names(elem)
#    version <- biocVersion() ## re-enable this in fall 
    version <- "3.1" ## Till just before release (b/c we want 'version before')
    part1 <- strsplit(as.character(version),split='[.]')[[1]][1]
    part2 <- strsplit(as.character(version),split='[.]')[[1]][2]
#    part2 <- as.character(as.integer(part2) - 1) ## no longer needed?
    version <- paste0(part1,"_",part2) 
    paste0("[/trunk/madman/Rpacks/",pkg,"]\n@",pkg,
           " = rw\n\n",
           "[/branches/RELEASE_",version,"/madman/Rpacks/",pkg,"]\n@",pkg,
           " = rw\n\n")
}

## helper to test if we are in devel
.isDevel <- function(){packageVersion("BiocInstaller")$minor %% 2 == 1}

## tarballs is a character vector of tarball paths.
generatePermissionEdits <- function(path = ".", pattern = "\\.tar\\.gz$"){
    ## start with tarballs in whatever dir we have here...
    tarballs <- dir(path = path, pattern = pattern, full.names = TRUE)
    ## store the above in a list object
    data <- lapply(tarballs, .getPkgNameAndUser)
    
    ### For all packages in that list:

    ## write out association part (for each - helper2)
    message(paste(sapply(data, .printAssociations), collapse=""))
    
    ## write out the tedious part (for each - helper3)
    message(paste(sapply(data, .printTediousStuff), collapse=""))
    
}











## Output should look like:
## , y.shen, t.carroll, w.yang, f.zhang, j.schumann, a.waardenberg

## ASSIGN = y.shen
## ChIPQC = t.carrol, r.stark
## ABSSeq = w.yang
## FRGEpistasis = f.zhang
## flowCyBar = j.schumann
## CompGO = a.waardenberg
## Rariant = j.gehring

## [/trunk/madman/Rpacks/ASSIGN]
## @ASSIGN = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ASSIGN]
## @ASSIGN = rw

## [/trunk/madman/Rpacks/ChIPQC]
## @ChIPQC = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ChIPQC]
## @ChIPQC = rw

## [/trunk/madman/Rpacks/ABSSeq]
## @ABSSeq = rw

## [/branches/RELEASE_2_13/madman/Rpacks/ABSSeq]
## @ABSSeq = rw

## [/trunk/madman/Rpacks/FRGEpistasis]
## @FRGEpistasis = rw

## [/branches/RELEASE_2_13/madman/Rpacks/FRGEpistasis]
## @FRGEpistasis = rw

## [/trunk/madman/Rpacks/flowCyBar]
## @flowCyBar = rw

## [/branches/RELEASE_2_13/madman/Rpacks/flowCyBar]
## @flowCyBar = rw

## [/trunk/madman/Rpacks/CompGO]
## @CompGO = rw

## [/branches/RELEASE_2_13/madman/Rpacks/CompGO]
## @CompGO = rw

## [/trunk/madman/Rpacks/Rariant]
## @Rariant = rw

## [/branches/RELEASE_2_13/madman/Rpacks/Rariant]
## @Rariant = rw

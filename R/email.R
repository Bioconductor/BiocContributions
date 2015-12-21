
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
  template("existingUserAcceptance.txt",
      packageName = packageName,
      authorName = authorName,
      biocVersion = BiocInstaller:::BIOC_VERSION,
      senderName = senderName)
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
    subject <- fmt("Congratulations, {{package}} has been added to Bioconductor!",
                   list(package = package))

    gmailr::mime(Subject = subject,
                 To = emails$email,
                 From = "packages@bioconductor.org",
                 body = msgs)
    ## either send emails OR write out messages
    #if (sendMail){
        ## send an email at this time.
        #.sendEmailMessages(email=emails$email, msg=msgs, subject=subject)
        #mime
    #} else {
        #paths <- paste(package, "-email-existing-<", emails$email, ">.txt", sep="")
        ### now make connections and write results out.
        #.writeOutEmailTemplates(paths, msgs)
    #}
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

trackerSuccess <- function(tarball, type = c("software", "experiment-data"),
                           senderName = "Jim") {

    type <- match.arg(type)

    description <- readDESCRIPTION(tarball)
    email <- .extractEmails(description)

    switch(type,
           software = template("tracker.txt",
                    author = paste(email$given, collapse = ", "),
                    tarball = basename(tarball),
                    package = description$Package,
                    senderName = senderName,
                    when = "Everyday",
                    type = "bioc-LATEST"),
           `experiment-data` = template("tracker.txt",
                    author = paste(email$given, collapse = ", "),
                    tarball = basename(tarball),
                    package = description$Package,
                    senderName = senderName,
                    when = "Wednesday and Saturday",
                    type = "data-experiment-LATEST")
           )

}

template <- function(path, ...) {
    template <- readFile(system.file(package = "BiocContributions", "extdata", path))
    res <- whisker::whisker.render(template, list(...))
    class(res) <- "template"
    res
}

print.template <- function(x, ...) {
    cat(x)
    invisible(x)
}

readFile <- function(file) {
    readChar(file, file.info(file)$size)
}
## 1st we need our new user greeting:
.makeNewUserMsg <- function(authorName, packageName, userId = "<user.name>", password = "<password>", senderName = "Jim"){
    template("newUserAcceptance.txt",
        packageName = packageName,
        authorName = authorName,
        biocVersion = BiocInstaller:::BIOC_VERSION,
        userId = userId,
        password = password,
        senderName = senderName)
}

# TODO need a message for data packages

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

    msgs <- .makeNewUserMsg(authorName=emails$given[1], packageName=package,
                          userId = userId,
                          password = password,
                          senderName = senderName)

    subject <- fmt("Congratulations, {{package}} has been added to Bioconductor!",
                   list(package = package))

    gmailr::mime(Subject = subject,
                 To = emails$email[1],
                 From = "packages@bioconductor.org",
                 body = msgs)
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

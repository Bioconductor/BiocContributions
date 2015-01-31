## This is a file for functions that interact with the issue tracker in some way

##############################################################################
## A function that can build a particular tracker issue again. (So
## that we don't have to bother Dan every time)
rebuildIssueTarball <- function(issueNumber,
                                tarballUrlPath){
    pythonPath<-'~/proj/IssueTracker/spb_history/'
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}
    
    ###############################################
    ## use system to call the python script
    pythonCmd <- paste0("python ",pythonPath,"rerun_build.py")
    cmd <- paste("ssh habu '", pythonCmd, issueNumber, tarballUrlPath,"'")
    system(cmd)
}


## example - test it out on sbptest:
## rebuildIssueTarball(558,'https://tracker.bioconductor.org/file4845/spbtest_0.99.0.tar.gz')



##############################################################################
## A function that will use the command line to remove a dead issue
removeDeadTrackerIssue <- function(issueNumber){
    if(.Platform$OS.type != "unix"){
        stop("Sorry this function is only available from Unix")}
    ## we really don't want a space between issue and issueNumber variable...
    adminCmd <- paste0("roundup-admin ",
                       "-i /var/www-trackers/bioc_submit retire issue",
                       issueNumber)
    cmd <- paste("ssh habu '" ,adminCmd, "'")
    system(cmd)
}


## usage of  this function:
## removeDeadTrackerIssue('1114')






###############################################################################
###############################################################################
## tracker SQL querying functions:
###############################################################################
###############################################################################
##
###############################################################################
## Some notes about the MySQL DB:
## Connect on habu like this:
## mysql -p -h habu.fhcrc.org -u roundup roundup_bioc_submit
###############################################################################

## contents of the tables:
## (This is promising - IOW the data is there, but schema is pretty obscure)
## _issue = one record per issue.
## _file = lists all uploaded file names
## _keyword = list of things put into the keywords slot
## _msg = list of messages who messaged who and when
## _status = status types (new-package, sent-back etc.)
## _user = list of all users
## issue_files = looks like a graph object is here. (big)
## issue_keyword = another (tiny) graph
## issue_messages = another graph (big)
## issue_nosy = another graph - there is a trend here...
## msg_files = another graph (big)
## msg_recipients = another graph (big)
## user_queries = another graph (10 records)
## ***__journal = another set of tables that look a lot alike. These appear to be related to the graphs  (they all have info. about nodeids)
## sessions = appears to log sesssions for each user (one record per PC that they use to connect to the site?) - just a guess




## Helper to get connection to the roundup DB
.getRoundupCon <- function(){
    pswd <- getOption("trackerPSWD")
    if(!exists('pswd')){
        stop("You need to set a password for the issue tracker in .Rprofile")
    }
    require(RMySQL)
    dbConnect(dbDriver('MySQL'),
              host='habu.fhcrc.org',
              dbname='roundup_bioc_submit',
              user='roundup',
              pass=pswd)
}


.getStatus <- function(str){
    switch(str,
           'new-package'=1,
           'preview-in-progress'=2,
           'sent-back'=3,
           'modified-package'=4,
           'review-in-progress'=5,
           'accepted'=6,
           'rejected'=7)
}


###############################################################################
## Make function that can get the links and DESCRIPTION files from the issue tracker DB for all unassigned issues.

filterIssues <- function(status=c('new-package'),
                         datePrefix='2015',
                         getUserFiles=FALSE){
    ## argument checking and processing
    validStatuses <- c('new-package','preview-in-progress','sent-back',
                       'modified-package','review-in-progress','accepted',
                       'rejected')
    match.arg(status, choices=validStatuses, several.ok=TRUE)
    if(!isSingleString(datePrefix)) stop("datePrefix must be single string")
    statusIds <- unlist(lapply(status,.getStatus))
    fmtStatusIds <- paste0(statusIds, collapse="','")
    ## DB stuff
    con <- .getRoundupCon()
    sql1 <- paste0("SELECT issue._title AS title ",
                   ",issue.id, file._name AS name, ",
                   "issue._activity AS activity ",
                   "FROM ",
                   "(SELECT * FROM _issue ",
                   "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                   "AND _issue._activity LIKE '",datePrefix,"%') ",
                   "AS issue, ",
                   "(SELECT * FROM _file ",
                   "WHERE _file._activity LIKE '",datePrefix,"%') ",
                   "AS file ",
                   "WHERE file._creator=issue._creator")
    sql2 <- paste0("SELECT _title AS title, id, _activity AS activity ",
                   "FROM _issue ",
                   "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                   "AND _issue._activity LIKE '",datePrefix,"%'")

    if(getUserFiles==TRUE){
        dbGetQuery(con, sql1)
    }else{
        dbGetQuery(con, sql2)   
    }
}


## Usage: filterIssues(status=c('new-package','preview-in-progress'), datePrefix='2015')

## And datePrefix can be made more specific.  So for example:
## Usage: filterIssues(status=c('new-package','preview-in-progress'), datePrefix='2014-12')
## Please NOTE:
## 'full' dates are formatted like this: 'YYYY-MM-DD timestamp'
## for example: '2007-05-07 18:50:11'

## And if you want the files too you can do this:
## Usage: filterIssues(status=c('new-package','preview-in-progress'), datePrefix='2015', getUserFiles=TRUE)



##############################################################################
## Then make a function that will get all records from the tracker
## that have not been checked for a couple of weeks or longer.  (all
## this is in _issue I think)

coneOfShame <- function(daysNeglected=14, daysToForget=30){
    con <- .getRoundupCon()
    sql <- "SELECT issue.dateDiff,issue._title AS title,issue.id,issue._activity AS activity,issue._actor AS actor,issue._assignedto AS assignedto,_user._address AS address,_user._username AS username FROM (SELECT DATEDIFF(DATE(NOW()), DATE(_activity)) AS dateDiff,_activity,_actor,_title,id,_assignedto FROM _issue WHERE _issue._status IN ('2','3','4','5')) AS issue, _user WHERE _user.id=issue._assignedto ORDER BY activity DESC"
    res <- dbGetQuery(con, sql)
    ## Remove records where the reviewer was the last person to touch the issue.
    res <- res[res$actor!=res$assignedto,]
    ## And then filter based on the dateDiff
    res <- res[res$dateDiff > daysNeglected,]
    ## And then filter based on things just being too damned old to matter
    res <- res[res$dateDiff < daysToForget,]
    res
}

## Usage: coneOfShame(daysNeglected=14, daysToForget=30)



##############################################################################
## And make a function that will look at records that are accepted but which have not been put into the manifest yet...
## basically just call the filterIssues() function and
## then also compare to what is in the manifest already. Just see the
## code in tallyManifests.R


readyToAdd <- function(datePrefix='2015',svnDir = "~/proj/Rpacks/", getUserFiles=FALSE){
    ## get the accepted issues from this year and their files 
    accepted <- filterIssues(status=c('accepted'),
                             datePrefix=datePrefix,
                             getUserFiles=getUserFiles)
    ## get most recent manifest filename
    maniNames <- .makeManifestNames(svnDir)
    lastMani <- maniNames[length(maniNames)]
    ## And update the manifest file
    system(paste0("svn up ", lastMani))
    ## use scan to read in that file
    res <- scan(lastMani, what="character",skip=1, quiet=TRUE)
    ## extract the names
    idxMani <- rep(c(FALSE,TRUE), length(res)/2)
    pkgNames <- res[idxMani] 
    ## filter the results using that list of names
    idx <- !(accepted$title %in% pkgNames)
    accepted[idx,]
}


##############################################################################
## Optionally: make a function that creates results from
## getUnassignedIDsAndFileNames into an email for potential reviewers?












## This is a file for functions that interact with the issue tracker in some way

## 1st up I want a function that can build a particular tracker issue again. (So that we don't have to bother Dan every time)
## This function requires both unix AND that you have installed stompy
## And you have to know where you put the python code.
## It should probably do some arg checking on it's two arguments too.
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

## TODO: make this (and other functions in here) so that if the user
## has set up their python path in the .Rprofile as an option that
## this will be respected)
## And actually the above won't work unless I 1st wrap it in an ssh command (IOW it won't work except on habu)

## example - test it out on sbptest:
## rebuildIssueTarball(558,'https://tracker.bioconductor.org/file4845/spbtest_0.99.0.tar.gz')



##############################################################################
## And make a function that will use the command line to remove dead issues
## For now lets just remove ONE dead issue (safety)
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



## The command-line interface is called roundup-admin, so when you
## ssh www-data@habu
## you can do
## roundup-admin --help
## For help. Our tracker is at /var/www-trackers/bioc_submit so any
## actual command should start out:
## roundup-admin -i /var/www-trackers/bioc_submit ...
## Where ... is the actual subcommand you want to run.







##############################################################################
##################################################################
## tracker SQL querying functions:



##############################################################################
## Make function that can get the links and DESCRIPTION files from the issue tracker DB for all unassigned issues.

## From habu:
## mysql -p -h habu.fhcrc.org -u roundup roundup_bioc_submit
## contents of the tables
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
## 


## So (for example), I want to ask for an _issue where the _status = 1 to get all unassigned issues.  And I want to also get the _issue 'ids' for those?-YES.  AND: I also want to get the associated file name for these (so get the records from _file as joined with _issue using _creator)

## This is part of it
## select count(*) from _file, _issue where _file._actor=_issue._actor AND _issue._status=1 AND _issue._creation LIKE '2015%';

## Then I need to *not* do a full join like above but to 1st apply the criteria to the issues and *then* join

## This gives the same thing as the inner join above
## SELECT * FROM (SELECT * FROM _issue WHERE _issue._status=1 AND _issue._creation LIKE '2015%') AS issue, _file WHERE _file._actor=issue._actor;

## SELECT * FROM (SELECT * FROM _issue WHERE _issue._status=1 AND _issue._creation LIKE '2015%') AS issue, (SELECT * FROM _file WHERE _file._creation LIKE '2015%') AS file WHERE file._actor=issue._actor;


## SELECT _title,_activity,_creator FROM (SELECT * FROM _issue WHERE _issue._status=1 AND _issue._creation LIKE '2015%') AS issue, (SELECT * FROM _file WHERE _file._creation LIKE '2015%') AS file WHERE file._actor=issue._actor;

## Now it's too strict (getting closer):
## SELECT issue._title,issue.id,file._name,file._activity FROM (SELECT * FROM _issue WHERE _issue._status=1 AND _issue._activity LIKE '2015%') AS issue, (SELECT * FROM _file WHERE _file._creation LIKE '2015%') AS file WHERE file._actor=issue._actor;


## Try again, but this time use 'creator' instead of 'actor' to join (GOOD!)
## SELECT issue._title,issue.id,file._name,file._activity FROM (SELECT * FROM _issue WHERE _issue._status=1 AND _issue._activity LIKE '2015%') AS issue, (SELECT * FROM _file WHERE _file._activity LIKE '2015%') AS file WHERE file._creator=issue._creator;

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


## Ok so this function will just get the basic information about the file names and the Issue IDs for those issues that are still unassigned.
getFilteredIDsAndFileNames <- function(status=c('new-package'), date=2015){
    pswd <- getOption("trackerPSWD")
    statusIds <- unlist(lapply(status,.getStatus))
    fmtStatusIds <- paste0(statusIds, collapse="','")
    if(!exists('pswd')){
        stop("You need to set a password for the issue tracker in .Rprofile")
    }
    require(RMySQL)
    con = dbConnect(dbDriver('MySQL'),
                    host='habu.fhcrc.org',
                    dbname='roundup_bioc_submit',
                    user='roundup',
                    pass=pswd)
    sql <- paste0("SELECT issue._title,issue.id,file._name,file._activity ",
                  "FROM ",
                  "(SELECT * FROM _issue ",
                  "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                  "AND _issue._activity LIKE '",date,"%') ",
                  "AS issue, ",
                  "(SELECT * FROM _file ",
                  "WHERE _file._activity LIKE '",date,"%') ",
                  "AS file ",
                  "WHERE file._creator=issue._creator")
    dbGetQuery(con, sql)
}


## Usage: getFilteredIDsAndFileNames(status=c('new-package','preview-in-progress'), date=2015)





##############################################################################
## Then make a function that will get all records from the tracker that have not been checked in a while.


##############################################################################
## And make a function that will look at records that are accepted but which have not been put into the manifest yet...






##############################################################################
## Optionally: make a function that creates results from
## getUnassignedIDsAndFileNames into an email for potential reviewers?







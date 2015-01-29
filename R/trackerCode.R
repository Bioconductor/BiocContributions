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
filterIssues <- function(status=c('new-package'),
                                           datePrefix='2015',
                                           getUserFiles=FALSE){
    validStatuses <- c('new-package','preview-in-progress','sent-back',
                       'modified-package','review-in-progress','accepted',
                       'rejected')
    match.arg(status, choices=validStatuses, several.ok=TRUE)
    if(!isSingleString(datePrefix)) stop("datePrefix must be single string")
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
    sql1 <- paste0("SELECT issue._title,issue.id,file._name,issue._activity ",
                   "FROM ",
                   "(SELECT * FROM _issue ",
                   "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                   "AND _issue._activity LIKE '",datePrefix,"%') ",
                   "AS issue, ",
                   "(SELECT * FROM _file ",
                   "WHERE _file._activity LIKE '",datePrefix,"%') ",
                   "AS file ",
                   "WHERE file._creator=issue._creator")
    sql2 <- paste0("SELECT _title, id, _activity ",
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
## Then make a function that will get all records from the tracker that have not been checked for a couple of weeks or longer.  (all this is in _issue I think)

## basically  I only want records where within an _issue table there is a difference between the last two _activity values (after being grouped by issue) such that the difference is greater than two weeks.  Then I want to take that list of ids and merge with _user.


## I think this is correct for getting users paired with the package that "they" created.
## SELECT _issue._title,_issue.id,_issue._activity,_user._address,_user._username FROM _issue, _user WHERE _user.id=_issue._creator limit 20;

## IOW you want the 'id' from the table that is the 'subject' paired with the 'action' from the other table.  Here the _user.id matches the issue._creator

## So to make it so that I get the ones where someone was assigned to an issue I do this:
## SELECT _issue._title,_issue.id,_issue._activity,_user._address,_user._username FROM _issue, _user WHERE _user.id=_issue._assignedto limit 20;




## And to quickly see the most recent ones:
## SELECT _issue._title,_issue.id,_issue._activity,_user._address,_user._username FROM _issue, _user WHERE _user.id=_issue._assignedto ORDER BY _activity DESC LIMIT 20;


## So now I just have to replace _issue (in the above) with a query that only keeps those records that are delinquint.  (compare the activity date to the current time)

## Something like this:
## SELECT NOW(),_activity,_title,id FROM _issue LIMIT 3;
## OR even better:
## SELECT DATE(NOW()),DATE(_activity),_activity,_title,id FROM _issue LIMIT 3;
## And even better 
## SELECT DATEDIFF(DATE(NOW()), DATE(_activity)) AS dateDiff,_activity,_title,id,_assignedto FROM _issue LIMIT 3;


## TODO: make a query like the above that pre-filters bases on _status
## SELECT DATEDIFF(DATE(NOW()), DATE(_activity)) AS dateDiff,_activity,_title,id,_assignedto FROM _issue WHERE _issue._status IN ('2','3','4','5') LIMIT 3; 

## So the final query will look something like this:

## SELECT issue.dateDiff,issue._title,issue.id,issue._activity,_user._address,_user._username FROM (SELECT DATEDIFF(DATE(NOW()), DATE(_activity)) AS dateDiff,_activity,_title,id,_assignedto FROM _issue WHERE _issue._status IN ('2','3','4','5')) AS issue, _user WHERE _user.id=issue._assignedto ORDER BY _activity DESC LIMIT 20;


## That gets me most of what I want, but I still need to be able to filter based on the status AND (ideally) I also need to be able to know tho the last person to touch the issue was...

## So two remaining problems:
## 1) prefilter based on the _status like above (internal query) - DONE

## 2) Find out who whether or not the last person to touch the issue was in fact that same person assigned to it... (I *think* this means when the _actor==_assignedto) - pretty sure thats right.  So then just filter out rows like that (be sure to include the _actor field from _issue in the subquery)
## And VERIFY that _actor is the field that is the last person who touched the issue! - VERIFIED  :)











##############################################################################
## And make a function that will look at records that are accepted but which have not been put into the manifest yet...
## basically just call the filterIssues() function and
## then also compare to what is in the manifest already. Just see the
## code in tallyManifests.R





##############################################################################
## Optionally: make a function that creates results from
## getUnassignedIDsAndFileNames into an email for potential reviewers?







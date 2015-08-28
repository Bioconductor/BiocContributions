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
           'rejected'=7,
           'closed'=8,
           'pre-accepted'=9 )
}

.getTextStatus <- function(no) {
    switch(no,
           '1' = 'new-package',
           '2' = 'preview-in-progress',
           '3' = 'sent-back',
           '4' = 'modified-package',
           '5' = 'review-in-progress',
           '6' = 'accepted',
           '7' = 'rejected',
           '8' = 'closed',
           '9' = 'pre-accepted'  )
}


.coreReviewerIds <- function(str){
    c('mcarlson'=19,
      'herve'=7,
      'nhayden'=560,
      'sarora'=559,
      'mtmorgan'=18,
      'vobencha'=209,
      'pshannon'=208,
      'dtenenba'=210,
      'jhester'=759)
}

###############################################################################
## Make function that can get the links and DESCRIPTION files from the issue tracker DB for all unassigned issues.

filterIssues <- function(status=c('new-package'),
                         datePrefix='2015',
                         getUserFiles=FALSE){
    ## argument checking and processing
    validStatuses <- c('new-package','preview-in-progress','sent-back',
                       'modified-package','review-in-progress','accepted',
                       'rejected', 'pre-accepted')
    match.arg(status, choices=validStatuses, several.ok=TRUE)
    if(!isSingleString(datePrefix)) stop("datePrefix must be single string")
    statusIds <- unlist(lapply(status,.getStatus))
    fmtStatusIds <- paste0(statusIds, collapse="','")
    ## DB stuff
    con <- .getRoundupCon()
    sql1 <- paste0("SELECT issue._title AS title ",
                   ",issue.id, file._name AS name, ",
                   "issue._activity AS activity, ",
                   "issue.__retired__ AS retired ",
                   "FROM ",
                   "(SELECT * FROM _issue ",
                   "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                   "AND _issue._activity LIKE '",datePrefix,"%') ",
                   "AS issue, ",
                   "(SELECT * FROM _file ",
                   "WHERE _file._activity LIKE '",datePrefix,"%') ",
                   "AS file ",
                   "WHERE file._creator=issue._creator")
    sql2 <- paste0("SELECT _title AS title, id, _activity AS activity, ",
                   "__retired__ AS retired ",
                   "FROM _issue ",
                   "WHERE _issue._status IN ('",fmtStatusIds,"') ",
                   "AND _issue._activity LIKE '",datePrefix,"%'")

    if(getUserFiles==TRUE){
        res <- dbGetQuery(con, sql1)
    }else{
        res <- dbGetQuery(con, sql2)   
    }
    ## Only keep records where the issue was NOT retired
    res <- res[res$retired==0,]
    res <- res[,!names(res) %in% 'retired']
    res
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

coneOfShame <- function(daysNeglected=14, userName=NULL, daysToForget=30,
                        lastTouchedFilter=TRUE){
    con <- .getRoundupCon()
    sql <- paste0("SELECT issue.dateDiff,",
                  "issue._title AS title,",
                  "issue.id,",
                  "issue._activity AS activity,",
                  "issue._actor AS actor,",
                  "issue._assignedto AS assignedto,",
                  "issue.__retired__ AS retired,",
                  "_user._address AS address,",
                  "_user._username AS username ",
                  "FROM (SELECT DATEDIFF(DATE(NOW()),DATE(_activity)) AS ",
                  "dateDiff,_activity,_actor,_title,id,_assignedto,",
                  "__retired__ ",
                  "FROM _issue WHERE _issue._status IN ('2','3','4','5')) ",
                  "AS issue, _user ",
                  "WHERE _user.id=issue._assignedto ",
                  "ORDER BY activity DESC")
    res <- dbGetQuery(con, sql)
    ## Remove records where a core reviewer was the last person to touch it.
    if(lastTouchedFilter){
        ##idx <- res$actor %in% .coreReviewerIds()
         res <- res[res$actor!=res$assignedto,]
        ##res <- res[!idx,]
    }
    ## Only keep records where the issue was NOT retired
    res <- res[res$retired==0,]
    res <- res[,!names(res) %in% 'retired']
    ## And then filter based on the dateDiff
    res <- res[res$dateDiff > daysNeglected,]
    ## And then filter based on things just being too damned old to matter
    res <- res[res$dateDiff < daysToForget,]
    if(!is.null(userName)){
        res <- res[res[['username']] %in% userName,]
    }
    res
}

## Usage: coneOfShame() ## show who is behind
## Usage: coneOfShame(7, 'mcarlson') ## show which issues I am about to be behind on
## Usage:  coneOfShame(1, 'mcarlson', lastTouchedFilter=FALSE) ## show which issues I have assigned to me that I have not finished yet...

##############################################################################
## And make a function that will look at records that are accepted but which have not been put into the manifest yet...
## basically just call the filterIssues() function and
## then also compare to what is in the manifest already. Just see the
## code in tallyManifests.R

## Helper just gets manifest names based on a path
.getAllPossibleManifestNames<- function(svnDir, software=TRUE){
    ## get most recent manifest filename
    if(software==TRUE){
        manis <- .makeManifestNames(svnDir)
    }else{
        manis <- .makeExpManifestNames(svnDir)
    }
    lastMani <- manis[length(manis)]
    ## And update the manifest file
    system(paste0("svn up ", lastMani))
    ## use scan to read in that file
    res <- scan(lastMani, what="character",skip=1, quiet=TRUE)
    ## extract the names
    idxMani <- rep(c(FALSE,TRUE), length(res)/2)
    res[idxMani] 
}

readyToAdd <- function(datePrefix='2015',
                       svnDir = "~/proj/Rpacks/",
                       svnDir1="~/proj/experiment/pkgs/",
                       getUserFiles=FALSE){
    ## get the accepted issues from this year and their files 
    accepted <- filterIssues(status=c('pre-accepted'),
                             datePrefix=datePrefix,
                             getUserFiles=getUserFiles)
    ## get most recent manifest filenames
    softwareNames <- .getAllPossibleManifestNames(svnDir, software=TRUE)
    experNames <- .getAllPossibleManifestNames(svnDir1, software=FALSE)
    pkgNames <- c(softwareNames, experNames)
    
    ## filter the results using that list of names
    idx <- !(accepted$title %in% pkgNames)
    accepted[idx,]
}


##############################################################################
## Optionally: make a function that creates results from
## getUnassignedIDsAndFileNames into an email for potential reviewers?





##########################################################################
## function to find whose working on what in last 30 days. 


.fullDb <- function() {
    con <- .getRoundupCon()
     sql <- paste0("SELECT issue.dateDiff,",
                  "issue._title AS title,",
                  "issue.id,",
                  "issue._activity AS activity,",
                  "issue._actor AS actor,",
                  "issue._assignedto AS assignedto,",
                  "issue.__retired__ AS retired,",
                  "issue._status AS status, ",
                  "_user._address AS address,",
                  "_user._username AS username ",
                  "FROM (SELECT DATEDIFF(DATE(NOW()),DATE(_activity)) AS ",
                  "dateDiff,_activity,_actor,_title,id,_assignedto,_status,",
                  "__retired__ ",
                  "FROM _issue WHERE _issue._status IN ('1','2','3','4','5','6', '8','9')) ",
                  "AS issue, _user ",
                  "WHERE _user.id=issue._assignedto ",
                  "ORDER BY activity DESC")
    
    dbGetQuery(con, sql)
}


creditworthy <- function(creditDays= 30, userName=NA_character_) {
    df <- .fullDb()
    
    userName <- c( "pshannon", "vobencha", "herve", "nhayden", "dtenenba", 
        "sarora", "mtmorgan", "mcarlson", "jhester")
    core_assignedto <- c( 208, 209, 7, 560, 210, 559, 18)
    dd <- subset(df, df$dateDiff < creditDays)
    dd <- dd[which(dd$username %in% userName),]
    dd$status <- apply(dd, 1, function(x) .getTextStatus(x["status"]))
    drop <- which(names(dd) %in% c("actor", "assignedto", 
        "retired", "address"))
    dd <- dd[, -drop]
    dd <- dd[ order(dd[,'username'], dd[,'status']), ]

    lst <- lapply(userName, function(x){
       tempdf <- subset(dd, dd$username == x)
       touched= nrow(tempdf)
       accepted = length(which(tempdf$status=='accepted'))
       preacc = length(which(tempdf$status=='pre-accepted'))
       preview=  length(which(tempdf$status=='preview-in-progress'))
       review=  length(which(tempdf$status=='review-in-progress'))
       sent= length(which(tempdf$status=='sent-back'))
       modpkg= length(which(tempdf$status=='modified-package'))

       list(username=x, touched=touched, 
           accepted=accepted, preacc=preacc, author=sent, 
           reviewer=as.numeric(modpkg+preview+review))
    })
    df <- as.data.frame(matrix(unlist(lst), nrow=length(userName), byrow=TRUE))
    colnames(df) = c('username', 'touched', 'accepted', 'preacc', 
        'author', 'reviewer')
    print(df)
    split(dd, dd$username)
}


preacceptedToAccepted <- function(){
   df <- .fullDb()
   newdf <- subset(df, status==9)
   allPkgs <- .getPackageContents_txtfile(biocVersion='3.2')
   
   testpkg <- newdf$title
   sa <- lapply(testpkg, function(p) {
        message(p)
        stat <- grep(p, allPkgs, value=TRUE)
        if(length(stat)!=0){
            warn <- grep("WARNINGS", stat)
            warn<- ifelse(length(warn)!=0, "yes", "none")
            error <- grep("ERROR", stat)
            err <- ifelse(length(error)!=0, yes="yes", "none")
        } else{
           warn= 'not added'
	   err = 'not added'
        }      
        c(warning=warn, error=err)
   })
   warn <- sapply(sa, "[[", "warning")
   error <- sapply(sa,"[[", "error")
   drop <- which(names(newdf) %in% c("actor", "assignedto", 
            "retired", "address", "activity", 'status'))
   newdf <- newdf[, -drop]
   cbind(newdf, warn, error, stringsAsFactors=FALSE)

}

# this function queries the issue tracker database 
# and finds which pkgs are not yet assigned. 
.newpkg <- function()
{
   con <- .getRoundupCon()
   sql <- "select * from _issue"
   test <- dbGetQuery(con, sql)
   test <- test[which(test["_status"]==1),]
   date <- as.Date(as.POSIXlt(test["_creation"][[1]], format = "%Y-%m-%d %H:%M:%S", tz="PST"))
   ind <- which(date > Sys.Date() -30)
   test[ind,]
}

weeklyEmailPackagesOverview <- function(daysToForget=30){
   df <- .fullDb()
   dd <- subset(df, df$dateDiff < daysToForget)
   preacc <- subset(df, status==9) # pre-accepted
   preacc_str <- paste(preacc$username, "-", preacc$title, "-", 
      paste0("https://tracker.bioconductor.org/issue", 
      preacc$id))

   accepted <- subset(dd, status==6) #accepted
   acc <- accepted[which(accepted$dateDiff < 7),]
   acc_str <- paste(acc$username, "-", acc$title, "-", 
      paste0("https://tracker.bioconductor.org/issue", 
      acc$id))

   cone <- coneOfShame(daysToForget=daysToForget)
   cone_str <- paste(cone$username, "-", cone$title, "-",
      paste0("https://tracker.bioconductor.org/issue",
      cone$id))

   newpkgs <- .newpkg()
   newpkg_str <- paste(newpkgs[,"_title"], "-", 
      paste0("https://tracker.bioconductor.org/issue", 
      newpkgs$id))

   message("a) Packages added to Bioconductor this week(preaccepted to accepted) ")
   print(acc_str)
   message()

   message("b) Packages pre-accepted this week")
   print(preacc_str) 
   message()

   message("c) laggers - CAUTION - check before you send ")
   message("possible that reviewer has posted and developer has not replied")		
   print(cone_str) 
   message()

   message("d) new packages to be assigned")
   print(newpkg_str)
   message()

   message("e) Core Reviewers Activity in the last 30 days")
   df <- creditworthy()

} 


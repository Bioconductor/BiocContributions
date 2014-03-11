## Here we set things up so that when the package loads we try to have
## a users file that we can access later on

## need a place to put my secret tempFile
stash <- new.env(parent=emptyenv())

## .tryToGetFile <- function(){
##     result <- tryCatch( system(cmd), error=function(err) NULL)
##     ## If we failed to get the file: don't freak out
##     if (is.null(result)){
##         warning(paste0("Unable to get the users File.\n",
##                        "Some functions will not work without it."))
##     }
## }


.onLoad <- function(libname, pkgname)
{
    if(.Platform$OS.type != "unix"){
        warning("Sorry the users file is only available from Unix, if you need to use functions that depend on it they are not going to work from here")}
    usersFile <- getOption("usersFile")
    tempDir <- tempdir()
    cmd <- paste0('rsync ',usersFile,' ',tempDir)
    message(paste0("Just wrote some user data to: ", tempDir))
    
    assign('tempDir', tempDir, envir=stash)
    ## Try to get the file to cwd like this:
    system(cmd)
    ## .tryToGetFile()
}


## onUnload and friends are just not unlinking this file.  So I will
## have to make a tempDir and put it there


## .onUnload <- function(libpath)
## {
##     ## if(file.exists('users')){
##     ##     unlink("users")
##     ## }
##     unlink("users")
## }

## .onDetach <- function(libpath)
## {
##     unlink("users")
## }

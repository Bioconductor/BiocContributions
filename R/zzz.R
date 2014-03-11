## Here we set things up so that when the package loads we try to have
## a users file that we can access later on

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
        warning("Sorry the users file is only available from Unix")}
    usersFile = getOption("usersFile")
    cmd <- paste0('rsync ',usersFile,' .')
    ## Try to get the file to cwd like this:
    system(cmd)
    ## .tryToGetFile()
}

.onUnload <- function(libpath)
{
    if(file.exists('users')){
        unlink("users")
    }
}

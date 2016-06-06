## Here we set things up so that when the package loads we try to have
## a users file that we can access later on

## need a place to put my secret tempFile
stash <- new.env(parent=emptyenv())

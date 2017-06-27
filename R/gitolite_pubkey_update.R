gitolite_pubkey_update <-
    function(gitid, svnid=gitid,
             keydir = "/home/mtmorgan/a/gitolite-admin/keydir")
{
    stopifnot(
        is.character(gitid), length(gitid) == 1L, !is.na(gitid),
        is.character(svnid), length(svnid) == 1L, !is.na(svnid),
        is.character(keydir), length(keydir) == 1L, dir.exists(keydir)
    )

    ## get keys from github
    response <- httr::GET(sprintf("https://github.com/%s.keys", gitid))
    stop_for_status(response)
    keys <- unique(trimws(strsplit(as.character(response), "\n")[[1]]))

    ## find existing keys
    pubfile <- sprintf("%s.pub", svnid)
    pub <- dir(keydir, pattern=pubfile, recursive=TRUE, full=TRUE)
    pub_keydir <- basename(dirname(pub))
    existing <- trimws(vapply(pub, readLines, character(1)))
    keys <- keys[!keys %in% existing]

    possible_keydir <- sprintf("key-%d", 1:100)
    need_keydir <- file.path(
        keydir,
        head(setdiff(possible_keydir, pub_keydir), length(keys))
    )

    ## write new keys
    for (key in seq_along(keys)) {
        if (!dir.exists(need_keydir[[key]]))
            dir.create(need_keydir[[key]])
        writeLines(keys[[key]], file.path(need_keydir[[key]], pubfile))
    }

    length(keys)
}       

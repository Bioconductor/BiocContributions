# README

The canonical location for this code is https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/BiocContributions

You can setup git-svn on it by cloning this repository and running

```bash
git svn init https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/BiocContributions
git svn fetch
git update-ref refs/remotes/git-svn refs/remotes/origin/master
```

Then after committing code locally run the following to commit the changes SVN and push the commits back to GitHub.

```bash
# Get any changes from SVN
git svn rebase

# commit code to svn
git svn dcommit

# push code to github
git push
```

There is no need to mess with any branches in this setup, just do everything on the master branch.


## TODO

[ ] BiocCheck error on invalid/inappropriate files in github repo, eg: .DS_STORE

[ ] BiocCheck requires github key

[ ] (optional) .gitignore template

## Repository after acceptance

[ ] 'bare' clone of developer github repository

[ ] add to git@git.bioconductor.org (e.g. git push)

[ ] add developer public key to keydir

[ ] create unique id/ use github id

[ ] update packages.conf

[ ] update manifest file

[ ] run gitolite-setup 

[ ] close github issue, pointing to developers to documentation

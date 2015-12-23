The canonical location for this code is https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/BiocContributions

You can setup git-svn on it by cloning this repository and runninggit svn init https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/BiocContributions

```bash
git svn fetch
git git update-ref refs/remotes/git-svn refs/remotes/origin/master
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

## Here I aim to formalize my code that currently processes any
## package that does not contain source code into mac and windows
## packages.

## Basically the R version of these three shell loops:
## for pkg in `ls ./fixed/*.tar.gz`
## do
##     echo $pkg
##     /home/mcarlson/RInstalls/R-302/bin/R CMD INSTALL --build $pkg
## #    R CMD INSTALL --build $pkg
## done

## for pkg in `ls ./*.tar.gz`
## do
##     echo $pkg
##     pkgName=${pkg%_R_x86_64-unknown-linux-gnu.tar.gz}
##     cp $pkg ${pkgName}.tgz
##     tar zxf $pkg
##     ##change Built: line in DESCRIPTION to say windows instead of unix
##     sed -i -e "s/unix/windows/g" */DESCRIPTION
## done

## for pkg in `ls ./*.tar.gz` ## lists dirs
## do 
##     pkgName=${pkg%_R_x86_64-unknown-linux-gnu.tar.gz}
##     pkgReal=`echo $pkg | cut -d _ -f 1` 
## #     echo $pkgName
## #     echo $pkgReal
##     rm -f ${pkgReal}/MD5 ## new line
##     zip -r ${pkgName}.zip  $pkgReal
##     rm -rf $pkgReal
##     rm -rf $pkg
## done


CreatePackageAssignmentEmail <- function(assignInTracker=FALSE)
{
    ## Retrieve unassigned packages
    pkgs <- unassigned_packages()
    
    ## Generate code used for assigning packages
    code <- assign_new_packages(pkgs)
    
    ## Generate package assignment email
    email <- package_assignment_email(pkgs, code)
    
    ## Create a new draft email with assignment content (in Gmail drafts)
    gmailr::gmail_auth(scope="compose")
    gmailr::create_draft(email)

    if (assignInTracker) {
        assign_packages(pkgs, code)
        return (invisible(NULL))
    } else
        return (list(pkgs=pkgs, code=code))
}



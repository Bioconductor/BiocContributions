## workflow

## library(jsonlite)
## https://developer.github.com/v3/

library(BiocContributions)

options(
    bioc_contributions_github_user="mtmorgan",
    bioc_contributions_github_auth=readLines("~/.git0Auth"),
    bioc_contributions_manifest_version="3.5", # manifest update
    bioc_contributions_release_version="3.4"   # svn_*_auth_text; trails devel
)
repository <- "https://api.github.com/Bioconductor/Contributions"

types <- github_accept()
github_svn_credentials_request_from_carl(types$Software)
github_svn_credentials_request_from_carl(types$ExperimentData)

github_svn_credentials_draft_to_user(types$Software)
github_svn_credentials_draft_to_user(types$ExperimentData)

svn_software_auth_text(types$Software)
svn_data_experiment_auth_text(types$ExperimentData)


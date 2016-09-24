## workflow

## library(jsonlite)
## https://developer.github.com/v3/

library(BiocContributions)

options(
    bioc_contributions_github_user="mtmorgan",
    bioc_contributions_github_auth="6e55b2fa27337750d63a6eada17c400a1609792a"
)

repository <- "https://api.github.com/Bioconductor/Contributions"

types <- github_accept()
github_svn_credentials_request_from_carl(types$Software)
github_svn_credentials_request_from_carl(types$ExperimentData)

github_svn_credentials_draft_to_user(types$Software)
github_svn_credentials_draft_to_user(types$ExperimentData)

svn_software_auth_text(types$Software)
svn_data_experiment_auth_text(types$ExperimentData)

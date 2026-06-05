# FAQs

## Table of Contents

* [Submission](#submission)
  - [Related Packages/Circular Dependencies](#related-packages-and-circular-dependencies)
* [Commits and Build Reports](#commits-and-build-reports)
  - [No Commit Message](#no-commit-message)
  - [No Posted Report](#no-posted-report)
* [General](#general)
  - [System Issues/Feature Requests](#system-issues-and-feature-requests)

## Submission

#### Related Packages and Circular Dependencies
Related packages should now be submitted as separate issues. The r-universe will
be able to find the related packages on subsequent builds. We may manually
adjust assigned reviewers to have a common reviewer if it is appropriate to
review together. You may also provide a comment on the issue if you would like
to suggest a set of packages should be reviewed together by a single person

## Commits and Build Reports

#### No Commit Message 
There is no confirmation that a successful commit is made to the Bioconductor
version of package. You should check to see the expected commit on the package
repository at https://github.com/orgs/BiocStaging/repositories

#### No Posted Report
There is a delay from when a push happens to when it registers in R-universe.
There is also a delay from when a build/check run completes to when the report
is posted on the issue. It can be a hour or two. If you do not receive a build report
within 24 hours please contact Bioconductor review admins by tagging
@Bioconductor/packagereviewers for assistance after checking that there was a
valid push commit at BiocStaging repository for the package.

## General

#### System Issues and Feature Requests
Please open an issue at [BiocSubmissionProcess](https://github.com/BiocStaging/BiocSubmissionProcess)
